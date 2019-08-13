library(here)
library(tidyverse)
library(tidymodels)

mex_data <- readRDS(file = here("data", "mexican_purse_seines.rds")) %>% 
  select(-c(landings, ssvid, shipname, species, commodity, value, category, date, price)) %>% 
  drop_na()

# testing and training datasets
## Split data
set.seed(43)
tuna_split <- initial_split(data = mex_data, prop = 0.6)
## Extract each split
### Training
tuna_train <- training(tuna_split)
### Testing
tuna_test <- testing(tuna_split)

# Cross validation
## Create a cross_validation schedule
set.seed(43)
validation_data <- vfold_cv(tuna_train, v = 5, repeats = 1)

# Recipe
## Create a recipe based on the training data
tuna_recipe <- function(dataset) {
  recipe(catches ~ ., data = dataset) %>% 
    step_center(all_numeric(), -all_outcomes(), -matches("year"), -matches("month")) %>%
    step_scale(all_numeric(), -all_outcomes(), -matches("year"), -matches("month"))
}

# Calculate parameters for the testing set
test_rec <- tuna_test %>% 
  prep(tuna_recipe(.), testing = .)

# Apply parameters to obtain a final testing set
data_test <- bake(test_rec, new_data = tuna_test)


# Create a random forest wrapper function
fitting_rf <- function(mtry, trees, split, id){
  
  analysis_set <- analysis(split)
  
  analysis_prep <- prep(tuna_recipe(analysis_set), training = analysis_set)
  
  analysis_processed <- bake(analysis_prep, new_data = analysis_set)
  
  model <- rand_forest(mtry = mtry, trees = trees, mode = "regression") %>%
    set_engine("ranger", importance = 'impurity') %>%
    fit(catches ~ ., data = analysis_processed)
  
  assessment_set <- assessment(split)
  
  assessment_prep <- prep(tuna_recipe(assessment_set), testing = assessment_set)
  
  assessment_processed <- bake(assessment_prep, new_data = assessment_set)
  

  tibble::tibble("id" = id,
                 "truth" = assessment_processed$catches,
                 "prediction" = unlist(predict(model, new_data = assessment_processed)))
}

tuning_rf <- function(mtry, trees, validation_data){
  
  results <- purrr::map2_df(.x = validation_data$splits,
                            .y = validation_data$id,
                            ~fitting_rf(mtry = mtry, trees = trees, split = .x, id = .y))
  
  results %>%
    group_by(id) %>%
    rmse(truth, prediction) %>%
    summarise(mean_rmse = mean(.estimate)) %>%
    pull
}

results <- expand.grid(mtry = c(6:10),
            trees = c(100, 150, 200)) %>% 
  mutate(res = future_map2_dbl(.x = mtry, .y = trees, .f = tuning_rf, validation_data = validation_data))




# Fit full model after cv
## First, create the training dataset
### Prep the recipe
training_rec <- prep(tuna_recipe(tuna_train))
### Bake the recipe
train_data <- bake(training_rec, new_data = tuna_train)

## Train the model
full_model <- rand_forest(mtry = 7, trees = 200, mode = "regression") %>%
  set_engine("ranger", importance = 'impurity') %>%
  fit(catches ~ ., data = train_data)

## Evaluate performance
full_model %>%
  predict(data_test) %>%
  bind_cols(data_test) %>% 
  metrics(truth = catches, estimate = .pred)

data_test %>% 
  mutate(prediction = predict(full_model, .)$.pred) %>% 
  ggplot(aes(x = catches, y = prediction)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)








