library(here)
library(furrr)
library(tidyverse)
library(tidymodels)

source(here("scripts", "tuning_functions.R"))
source(here("scripts", "fitting_functions.R"))

mex_data <- readRDS(file = here("data", "mexican_purse_seines.rds")) %>% 
  select(-c(landings, ssvid, shipname, commodity, value, category, date)) %>% 
  filter(year > 2014)


# testing and training datasets
## Split data
set.seed(43)
tuna_split <- initial_split(data = mex_data, prop = 0.6, strata = month)
## Extract each split
### Training
tuna_train <- training(tuna_split)
### Testing
tuna_test <- testing(tuna_split)

# Recipe
## Create a recipe based on the training data
tuna_recipe <- function(dataset) {
  recipe(catches ~ ., data = dataset) %>% 
    step_center(all_numeric(), -matches("year"), -matches("month")) %>%
    step_scale(all_numeric(), -matches("year"), -matches("month"))
}


# Cross validation for parameter tunning
## Create a cross_validation splits
set.seed(43)
validation_data <- vfold_cv(tuna_train, v = 5, repeats = 1)

# Set paralel processing when needed
ncores <- parallel::detectCores() - 1
plan(multiprocess)

## RANDOM FOREST
## CV
results_rf <- expand.grid(mtry = c(5:18),
            trees = c(100, 200, 500, 1000)) %>% 
  mutate(res = future_map2(.x = mtry,
                           .y = trees,
                           .f = tuning_rf,
                           validation_data = validation_data)) %>% 
  unnest()

## Visualize
ggplot(results_rf, aes(x = mtry, y = mean_rmse, group = trees, color = trees)) +
  geom_point() +
  geom_line()

## KNN
### CV
results_knn <- tibble(neighbors = 1:20) %>% 
  mutate(res = future_map(.x = neighbors,
                          .f = tuning_knn,
                          validation_data = validation_data)) %>% 
  unnest()

### Visualize
ggplot(results_knn, aes(x = neighbors, y = mean_rmse)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_rmse - sd_rmse, ymax = mean_rmse + sd_rmse))


## Boosted trees
### CV
results_bt <- expand.grid(trees = c(500, 1000, 5000, 10000),
                          tree_depth = c(15, 20, 45),
                          learn_rate = c(0.3, 0.5)) %>% 
  mutate(res = pmap(.l = list(trees, tree_depth, learn_rate),
                           .f = tuning_btree,
                           validation_data = validation_data)) %>% 
  unnest()

## MARS
### CV
results_mars <- expand.grid(num_terms = c(1:18),
                       prod_degree = c(1, 2)) %>% 
  mutate(res = future_map2(.x = num_terms,
                           .y = prod_degree,
                           .f = tuning_mars,
                           validation_data = validation_data)) %>% 
  unnest()

best_pars <- function(cv_results) {
  cv_results %>% 
    arrange(mean_rmse) %>% 
    head(5)
}

# Fit full models after cv
## First, create the training dataset
### Prep the recipe
training_rec <- prep(tuna_recipe(tuna_train))
### Bake the recipe
train_data <- bake(training_rec, new_data = tuna_train)

## Train the model
full_rf_model <- rand_forest(mtry = 6, trees = 200, mode = "regression") %>%
  set_engine("ranger", importance = 'impurity') %>%
  fit(catches ~ ., data = train_data)

full_knn_model <- nearest_neighbor(mode = "regression", neighbors = 6, dist_power = 2) %>% 
  set_engine("kknn") %>% 
  fit(catches ~ ., data = train_data)

full_btree_model <- boost_tree(mode = "regression", trees = 500, tree_depth = 15, learn_rate = 0.3) %>% 
  set_engine("xgboost") %>% 
  fit(catches ~ ., data = train_data)

full_lm_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(catches ~ ., data = train_data)

full_mars_model <- mars(mode = "regression", num_terms = 5, prod_degree = 1) %>% 
  set_engine("earth") %>% 
  fit(catches ~ ., data = train_data)
  
## Evaluate performance
metrics_wrap <- function(model_object, new_data) {
 model_object %>%
    predict(new_data) %>%
    bind_cols(new_data) %>% 
    metrics(truth = catches, estimate = .pred) %>%
    select(-.estimator)
}

# Calculate parameters for the testing set
test_rec <- tuna_test %>% 
  prep(tuna_recipe(.), testing = .)

# Apply parameters to obtain a final testing set
data_test <- bake(test_rec, new_data = tuna_test)

tibble(model = c("rf", "knn", "btree", "mars", "lm"),
       model_object = list(full_rf_model,
                           full_knn_model,
                           full_btree_model,
                           full_mars_model,
                           full_lm_model)) %>% 
  mutate(res = map(model_object, metrics_wrap, new_data = data_test)) %>% 
  unnest(res) %>% 
  ggplot(aes(x = model, y = .estimate)) +
  geom_col() +
  facet_wrap(~.metric, scales = "free_y") +
  startR::ggtheme_plot()

data_test %>% 
  mutate(rf_prediction = predict(full_rf_model, .)$.pred,
         knn_prediction = predict(full_knn_model, .)$.pred,
         btree_prediction = predict(full_btree_model, .)$.pred,
         mars_prediction = predict(full_mars_model, .)$.pred,
         lm_prediction = predict(full_lm_model, .)$.pred) %>% 
  select(catches, rf_prediction, knn_prediction, btree_prediction, mars_prediction, lm_prediction, year) %>% 
  gather(model, prediction, -c(catches, year)) %>% 
  ggplot(aes(x = catches, y = prediction)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~model) +
  coord_equal() +
  startR::ggtheme_plot()








