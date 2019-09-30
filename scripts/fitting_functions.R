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

fitting_knn <- function(neighbors, split, id){
  
  analysis_set <- analysis(split)
  
  analysis_prep <- prep(tuna_recipe(analysis_set), training = analysis_set)
  
  analysis_processed <- bake(analysis_prep, new_data = analysis_set)
  
  model <- nearest_neighbor(mode = "regression", neighbors = neighbors, dist_power = 2) %>% 
    set_engine("kknn") %>% 
    fit(catches ~ ., data = analysis_processed)
  
  assessment_set <- assessment(split)
  
  assessment_prep <- prep(tuna_recipe(assessment_set), testing = assessment_set)
  
  assessment_processed <- bake(assessment_prep, new_data = assessment_set)
  
  
  tibble::tibble("id" = id,
                 "truth" = assessment_processed$catches,
                 "prediction" = unlist(predict(model, new_data = assessment_processed)))
}


fitting_btree <- function(trees, tree_depth, learn_rate, split, id){
  
  analysis_set <- analysis(split)
  
  analysis_prep <- prep(tuna_recipe(analysis_set), training = analysis_set)
  
  analysis_processed <- bake(analysis_prep, new_data = analysis_set)
  
  model <- boost_tree(mode = "regression",
                      trees = trees,
                      tree_depth = tree_depth,
                      learn_rate = learn_rate) %>% 
    set_engine("xgboost", nthread = 1, silent = 0) %>% 
    fit(catches ~ ., data = analysis_processed)
  
  assessment_set <- assessment(split)
  
  assessment_prep <- prep(tuna_recipe(assessment_set), testing = assessment_set)
  
  assessment_processed <- bake(assessment_prep, new_data = assessment_set)
  
  
  tibble::tibble("id" = id,
                 "truth" = assessment_processed$catches,
                 "prediction" = unlist(predict(model, new_data = assessment_processed)))
}

fitting_mars <- function(num_terms, prod_degree, split, id) {
  
  analysis_set <- analysis(split)
  
  analysis_prep <- prep(tuna_recipe(analysis_set), training = analysis_set)
  
  analysis_processed <- bake(analysis_prep, new_data = analysis_set)
  
  model <- mars(mode = "regression",
                num_terms = num_terms,
                prod_degree = prod_degree) %>% 
    set_engine("earth") %>% 
    fit(catches ~ ., data = analysis_processed)
  
  assessment_set <- assessment(split)
  
  assessment_prep <- prep(tuna_recipe(assessment_set), testing = assessment_set)
  
  assessment_processed <- bake(assessment_prep, new_data = assessment_set)
  
  
  tibble::tibble("id" = id,
                 "truth" = assessment_processed$catches,
                 "prediction" = unlist(predict(model, new_data = assessment_processed)))
}

