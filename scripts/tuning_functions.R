# Create a random forest wrapper function
tuning_rf <- function(mtry, trees, validation_data){
  
  results <- furrr::future_map2_dfr(.x = validation_data$splits,
                                    .y = validation_data$id,
                                    ~fitting_rf(mtry = mtry,
                                                trees = trees,
                                                split = .x,
                                                id = .y))
  
  results %>%
    group_by(id) %>%
    rmse(truth, prediction) %>%
    summarise(mean_rmse = mean(.estimate),
              sd_rmse = sd(.estimate))
}

tuning_knn <- function(neighbors, validation_data){
  
  results <- furrr::future_map2_dfr(.x = validation_data$splits,
                                    .y = validation_data$id,
                                    ~fitting_knn(neighbors = neighbors,
                                                 split = .x, 
                                                 id = .y))
  
  results %>%
    group_by(id) %>%
    rmse(truth, prediction) %>%
    summarise(mean_rmse = mean(.estimate),
              sd_rmse = sd(.estimate))
}

tuning_btree <- function(trees, tree_depth, learn_rate, validation_data){
  
  results <- map2_dfr(.x = validation_data$splits,
                                    .y = validation_data$id,
                                    ~fitting_btree(trees = trees,
                                                   tree_depth = tree_depth,
                                                   learn_rate = learn_rate,
                                                   split = .x,
                                                   id = .y))
  
  results %>%
    group_by(id) %>%
    rmse(truth, prediction) %>%
    summarise(mean_rmse = mean(.estimate),
              sd_rmse = sd(.estimate))
}

tuning_mars <- function(num_terms, prod_degree, validation_data){
  
  results <- map2_dfr(.x = validation_data$splits,
                      .y = validation_data$id,
                      ~fitting_mars(num_terms,
                                    prod_degree,
                                    split = .x,
                                    id = .y))
  
  results %>%
    group_by(id) %>%
    rmse(truth, prediction) %>%
    summarise(mean_rmse = mean(.estimate),
              sd_rmse = sd(.estimate))
}



