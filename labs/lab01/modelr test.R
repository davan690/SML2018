rm(list=ls())

library(tidyverse)
library(modelr)

lm_cv_folds <- function(folds) {
  map(folds$train, ~ lm(Sepal.Length ~ ., data = .))
}

# calc_test_rmse <- function(model, folds) {
#   pred_list <- map2(.x = fit, .y = folds$test, ~ predict(.x, .y))
#   resid_list <- map2(pred_list, folds$test,
#                      ~ .x - as.data.frame(.y)$Sepal.Length)
#   rmse_list <- map(resid_list, ~ sqrt(mean(.x^2, na.rm = TRUE)))
#   rmse_list
# }

calc_test_rmse <- function(model, folds) {
  rmse_list <- map2(model, folds$test, rmse)
  
  mean(unlist(rmse_list))
}

iris_ndf <- iris %>% 
  group_by(Species) %>% 
  nest() %>% 
  mutate(folds = map(data, crossv_kfold),
         lm_fit_cv = map(folds, lm_cv_folds),
         lm_test_rmse = map2(lm_fit_cv, folds, calc_test_rmse)) %>% 
  unnest(test_rmse)

