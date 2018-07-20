rm(list=ls())

library(tidyverse)
library(modelr)
library(randomForest)

lm_cv_folds <- function(folds) {
  map(folds$train, ~ lm(Sepal.Length ~ ., data = .x))
}

rf_cv_folds <- function(folds) {
  map(folds$train, ~ randomForest(Sepal.Length ~ ., data = .x))
}

predict_test <- function(model, folds) {
  map2(model, folds$test, predict)
}

rmse_test <- function(model, folds) {
  rmse_list <- map2(model, folds$test, rmse)
  
  mean(unlist(rmse_list))
}

iris_ndf <- iris %>% 
  group_by(Species) %>% 
  nest() %>% 
  mutate(folds = map(data, crossv_kfold),
         lm_fit_cv = map(folds, lm_cv_folds),
         rf_fit_cv = map(folds, rf_cv_folds),
         lm_pred = map2(lm_fit_cv, folds, predict_test),
         rf_pred = map2(rf_fit_cv, folds, predict_test),
         lm_rmse_test = map2(lm_fit_cv, folds, rmse_test),
         rf_rmse_test = map2(rf_fit_cv, folds, rmse_test)) %>% 
  unnest(lm_rmse_test, rf_rmse_test)


iris_ndf %>% 
  select(Species, LM = lm_rmse_test, RF = rf_rmse_test) %>% 
  gather(Model, RMSE, -Species) %>% 
  ggplot(aes(x = Model, y = RMSE, fill = Model, colour = Model)) +
  geom_violin(alpha = 0.5, colour = NA) +
  geom_jitter() +
  # geom_point() +
  geom_text(aes(label = Species))


# Plot prediction - need to train model off all data first.





#### Old ======================================================================

# calc_test_rmse <- function(model, folds) {
#   pred_list <- map2(.x = fit, .y = folds$test, ~ predict(.x, .y))
#   resid_list <- map2(pred_list, folds$test,
#                      ~ .x - as.data.frame(.y)$Sepal.Length)
#   rmse_list <- map(resid_list, ~ sqrt(mean(.x^2, na.rm = TRUE)))
#   rmse_list
# }