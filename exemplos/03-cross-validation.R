# Pacotes ------------------------------------------------------------------

library(ggplot2)
library(tidymodels)

# Dados -------------------------------------------------------------------
data("diamonds")

# base treino e teste -----------------------------------------------------
set.seed(1)
diamonds_initial_split <- diamonds %>% initial_split(3/4)

diamonds_train <- training(diamonds_initial_split)
diamonds_test <- testing(diamonds_initial_split)

# definicao do modelo -----------------------------------------------------
# OBS: repare que agora colocamos "tune()" nos hiperparâmetros para os quais 
# queremos encontrar o melhor valor.
diamonds_model <- decision_tree(
  cost_complexity = tune(),
  min_n = 2, 
  tree_depth = 10
) %>% 
  set_engine("rpart") %>%
  set_mode("regression")

# reamostragem com cross-validation ---------------------------------------
diamonds_resamples <- vfold_cv(diamonds_train, v = 5)

# tunagem de hiperparametros ----------------------------------------------
diamonds_tune_grid <- tune_grid(
  price ~ x, 
  diamonds_model,
  resamples = diamonds_resamples,
  grid = 10,
  metrics = metric_set(rmse, rsq),
  control = control_grid(verbose = TRUE, allow_par = FALSE)
)

# inspecao da tunagem -----------------------------------------------------
autoplot(diamonds_tune_grid)
collect_metrics(diamonds_tune_grid)
show_best(diamonds_tune_grid)

# seleciona o melhor conjunto de hiperparametros
diamonds_best_hiperparams <- select_best(diamonds_tune_grid, "rmse")
diamonds_model <- diamonds_model %>% finalize_model(diamonds_best_hiperparams)
  
# desempenho do modelo final ----------------------------------------------
diamonds_last_fit <- diamonds_model %>% last_fit(price ~ x, split = diamonds_initial_split)

collect_metrics(diamonds_last_fit)
collect_predictions(diamonds_last_fit) %>%
  ggplot(aes(.pred, price)) +
  geom_point()

# modelo final ------------------------------------------------------------
diamonds_final_model <- diamonds_model %>% fit(price ~ x, diamonds)

# rpart.plot::rpart.plot(diamonds_final_model$fit)

# predicoes ---------------------------------------------------------------
diamonds_com_previsao <- diamonds %>% 
  mutate(
    price_pred = predict(diamonds_final_model, new_data = .)$.pred
  )

# guardar o modelo para usar depois ---------------------------------------
saveRDS(diamonds_final_model, file = "diamonds_final_model.rds")

