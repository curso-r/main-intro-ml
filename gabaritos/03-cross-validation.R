# Pacotes ------------------------------------------------------------------

library(ggplot2)
library(tidymodels)

# Dados -------------------------------------------------------------------
data("diamonds")

# base treino e teste -----------------------------------------------------
# crie as tabelas de treino e teste com proporções de 80% e 20% respectivamente.
# use initial_split(), training() e testing()
set.seed(1)
diamonds_initial_split <- diamonds %>% initial_split(8/10)

diamonds_train <- training(diamonds_initial_split)
diamonds_test <- testing(diamonds_initial_split)

# definicao do modelo -----------------------------------------------------
# Defina um modelo de árvore de decisão para regressão usando rpart e 
# prepare para tunar os hiperparâmetros cost_complexity e min_n. 
# Deixe o tree_depth fixo em 10.
# use as funções decision_tree(), tune(), set_engine() e set_mode().
diamonds_model <- decision_tree(
  cost_complexity = tune(),
  min_n = tune(), 
  tree_depth = 10
) %>% 
  set_engine("rpart") %>%
  set_mode("regression")

# reamostragem com cross-validation ---------------------------------------
# Crie 5 conjuntos de cross-validation
# use vfold_cv().
diamonds_resamples <- vfold_cv(diamonds_train, v = 5)

# tunagem de hiperparametros ----------------------------------------------
# tune os hiperparâmetros usando somente a métrica rmse. Faça um grid de tamanho 5.
# OBS: a variável resposta é 'price' e a variável explicativa é 'x'.
# use tune_grid().
diamonds_tune_grid <- tune_grid(
  diamonds_model,
  price ~ x, 
  resamples = diamonds_resamples,
  grid = 5,
  metrics = metric_set(rmse, rsq),
  control = control_grid(verbose = TRUE, allow_par = FALSE)
)

# inspecao da tunagem -----------------------------------------------------
# autoplot(...)
# collect_metrics(...)
# show_best(...)

# seleciona o melhor conjunto de hiperparametros --------------------------
# extraia o conjunto de hiperparametros que apresentou o melhor rmse de 
# cross-validation.
# use select_best().
diamonds_best_hiperparams <- select_best(diamonds_tune_grid, "rmse")

# finalizacao do modelo ---------------------------------------------------
# atualize a especificação do modelo com os hiperparametros encontrados na
# secao acima.
# use finalize_model().
diamonds_model <- diamonds_model %>% finalize_model(diamonds_best_hiperparams)

# desempenho do modelo final ----------------------------------------------
# rode o ultimo ajuste para calcular o rmse na base de teste.
# use last_fit().
diamonds_last_fit <- diamonds_model %>% last_fit(price ~ x, split = diamonds_initial_split)

# collect_metrics(...)
# collect_predictions(...) %>%
#   ggplot(aes(.pred, price)) +
#   geom_point()

# modelo final ------------------------------------------------------------
# ajuste o modelo final com a base inteira (teste + treino).
# use fit().
diamonds_final_model <- diamonds_model %>% fit(price ~ x, diamonds)

# predicoes ---------------------------------------------------------------
# coloque uma coluna de predicoes na base original.
# use mutate(), predict()$.pred
diamonds_com_previsao <- diamonds %>% 
  mutate(
    price_pred = predict(diamonds_final_model, new_data = .)$.pred
  )

# rpart.plot::rpart.plot(...$fit)

# guardar o modelo para usar depois ---------------------------------------
# saveRDS(..., file = "diamonds_final_model.rds")

