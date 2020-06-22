# Pacotes ------------------------------------------------------------------

library(ggplot2)
library(tidymodels)
library(tidyverse)
library(vip)

# Dados -------------------------------------------------------------------
data("diamonds")

# base treino e teste -----------------------------------------------------
set.seed(1)
diamonds_initial_split <- diamonds %>% initial_split(8/10)

diamonds_train <- training(diamonds_initial_split)
diamonds_test <- testing(diamonds_initial_split)

# definicao do modelo -----------------------------------------------------
# Defina um modelo de regressão linear usando glmnet e 
# prepare para tunar o hiperparâmetro penalty. 
# Deixe o mixture fixo em 1.
# use as funções decision_tree(), tune(), set_engine() e set_mode().
diamonds_lr_model <- linear_reg(
  penalty = tune(),
  mixture = 1
) %>% 
  set_engine("glmnet")

# reamostragem com cross-validation ---------------------------------------
# 5 conjuntos de cross-validation
diamonds_resamples <- vfold_cv(diamonds_train, v = 5)

# tunagem de hiperparametros ----------------------------------------------
# tunagem do hiperparametro usando somente a métrica rmse com grid de tamanho 100.
# OBS: a variável resposta é 'price' e as variáveis explicativas são todas as demais.
diamonds_tune_grid <- tune_grid(
  diamonds_lr_model,
  price ~ .,
  resamples = diamonds_resamples,
  grid = 100,
  metrics = metric_set(rmse),
  control = control_grid(verbose = TRUE, allow_par = FALSE)
)

# inspecao da tunagem -----------------------------------------------------
# autoplot(diamonds_tune_grid)
# collect_metrics(diamonds_tune_grid)
# show_best(diamonds_tune_grid)

# seleciona o melhor conjunto de hiperparametros --------------------------
diamonds_best_hiperparams <- select_best(diamonds_tune_grid, "rmse")

# finalizacao do modelo ---------------------------------------------------
diamonds_lr_model <- diamonds_lr_model %>% finalize_model(diamonds_best_hiperparams)

# desempenho do modelo final ----------------------------------------------
diamonds_last_fit <- diamonds_lr_model %>% last_fit(price ~ ., split = diamonds_initial_split)

# modelo final ------------------------------------------------------------
diamonds_final_lr_model <- diamonds_lr_model %>% fit(price ~ ., diamonds)

# importancia das variaveis -----------------------------------------------
# Extraia as importancias das variaveis do modelo final com vip().
vip::vip(diamonds_final_lr_model)


# guardar o modelo para usar depois ---------------------------------------
# saveRDS(diamonds_final_lr_model, file = "diamonds_final_lr_model.rds")

# 3. [desafio] Ajuste uma árvore de decisão, agora com todas as variáveis, 
# e compare:
# (a) se as variáveis mais importantes são as mesmas.
# (b) se o desempenho da árvore supera o do LASSO em termos de RMSE.
# Dica: reutilize os códigos anteriores! (03-cross-validation.R por exemplo)

