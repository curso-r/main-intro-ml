# Pacotes ------------------------------------------------------------------

library(ggplot2)
library(tidymodels)
library(tidyverse)
library(vip)

# Dados -------------------------------------------------------------------
data("diamonds")

# EAD ---------------------------------------------------------------------
# glimpse(diamonds)
# skim(diamonds)
# GGally::ggpairs(diamonds)
# qplot(x, price, data = diamonds)

# base treino e teste -----------------------------------------------------
set.seed(1)
diamonds_initial_split <- diamonds %>% initial_split(8/10)

diamonds_train <- training(diamonds_initial_split)
diamonds_test <- testing(diamonds_initial_split)

# data prep --------------------------------------------------
# [opcional] Experimente criar step_log(), step_interact(), etc
# para ver se melhora o modelo.
diamonds_recipe <- recipe(price ~ ., data = diamonds_train) %>%
  step_novel(all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) 

# prep(diamonds_recipe)
# bake(prep(diamonds_recipe), new_data = NULL)

# definicao do modelo -----------------------------------------------------
# Defina um modelo de regressão linear usando glmnet e 
# prepare para tunar o hiperparâmetro penalty. 
# Deixe o mixture fixo em 1.
# use as funções decision_tree(), tune(), set_engine() e set_mode().

##########################
# PREENCHA AQUI
##########################

# workflow ----------------------------------------------------------------
# Defina o workflow com workflow(), add_model() e add_recipe().

##########################
# PREENCHA AQUI
##########################

# reamostragem com cross-validation ---------------------------------------
# 5 conjuntos de cross-validation
diamonds_resamples <- vfold_cv(diamonds_train, v = 5)

# tunagem de hiperparametros ----------------------------------------------
# tunagem do hiperparametro usando somente a métrica rmse com grid de tamanho 100.
diamonds_tune_grid <- tune_grid(
  
  ##########################
  # PREENCHA AQUI
  ##########################
  # coloque o workflow aqui
  resamples = diamonds_resamples,
  grid = 20,
  metrics = metric_set(rmse),
  control = control_grid(verbose = TRUE, allow_par = FALSE)
)

# inspecao da tunagem -----------------------------------------------------
autoplot(diamonds_tune_grid)
collect_metrics(diamonds_tune_grid)
show_best(diamonds_tune_grid, "rmse")

# seleciona o melhor conjunto de hiperparametros
diamonds_best_hiperparams <- select_best(diamonds_tune_grid, "rmse")

# atualize o workflow com finalize_workflow()
##########################
# PREENCHA AQUI
##########################

# desempenho do modelo final ----------------------------------------------

# Faça o last_fit()

##########################
# PREENCHA AQUI
##########################
diamonds_last_fit <- "preencha aqui..."
  
  
  
collect_metrics(diamonds_last_fit)
collect_predictions(diamonds_last_fit) %>%
  ggplot(aes(.pred, price)) +
  geom_point()

collect_predictions(diamonds_last_fit) %>%
  mutate(
    price = (price),
    .pred = (.pred)
  ) %>%
  rmse(price, .pred)

collect_predictions(diamonds_last_fit) %>%
  ggplot(aes(.pred, ((price)-(.pred)))) +
  geom_point() +
  geom_smooth(se = FALSE)

vip(diamonds_last_fit$.workflow[[1]]$fit$fit)

# modelo final ------------------------------------------------------------
# Aplique o fit() no workflow usando todos os dados para o modelo final
##########################
# PREENCHA AQUI
##########################
diamonds_final_model <- "preencha aqui...."

# importancia das variaveis -----------------------------------------------
vip::vip(diamonds_final_model$fit$fit)

vip::vi(diamonds_final_model$fit$fit) %>%
  mutate(
    abs_importance = abs(Importance),
    Variable = fct_reorder(Variable, abs_importance)
  ) %>%
  ggplot(aes(x = abs_importance, y = Variable, fill = Sign)) +
  geom_col()

# coisas especiais do glmnet e regressão LASSO ----------------------------
diamonds_final_model$fit$fit$fit %>% plot

# só para fins didáticos
diamonds_final_model$fit$fit$fit$beta %>%
  as.matrix() %>%
  t() %>%
  as.tibble() %>%
  mutate(
    lambda = diamonds_final_model$fit$fit$fit$lambda
  ) %>%
  pivot_longer(
    c(-lambda),
    names_to = "variavel",
    values_to = "peso"
  ) %>%
  ggplot(aes(x = lambda, y = peso, colour = variavel)) +
  geom_line(size = 1) +
  geom_vline(xintercept = exp(diamonds_final_model$fit$fit$spec$args$penalty), colour = "red", linetype = "dashed") +
  scale_x_log10() +
  theme_minimal()

# predicoes ---------------------------------------------------------------
diamonds_com_previsao <- diamonds %>% 
  mutate(
    mpg_pred = predict(diamonds_final_model, new_data = .)$.pred
  )

# guardar o modelo para usar depois ---------------------------------------
saveRDS(diamonds_final_model, file = "diamonds_final_model.rds")


# guardar o modelo para usar depois ---------------------------------------
# saveRDS(diamonds_final_lr_model, file = "diamonds_final_lr_model.rds")





# 3. [desafio] Ajuste uma árvore de decisão, agora com todas as variáveis, 
# e compare:
# (a) se as variáveis mais importantes são as mesmas.
# (b) se o desempenho da árvore supera o do LASSO em termos de RMSE.
# Dica: Siga as mesmas etapas, apenas mudando onde necessário.

