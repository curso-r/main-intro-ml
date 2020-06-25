# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(tidyverse)
library(ISLR)
library(vip)
library(skimr)

# Dados -------------------------------------------------------------------
help(Auto)

# EAD ---------------------------------------------------------------------
# glimpse(Auto)
# skim(Auto)
# GGally::ggpairs(Auto %>% select(-name))
# qplot(x, mpg, data = Auto)

# data prep (ainda vamos ver como usar o recipes!) ------------------------
auto <- ISLR::Auto %>%
  mutate(
    origin = factor(origin)
  ) %>%
  select(-name)

# base treino e teste -----------------------------------------------------
set.seed(1)
auto_initial_split <- auto %>% initial_split(3/4)

auto_train <- training(auto_initial_split)
auto_test <- testing(auto_initial_split)

# definicao do modelo -----------------------------------------------------
auto_model <- linear_reg(
  penalty = tune(),
  mixture = 1 # LASSO
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# reamostragem com cross-validation ---------------------------------------
auto_resamples <- vfold_cv(auto_train, v = 5)

# tunagem de hiperparametros ----------------------------------------------
auto_tune_grid <- tune_grid(
  auto_model,
  mpg ~ ., 
  resamples = auto_resamples,
  grid = 100,
  metrics = metric_set(rmse),
  control = control_grid(verbose = TRUE, allow_par = FALSE)
)

# inspecao da tunagem -----------------------------------------------------
autoplot(auto_tune_grid)
collect_metrics(auto_tune_grid)
show_best(auto_tune_grid, "rmse")

# seleciona o melhor conjunto de hiperparametros
auto_best_hiperparams <- select_best(auto_tune_grid, "rmse")
auto_model <- auto_model %>% finalize_model(auto_best_hiperparams)

# desempenho do modelo final ----------------------------------------------
auto_last_fit <- auto_model %>% last_fit(mpg ~ ., split = auto_initial_split)

collect_metrics(auto_last_fit)
collect_predictions(auto_last_fit) %>%
  ggplot(aes(.pred, mpg)) +
  geom_point()

collect_predictions(auto_last_fit) %>%
  ggplot(aes(.pred, mpg-.pred)) +
  geom_point()
# auto_last_fit_model <- auto_last_fit$.workflow[[1]]
# vip(auto_last_fit_model)

# modelo final ------------------------------------------------------------
auto_final_model <- auto_model %>% fit(mpg ~ ., auto)

# importancia das variaveis -----------------------------------------------
vip::vip(auto_final_model)

vip::vi(auto_final_model) %>%
  mutate(
    abs_importance = abs(Importance),
    Variable = fct_reorder(Variable, abs_importance)
  ) %>%
  ggplot(aes(x = abs_importance, y = Variable, fill = Sign)) +
  geom_col()

# coisas especiais do glmnet e regressão LASSO ----------------------------
auto_final_model$fit %>% plot

# só para fins didáticos
auto_final_model$fit$beta %>%
  as.matrix() %>%
  t() %>%
  as.tibble() %>%
  mutate(
    lambda = auto_final_model$fit$lambda
  ) %>%
  pivot_longer(
    c(-lambda),
    names_to = "variavel",
    values_to = "peso"
  ) %>%
  ggplot(aes(x = lambda, y = peso, colour = variavel)) +
  geom_line(size = 1) +
  geom_vline(xintercept = auto_final_model$spec$args$penalty, colour = "red", linetype = "dashed") +
  scale_x_log10() +
  theme_minimal()

# predicoes ---------------------------------------------------------------
auto_com_previsao <- auto %>% 
  mutate(
    mpg_pred = predict(auto_final_model, new_data = .)$.pred
  )

# guardar o modelo para usar depois ---------------------------------------
saveRDS(auto_final_model, file = "auto_final_model.rds")


