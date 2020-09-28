# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(tidyverse)
library(ISLR)
library(vip)
library(skimr)

# Dados -------------------------------------------------------------------
help(Auto)

# EAD ---------------------------------------------------------------------
glimpse(Auto)
skim(Auto)
# GGally::ggpairs(Auto %>% select(-name))
library(patchwork)

qplot(log(horsepower), log(mpg), data = Auto) / qplot(horsepower, mpg, data = Auto)

Auto %>%
  ggplot(aes(x = log(horsepower))) +
  geom_histogram()

Auto %>%
  select(where(is.numeric), mpg) %>%
  pivot_longer(c(-mpg, -origin)) %>%
  ggplot(aes(x = value, y = mpg, colour = factor(origin))) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~name, scales = "free_x")

set.seed(1)
runif(1)

# base treino e teste -----------------------------------------------------
set.seed(1)
auto_initial_split <- Auto %>% initial_split(3/4)

auto_train <- training(auto_initial_split)
auto_test <- testing(auto_initial_split)

# data prep (ainda vamos falar mais de como usar o recipes!) ------------------------
auto_recipe <- recipe(mpg ~ ., data = auto_train) %>%
  step_rm(name, skip = TRUE) %>%
  step_num2factor(origin, levels = c("American", "European", "Japanese")) %>%
  step_novel(all_nominal()) %>%
  step_log(all_numeric(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())

prep(auto_recipe)
juice(prep(auto_recipe))

# definicao do modelo -----------------------------------------------------
auto_model <- linear_reg(
  penalty = tune(),
  mixture = 1 # LASSO
) %>% 
  set_engine("glmnet")

# workflow ----------------------------------------------------------------
auto_wf <- workflow() %>% 
  add_model(auto_model) %>%
  add_recipe(auto_recipe)

# reamostragem com cross-validation ---------------------------------------
auto_resamples <- vfold_cv(auto_train, v = 5)

# tunagem de hiperparametros ----------------------------------------------
auto_tune_grid <- tune_grid(
  auto_wf, 
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
auto_wf <- auto_wf %>% finalize_workflow(auto_best_hiperparams)

# desempenho do modelo final ----------------------------------------------
auto_last_fit <- auto_wf %>% last_fit(split = auto_initial_split)

collect_metrics(auto_last_fit)
collect_predictions(auto_last_fit) %>%
  ggplot(aes(.pred, mpg)) +
  geom_point()

collect_predictions(auto_last_fit) %>%
  mutate(
    mpg = (mpg),
    .pred = (.pred)
  ) %>%
  rmse(mpg, .pred)

collect_predictions(auto_last_fit) %>%
  ggplot(aes(.pred, ((mpg)-(.pred)))) +
  geom_point() +
  geom_smooth(se = FALSE)

vip(auto_last_fit$.workflow[[1]]$fit$fit)

# modelo final ------------------------------------------------------------
auto_final_model <- auto_wf %>% fit(data = Auto)

# importancia das variaveis -----------------------------------------------
vip::vip(auto_final_model$fit$fit)

vip::vi(auto_final_model$fit$fit) %>%
  mutate(
    abs_importance = abs(Importance),
    Variable = fct_reorder(Variable, abs_importance)
  ) %>%
  ggplot(aes(x = abs_importance, y = Variable, fill = Sign)) +
  geom_col()

# coisas especiais do glmnet e regressão LASSO ----------------------------
auto_final_model$fit$fit$fit %>% plot

# só para fins didáticos
auto_final_model$fit$fit$fit$beta %>%
  as.matrix() %>%
  t() %>%
  as.tibble() %>%
  mutate(
    lambda = auto_final_model$fit$fit$fit$lambda
  ) %>%
  pivot_longer(
    c(-lambda),
    names_to = "variavel",
    values_to = "peso"
  ) %>%
  ggplot(aes(x = lambda, y = peso, colour = variavel)) +
  geom_line(size = 1) +
  geom_vline(xintercept = auto_final_model$fit$fit$spec$args$penalty, colour = "red", linetype = "dashed") +
  scale_x_log10() +
  theme_minimal()

# predicoes ---------------------------------------------------------------
auto_com_previsao <- Auto %>% 
  mutate(
    mpg_pred = predict(auto_final_model, new_data = .)$.pred
  )

# guardar o modelo para usar depois ---------------------------------------
saveRDS(auto_final_model, file = "auto_final_model.rds")


