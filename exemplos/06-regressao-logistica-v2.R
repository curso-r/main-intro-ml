# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(ISLR)
library(tidyverse)
library(modeldata)
library(pROC)
library(vip)

# PASSO 0) CARREGAR AS BASES -----------------------------------------------
data("credit_data")
help(credit_data)
credit_data <- credit_data %>% na.omit()
glimpse(credit_data) # German Risk

credit_data %>% count(Status)

# PASSO 1) BASE TREINO/TESTE -----------------------------------------------
set.seed(1)
credit_initial_split <- initial_split(credit_data, strata = "Status", p = 0.75)

credit_train <- training(credit_initial_split)
credit_test  <- testing(credit_initial_split)

# PASSO 2) EXPLORAR A BASE -------------------------------------------------

## veremos mais pra frente!

skimr::skim(credit_train)

# PASSO 3) DATAPREP --------------------------------------------------------

## veremos mais pra frente!

credit_recipe <- recipe(Status ~ ., data = credit_train) %>%
  step_normalize(all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) 

bake(prep(credit_recipe), new_data = NULL)

# PASSO 4) MODELO ----------------------------------------------------------
# Definição de 
# a) a f(x): logistc_reg()
# b) modo (natureza da var resp): classification
# c) hiperparametros que queremos tunar: penalty = tune()
# d) hiperparametros que não queremos tunar: mixture = 1 # LASSO
# e) o motor que queremos usar: glmnet
credit_lr_model <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

credit_wf <- workflow() %>% add_model(credit_lr_model) %>% add_recipe(credit_recipe)

# PASSO 5) TUNAGEM DE HIPERPARÂMETROS --------------------------------------
# a) bases de reamostragem para validação: vfold_cv()
# b) (opcional) grade de parâmetros: parameters() %>% update() %>% grid_regular()
# c) tune_grid(y ~ x + ...)
# d) escolha das métricas (rmse, roc_auc, etc)
# d) collect_metrics() ou autoplot() para ver o resultado
credit_resamples <- vfold_cv(credit_train, v = 5)

credit_lr_tune_grid <- tune_grid(
  credit_wf,
  resamples = credit_resamples,
  metrics = metric_set(
    accuracy, 
    kap, # KAPPA 
    roc_auc, 
    precision, 
    recall, 
    f_meas, 
    mn_log_loss #binary cross entropy
  )
)

# minha versão do autoplot()
collect_metrics(credit_lr_tune_grid)

collect_metrics(credit_lr_tune_grid) %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
  facet_wrap(~.metric, scales = "free") +
  scale_x_log10()

# PASSO 6) DESEMPENHO DO MODELO FINAL ------------------------------------------
# a) extrai melhor modelo com select_best()
# b) finaliza o modelo inicial com finalize_model()
# c) ajusta o modelo final com todos os dados de treino (bases de validação já era)
credit_lr_best_params <- select_best(credit_lr_tune_grid, "roc_auc")
credit_lr_model <- credit_lr_model %>% finalize_model(credit_lr_best_params)

credit_lr_last_fit <- last_fit(
  credit_lr_model,
  Status ~ .,
  credit_initial_split
)

# Variáveis importantes
credit_lr_last_fit_model <- credit_lr_last_fit$.workflow[[1]]$fit$fit
vip(credit_lr_last_fit_model)

# PASSO 7) GUARDA TUDO ---------------------------------------------------------
write_rds(credit_lr_last_fit, "credit_lr_last_fit.rds")
write_rds(credit_lr_model, "credit_lr_model.rds")

collect_metrics(credit_lr_last_fit)

credit_test_preds <- credit_lr_last_fit$.predictions[[1]]

# roc
credit_roc_curve <- credit_test_preds %>% roc_curve(Status, .pred_bad)
autoplot(credit_roc_curve)

# confusion matrix
credit_test_preds %>%
  mutate(
    Status_class = factor(if_else(.pred_bad > 0.6, "bad", "good"))
  ) %>%
  conf_mat(Status, Status_class)

# gráficos extras!

# risco por faixa de score
credit_test_preds %>%
  mutate(
    score =  factor(ntile(.pred_bad, 10))
  ) %>%
  count(score, Status) %>%
  ggplot(aes(x = score, y = n, fill = Status)) +
  geom_col(position = "fill") +
  geom_label(aes(label = n), position = "fill") +
  coord_flip()

# gráfico sobre os da classe "bad"
percentis = 20
credit_test_preds %>%
  mutate(
    score = factor(ntile(.pred_bad, percentis))
  ) %>%
  filter(Status == "bad") %>%
  group_by(score) %>%
  summarise(
    n = n(),
    media = mean(.pred_bad)
  ) %>%
  mutate(p = n/sum(n)) %>%
  ggplot(aes(x = p, y = score)) +
  geom_col() +
  geom_label(aes(label = scales::percent(p))) +
  geom_vline(xintercept = 1/percentis, colour = "red", linetype = "dashed", size = 1)

# PASSO 7) MODELO FINAL -----------------------------------------------------
credit_final_lr_model <- credit_lr_model %>% fit(Status ~ ., credit_data)



