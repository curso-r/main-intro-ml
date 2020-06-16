# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(rpart) # recursive partitioning
library(rpart.plot)
library(parsnip)
library(ISLR)
library(rsample)
library(dials)
library(yardstick)
library(tidyverse)
library(pROC)

# Me ----------------------------------------------------------------------

# PASSO 0) CARREGAR AS BASES
credit_data <- read_rds("dados/credit_data.rds") %>% na.omit()
glimpse(credit_data) # German Risk 

credit_data %>% count(Status)

# PASSO 1) BASE TREINO/TESTE
set.seed(1)
data_split <- initial_split(credit_data, strata = "Status", p = 0.75)

credit_train <- training(data_split)
credit_test  <- testing(data_split)

# PASSO 2) EXPLORAR A BASE

## veremos na última aula!!

# PASSO 3) DATAPREP

## veremos na última aula!!

# PASSO 4) MODELO
# Definição de 
# a) a f(x) (ou do modelo): logistc_reg()
# b) modo (ou natureza da var resp): classification
# c) hiperparametros que queremos tunar: penalty = tune()
# d) hiperparametros que não queremos tunar: mixture = 0 # QUERO FAZER LASSO
# e) o motor que queremos usar: glmnet
credit_lr_model <- logistic_reg(penalty = tune(), mixture = 0) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

# PASSO 5) TUNAGEM DE HIPERPARÂMETROS
# a) bases de reamostragem para validação: vfold_cv()
# b) (opcional) grade de parâmetros: parameters() %>% update() %>% grid_regular()
# c) tune_grid(y ~ x + ...)
# d) escolha das métricas (rmse, roc_auc, etc)
# d) collect_metrics() ou autoplot() para ver o resultado
credit_resamples <- vfold_cv(credit_train, v = 5)

credit_lr_tune_grid <- tune_grid(
  Status ~ .,
  credit_lr_model,
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

# PASSO 6) MODELO FINAL
# a) extrai melhor modelo com select_best()
# b) finaliza o modelo inicial com finalize_model()
# c) ajusta o modelo final com todos os dados de treino (bases de validação já era)
credit_lr_best_params <- select_best(credit_lr_tune_grid, "roc_auc")
credit_lr_model <- credit_lr_model %>% finalize_model(credit_lr_best_params)

credit_lr_fit <- fit(
  credit_lr_model,
  Status ~ .,
  data = credit_train
)

# PASSO 7) PREDIÇÕES
# a) escora a base de teste (escolher o type: se quer a probabilidade ou a classe)
# b) calcula métricas de erro/relatório final
# c) usa o modelo para tomar decisões

# predict
credit_probs <- predict(credit_lr_fit, credit_test, type = "prob")
credit_test$Status_prob <- credit_probs$.pred_bad

# roc
credit_roc_curve <- credit_test %>% roc_curve(Status, Status_prob)
autoplot(credit_roc_curve)

# confusion matrix
credit_test %>%
  mutate(
    Status_class = factor(if_else(Status_prob > 0.6, "bad", "good"))
  ) %>%
  conf_mat(Status, Status_class)

# gráficos extras!

# risco por faixa de score
credit_test %>%
  mutate(
    score =  factor(ntile(Status_prob, 10))
  ) %>%
  count(score, Status) %>%
  ggplot(aes(x = score, y = n, fill = Status)) +
  geom_col(position = "fill") +
  geom_label(aes(label = n), position = "fill") +
  coord_flip()

# gráfico sobre os da classe "bad"
credit_test %>%
  mutate(
    score = factor(ntile(Status_prob, 10))
  ) %>%
  filter(Status == "bad") %>%
  group_by(score) %>%
  summarise(
    n = n(),
    media = mean(Status_prob)
  ) %>%
  mutate(p = n/sum(n)) %>%
  ggplot(aes(x = score, y = p)) +
  geom_col() +
  geom_label(aes(label = scales::percent(p))) +
  coord_flip()


# Us ---------------------------------------------------------------------

# Repetir os passos ajustando uma decision_tree() em vez de logistic_reg()

# PASSO 4) MODELO
# Definição de 
# a) a f(x) (ou do modelo): logistc_reg()
# b) modo (ou natureza da var resp): classification
# c) hiperparametros que queremos tunar: penalty = tune()
# d) hiperparametros que não queremos tunar: mixture = 0
# e) o motor que queremos usar: glmnet
credit_tree_model <- decision_tree(min_n = tune(), tree_depth = 10, cost_complexity = 0) %>%
  set_mode("classification") %>%
  set_engine("rpart")

# PASSO 5) TUNAGEM DE HIPERPARÂMETROS
# a) bases de reamostragem para validação: vfold_cv()
# b) (opcional) grade de parâmetros: parameters() %>% update() %>% grid_regular()
# c) tune_grid(y ~ x + ...)
# d) escolha das métricas (rmse, roc_auc, etc)
# d) collect_metrics() ou autoplot() para ver o resultado
credit_tree_resamples <- vfold_cv(credit_train, v = 5)

credit_tree_tune_grid <- tune_grid(
  Status ~ .,
  credit_tree_model,
  resamples = credit_tree_resamples,
  metrics = metric_set(roc_auc, recall, precision)
)

# minha versão do autoplot()
autoplot(credit_tree_tune_grid)

# PASSO 6) MODELO FINAL
# a) extrai melhor modelo com select_best()
# b) finaliza o modelo inicial com finalize_model()
# c) ajusta o modelo final com todos os dados de treino (bases de validação já era)
credit_tree_best <- select_best(credit_tree_tune_grid, "roc_auc")
credit_tree_model <- credit_tree_model %>% finalize_model(credit_tree_best)

credit_tree_fit <- fit(
  credit_tree_model,
  Status ~.,
  data = credit_train,
  metrics = metric_set(roc_auc, recall, precision)
)

# PASSO 7) PREDIÇÕES
# a) escora a base de teste (escolher o type: se quer a probabilidade ou a classe)
# b) calcula métricas de erro/relatório final
# c) usa o modelo para tomar decisões
# d) curva ROC

credit_test$Status_prob_tree <- predict(credit_tree_fit, new_data = credit_test, type = "prob")$.pred_bad

# roc
credit_roc_curve <- credit_test %>% roc_curve(Status, Status_prob_tree)
autoplot(credit_roc_curve)
credit_test %>% roc_auc(Status, Status_prob_tree)

# confusion matrix
credit_test %>%
  mutate(
    Status_class = factor(if_else(Status_prob_tree > 0.6, "bad", "good"))
  ) %>%
  conf_mat(Status, Status_class)

# You ---------------------------------------------------------------------

# Exercício 1) Queremos comparar os modelos.
# Crie uma tabela que contenha uma linha por modelo e uma coluna por métrica.

credit_lr_preds <- predict(credit_lr_fit, credit_test, type = "prob") %>% 
  mutate(
    modelo = "lr",
    Status = factor(credit_test$Status)
  )

credit_tree_preds <- predict(credit_tree_fit, credit_test, type = "prob") %>% 
  mutate(
    modelo = "tree",
    Status = factor(credit_test$Status)
  )

credit_preds <- bind_rows(
  credit_tree_preds,
  credit_lr_preds,
) %>%
  mutate(
    pred_prob = .pred_bad,
    pred_class = factor(if_else(pred_prob > 0.5,  "bad", "good"))
  ) 

# RESULTS
credit_preds %>% group_by(modelo) %>% roc_auc(Status, pred_prob)
credit_preds %>% group_by(modelo) %>% accuracy(Status, pred_class)
credit_preds %>% group_by(modelo) %>% recall(Status, pred_class)

credit_comparacao_de_modelos <- credit_preds %>%
  group_by(modelo) %>%
  summarise(
    auc = roc_auc_vec(Status, pred_prob),
    acc = accuracy_vec(Status, pred_class),
    prc = precision_vec(Status, pred_class),
    rec = recall_vec(Status, pred_class),
    roc = list(roc(Status, pred_prob)) # pROC::roc
  ) %>%
  mutate(roc = set_names(roc, modelo))

# Exercício EXTRA1)
# Gráficos de risco para o score do modelo de árvore!

# risco por faixa de score
# gráfico para quaisquer modelos que estejam na base!
credit_test %>%
  gather(modelo, pred, starts_with("Status_prob")) %>%
  group_by(modelo) %>%
  mutate(
    score =  factor(ntile(pred, 10))
  ) %>%
  count(modelo, score, Status) %>%
  ggplot(aes(x = score, y = n, fill = Status)) +
  geom_col(position = "fill") +
  geom_label(aes(label = n), position = "fill") +
  facet_wrap(~ modelo) +
  coord_flip()

# Gráfico só para LR!!
credit_test %>%
  mutate(
    score =  factor(ntile(Status_prob_tree, 10))
  ) %>%
  count(score, Status) %>%
  ggplot(aes(x = score, y = n, fill = Status)) +
  geom_col(position = "fill") +
  geom_label(aes(label = n), position = "fill") +
  coord_flip()


# Exercício EXTRA2)
# As duas curvas ROC (da logistica e da arvore) no mesmo gráfico.

# ROC tipo 1
ggroc(credit_comparacao_de_modelos$roc)

# ROC tipo 2
credit_preds %>% 
  group_by(modelo) %>% 
  roc_curve(Status, pred_prob) %>% 
  autoplot()

