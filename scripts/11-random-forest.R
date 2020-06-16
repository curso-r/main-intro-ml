# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(rpart)
library(rpart.plot)
library(parsnip)
library(ISLR)
library(rsample)
library(dials)
library(yardstick)
library(tidyverse)

# bibliotecas novas
library(tictoc)
library(pROC)

# You ----------------------------------------------------------------------
# Ajustar uma random forest.

# DICAS:
# 1) a f(x) é a rand_forest() e a engine é o "ranger" (default, não precisa mudar).
 
# 2) Existem três hiperparâmetros para tunar no rand_forest(). Vou deixar para vocês
# explorarem quais são consultando a documentação da função (help(rand_forest)).
# Essas consultas vão ficar comuns quando forem explorar novos modelos.

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

credit_tree_model <- decision_tree(min_n = tune(), tree_depth = 10, cost_complexity = 0) %>%
  set_mode("classification") %>%
  set_engine("rpart")

# PASSO 5) TUNAGEM DE HIPERPARÂMETROS
# a) bases de reamostragem para validação: vfold_cv()
# b) (opcional) grade de parâmetros: parameters() %>% update() %>% grid_regular()
# c) tune_grid(y ~ x + ...)
# d) escolha das métricas (rmse, roc_auc, etc)
# d) collect_metrics() ou autoplot() para ver o resultado
credit_resamples <- vfold_cv(credit_train, v = 5)

tic("lr")
credit_lr_tune_grid <- tune_grid(
  Status ~ .,
  credit_lr_model,
  resamples = credit_resamples,
  metrics = metric_set(roc_auc, recall, precision)
)
toc()

tic("tree")
credit_tree_tune_grid <- tune_grid(
  Status ~ .,
  credit_tree_model,
  resamples = credit_resamples,
  metrics = metric_set(roc_auc, recall, precision)
)
toc()

# PASSO 6) MODELO FINAL
# a) extrai melhor modelo com select_best()
# b) finaliza o modelo inicial com finalize_model()
# c) ajusta o modelo final com todos os dados de treino (bases de validação já era)
meu_fit <- function(tune_grid, model, metric = "roc_auc") {
  credit_best_model <- select_best(tune_grid, metric)
  credit_final_model <- finalize_model(model, credit_best_model)
  
  credit_fit <- fit(
    credit_final_model,
    Status ~ ., 
    data = credit_train
  )
  credit_fit
}

credit_lr_fit <- meu_fit(credit_lr_tune_grid, credit_lr_model)
credit_tree_fit <- meu_fit(credit_tree_tune_grid, credit_tree_model)

# PASSO 7) PREDIÇÕES
# a) escora a base de teste (escolher o type: se quer a probabilidade ou a classe)
# b) calcula métricas de erro/relatório final
# c) usa o modelo para tomar decisões
# d) ROC

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
    pred_prob = `.pred_bad`,
    pred_class = factor(if_else(pred_prob > 0.5,  "bad", "good"))
  ) 

# RESULTS
credit_comparacao_de_modelos <- credit_preds %>%
  group_by(modelo) %>%
  summarise(
    auc = roc_auc_vec(Status, pred_prob),
    acc = accuracy_vec(Status, pred_class),
    prc = precision_vec(Status, pred_class),
    rec = recall_vec(Status, pred_class),
    roc = list(roc(Status, pred_prob))
  ) %>%
  mutate(roc = set_names(roc, modelo))

# ROC tipo 1
ggroc(credit_comparacao_de_modelos$roc)

# ROC tipo 2
credit_preds %>% 
  group_by(modelo) %>% 
  roc_curve(Status, pred_prob) %>% 
  autoplot()
