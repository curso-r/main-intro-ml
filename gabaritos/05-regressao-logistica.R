# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(ISLR)
library(tidyverse)
library(modeldata)
library(pROC)
library(vip)
library(skimr)
library(naniar)

# Exercício de Regressão Logística #########################################
# Objetivo: fazer um modelo preditivo usando regressão logística para prever
# sobreviventes no desastre do Titanic (variável 'Survived') 

# PASSO 0) CARREGAR AS BASES -----------------------------------------------
# instale o pacote titanic para pegar a base de dados "titanic::titanic_train"
# install.packages("titanic")

# PASSO 1) BASE TREINO/TESTE -----------------------------------------------
set.seed(1)
titanic <- titanic::titanic_train %>% mutate(Survived = factor(Survived, levels = c(1, 0), labels = c("yes", "no")))
titanic_initial_split <- initial_split(titanic)

titanic_train <- training(titanic_initial_split)
titanic_test <- testing(titanic_initial_split)

# PASSO 2) EXPLORAR A BASE -------------------------------------------------
# problemas para analisar:
# 2) os tipos das variáveis estão OK? numericas são numericas e categóricas são categóricas?
# 1) a proporcao da variável resposta está balanceada?
# 1) valores faltantes: existem variáveis com poucos dados faltantes?
# 1) valores faltantes: existem variáveis com muitos dados faltantes? é possível imputar?
# 3) existem variáveis categóricas com muitas categorias? (10 ou mais)
# 3) existem variáveis categóricas com textos muito poluídos e com informação escondida a ser extraída?
# 4) existem variáveis numéricas com muitos zeros?
# 4) existem variáveis constantes? (que é tudo igual na base inteira)
# 4) existem variáveis numéricas muito assimétricas com potencial de outliers?
# 4) existem variáveis numéricas com escalas muito diferentes (uma vai de -1 a 1 e a outra vai de 0 a 1000, por ex)?
# 5) existem variáveis explicativas muito correlacionadas umas com as outras?
# 6) 'dummificar' variáveis categóricas (exceto a variável resposta,'outcome') (importantíssimo!!)

# PASSO 3) DATAPREP --------------------------------------------------------
titanic_recipe <- recipe(Survived ~ ., data = titanic_train) %>%
  # step_bin2factor(Survived, ref_first = FALSE)  %>%
  # step_mutate(Survived = factor(Survived)) %>%
  step_zv(all_predictors()) %>%
  step_rm(Name, Ticket, Cabin) %>%
  step_modeimpute(all_nominal(), -all_outcomes()) %>%
  step_medianimpute(all_numeric()) %>%
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes())


juice(prep(titanic_recipe)) %>% count(Survived)

# PASSO 4) MODELO ----------------------------------------------------------
# f(x) modo engine
# tunar penalty e deixar mixture = 1
titanic_lr_model <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

# PASSO 5) WORKFLOW ----------------------------------------------------------------
# workflow add_model add_recipe
titanic_workflow <- workflow() %>%
  add_model(titanic_lr_model) %>%
  add_recipe(titanic_recipe)

# PASSO 6) TUNAGEM DE HIPERPARÂMETROS --------------------------------------
# vfold_cv() tune_grid()
titanic_resamples <- vfold_cv(titanic_train, v = 5)

titanic_tune_grid <- tune_grid(
  titanic_workflow,
  resamples = titanic_resamples,
  metrics = metric_set(roc_auc)
)

# PASSO 7) SELECAO DO MODELO FINAL --------------------------------------------
# autoplot select_best finalize_workflow last_fit 
autoplot(titanic_tune_grid) + scale_x_log10()

titanic_select_best <- select_best(titanic_tune_grid, "roc_auc")
titanic_workflow <- titanic_workflow %>% finalize_workflow(titanic_select_best)

titanic_last_fit <- last_fit(
  titanic_workflow,
  titanic_initial_split
)

# PASSO 7) DESEMPENHO DO MODELO FINAL ------------------------------------------
# collect_metrics collect_predictions roc_curve autoplot
collect_metrics(titanic_last_fit)

collect_predictions(titanic_last_fit) %>% 
  roc_curve(Survived, .pred_yes) %>% 
  autoplot()

# PASSO 8) Variáveis importantes -----------------------------------------
vip(extract_model(titanic_last_fit$.workflow[[1]]))
vi(extract_model(titanic_last_fit$.workflow[[1]]))


# PASSO 9) MODELO FINAL ------------------------------------------------------
# fit
titanic_lr_fit <- fit(titanic_workflow, data = titanic)

# PASSO 10) GUARDA TUDO ---------------------------------------------------------
# write_rds()
# 1) guarda o last_fit
# 2) guarda o fit
# 3) guarda o que quiser reutilizar depois, por exemplo a tabela do vi().


# PASSO 11) ESCORE A BASE DE TESTE DO KAGGLE -----------------------------------
# predict() da base titanic::titanic_test

predict(titanic_lr_fit, new_data = titanic::titanic_test, type = "prob")


