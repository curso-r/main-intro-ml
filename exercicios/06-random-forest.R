# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(ISLR)
library(tidyverse)
library(modeldata)
library(pROC)
library(vip)
library(skimr)
library(naniar)

# Exercício de Random Forest #########################################
# Objetivo: fazer um modelo preditivo usando random forest para prever
# sobreviventes no desastre do Titanic (variável 'Survived').


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
  step_zv(all_predictors()) %>%
  step_rm(Name, Ticket, Cabin) %>%
  step_modeimpute(all_nominal(), -all_outcomes()) %>%
  step_medianimpute(all_numeric()) %>%
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes())

# para ficar checando o resultado do recipe
# bake(prep(titanic_recipe), new_data = NULL)

# PASSO 4) MODELO ----------------------------------------------------------
# f(x) modo engine
# tunar mtry, trees e min_n

# PASSO 5) WORKFLOW ----------------------------------------------------------------
# workflow add_model add_recipe

# PASSO 6) TUNAGEM DE HIPERPARÂMETROS --------------------------------------
# vfold_cv() tune_grid()

# PASSO 7) SELECAO DO MODELO FINAL --------------------------------------------
# autoplot select_best finalize_workflow last_fit 

# PASSO 8) DESEMPENHO DO MODELO FINAL ------------------------------------------
# collect_metrics collect_predictions roc_curve autoplot

# PASSO 9) Variáveis importantes -----------------------------------------
# vip(extract_model(titanic_last_fit$.workflow[[1]]))
# vi(extract_model(titanic_last_fit$.workflow[[1]]))


# PASSO 10) MODELO FINAL ------------------------------------------------------
# fit (usando a base titanic)

# PASSO 11) GUARDA TUDO ---------------------------------------------------------
# write_rds()
# 1) guarda o last_fit
# 2) guarda o fit
# 3) guarda o que quiser reutilizar depois, por exemplo a tabela do vi().


# PASSO 12) ESCORE A BASE DE TESTE DO KAGGLE -----------------------------------
# predict(new_data = titanic::titanic_test, type = "prob")


