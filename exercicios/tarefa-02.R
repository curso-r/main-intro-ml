# Tarefa 02 - Classificação com Regressão Logística e Modelos de árvore
# ANÁLISE DA BASE ADULTS

# OBJETIVO PRINCIPAL: CRIAR UM CÓDIGO QUE GERE 2 OU MAIS TIPOS DE MODELOS (EX: REGRESSÃO LINEAR E RANDOM FOREST)

# BONUS: foi colocada uma etapa de "exploração" dos dados que será pauta da próxima aula. 

# Pacotes ------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(parsnip)
library(ISLR)
library(rsample)
library(dials)
library(yardstick)
library(rpart)
library(rpart.plot)

#####################################################################################################
# PASSO 0) CARREGAR AS BASES

# Download dos dados -----------------------------------------------------------------------------

# baixa adult.data se nao existe ainda
if(!file.exists("dados/adult.data")) 
  httr::GET("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", httr::write_disk("dados/adult.data"))

# baixa adult.test se nao existe ainda
if(!file.exists("dados/adult.test"))
  httr::GET("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test", httr::write_disk("dados/adult.test"))

# baixa adult.names se nao existe ainda
if(!file.exists("dados/adult.names"))
  httr::GET("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names", httr::write_disk("dados/adult.names"))

# Carrega dados ---------------------------------------------------------------------------------------

# prepara os nomes das colunas para colocar no cabecalho
adult_names <- tibble(name = read_lines("dados/adult.names")) %>%
  filter(
    str_detect(name, "^[^\\|].*:")
  ) %>%
  separate(name, c("name", "description"), sep = ":") %>%
  mutate(
    name = snakecase::to_snake_case(name)
  ) %>%
  add_row(name = "less_than_50k", description = "person earn more than USD 50K per year.")

# treino/teste 
adult_train <- read_csv(file = "dados/adult.data", na = c("?", "", "NA"), col_names = adult_names$name)
adult_test  <- read_csv(file = "dados/adult.test", na = c("?", "", "NA"), col_names = adult_names$name, skip = 1) %>%
  mutate(
    less_than_50k = if_else(less_than_50k == "<=50K.", "<=50K", ">50K")
  )

#####################################################################################################
# PASSO 1) BASE TREINO/TESTE

#####################################################################################################
# PASSO 2) EXPLORAR A BASE
# PASSO 3) DATAPREP

# map(list(test = adult_test, train = adult_train), introduce) %>% enframe() %>% unnest() %>% t %>% as_tibble() %>% set_names(c("test", "train"))
# skimr::skim(adult_train)
# skimr::skim(adult_test)
# 
# plot_bar(adult_train)
# plot_histogram(adult_train)
# plot_histogram(adult_train %>% mutate_if(is.numeric, log1p))
# plot_qq(adult_train)
# plot_qq(adult_train %>% mutate_if(is.numeric, log1p))
# plot_correlation(na.omit(adult_train), maxcat = 5L)
# plot_correlation(na.omit(adult_train), type = "d")
# gg_miss_var(adult_train)
# vis_miss(adult_train)
# gg_miss_case(adult_train)

# NESSA PARTE REMOVE-SE O NA APENAS PARA SEGUIR MAIS SUAVE O EXERCÍCIO, MAS NA PRÓXIMA AULA 
# IREMOS TRATAR ESSE PROBLEMA DE MANEIRA MAIS ADEQUADA!
adult_train <- adult_train %>% na.omit()
adult_test <- adult_test %>% na.omit()

#####################################################################################################
# PASSO 4) MODELO
# Definição de 
# a) a f(x) (ou do modelo): logistc_reg(), etc...
# b) modo (ou natureza da var resp): classification
# c) hiperparametros que queremos tunar: tune()
# d) hiperparametros que não queremos tunar:
# e) o motor que queremos usar

adult_tree_model <- decision_tree(cost_complexity = tune()) %>%
  set_mode("classification") %>%
  set_engine("rpart")

adult_rf_model <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")

adult_lr_model <- logistic_reg(penalty = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

adult_xgb_model <- boost_tree(
  mtry = tune(), min_n = tune(), 
  tree_depth = tune(), 
  trees = 1500, sample_size = 0.75, 
  learn_rate = tune(), loss_reduction = tune()) %>%
  set_mode("classification") %>%
  set_engine("xgboost")

#####################################################################################################
# PASSO 5) TUNAGEM DE HIPERPARÂMETROS
# a) bases de reamostragem para validação: vfold_cv()
# b) (opcional) grade de parâmetros: parameters() %>% update() %>% grid_regular()
# c) tune_grid(y ~ x + ...)
# d) collect_metrics() ou autoplot() para ver o resultado

#####################################################################################################
# PASSO 6) MODELO FINAL
# a) extrai melhor modelo com select_best()
# b) finaliza o modelo inicial com finalize_model()
# c) ajusta o modelo final com todos os dados de treino (bases de validação já era)

#####################################################################################################
# PASSO 7) PREDIÇÕES
# a) escora a base de teste
# b) calcula métricas de erro
# c) usa o modelo para tomar decisões
# d) curva ROC

