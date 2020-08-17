# install.packages("tidymodels")
# install.packages("ISLR")
# install.packages("tidyverse")
# install.packages("modeldata")
# install.packages("pROC")
# install.packages("vip")
# install.packages("skimr")
# install.packages("naniar")
# install.packages("GGally")
# install.packages("ggcorrplot")

#carrega pacotes

library(tidymodels)
library(ISLR)
library(tidyverse)
library(modeldata)
library(pROC)
library(vip)
library(skimr)
library(naniar)
library(GGally)
library(ggcorrplot)

#início do processo
set.seed(1)

#carrega o dataset (alterar para o caminho onde o dataset se encontra)
adults <- readr::read_rds("C:\\Users\\Erico\\Documents\\Erico\\Estudos\\Curso R\\Machine Learning\\Machine Learning\\Projeto Final\\adult.rds")

#exploração inicial do dataset
glimpse(adults)
skim(adults)
#vis_miss(adults)

#contagem das variáveis categóricas
adults %>%  count(workclass)
adults %>%  count(occupation)
adults %>%  count(education)
adults %>%  count(marital_status)
adults %>%  count(relationship)
adults %>%  count(race)
adults %>%  count(sex)
adults %>%  count(native_country)
adults %>%  count(education_num)

#Visualização da interação entre as variáveis
GGally::ggpairs(adults %>% select(-id,-occupation,-native_country,-education))

#Visualização da correlação entre variáveis
adults %>% 
  mutate(
    resposta = ifelse(resposta == "<=50K", 0, 1)
  ) %>% 
  select_if(is.numeric) %>%
  select(-id) %>% 
  cor(.) %>% 
  ggcorrplot(hc.order = TRUE, 
             lab = TRUE, 
             lab_size = 4,
             tl.cex = 10,
             method="square", 
             colors = c("lemonchiffon4", "white", "darkgreen"),
             ggtheme=theme_bw, insig = "blank")


#Comparativo das variaveis com maior correlação

# Education num vs hours per week vs race vs marital_status vs resposta (50k+ / 50k-)
adults %>% 
  ggplot(aes(x = hours_per_week, y = education_num)) +
  geom_point(aes(color = resposta)) +
  facet_grid(race ~ marital_status) +
  scale_color_manual(values = c("olivedrab2", "darkgreen")) + 
  theme_minimal(10)

# Education num vs hours per week vs workclass vs marital_status vs resposta (50k+ / 50k-)
adults %>% 
  filter(!is.na(workclass)) %>% 
  ggplot(aes(x = hours_per_week, y = education_num)) +
  geom_point(aes(color = resposta)) +
  facet_grid(workclass ~ marital_status) +
  scale_color_manual(values = c("olivedrab2", "darkgreen")) + 
  theme_minimal(10)

# Education num vs sex vs marital_status vs resposta (50k+ / 50k-)
adults %>% 
  ggplot(aes(x = resposta, y = education_num)) +
  geom_col(aes(fill = resposta)) +
  facet_grid(sex ~ marital_status) +
  scale_fill_manual(values = c("olivedrab2", "darkgreen")) + 
  theme_minimal(10)

#########################################
##Conclusões das Analises Explortórias.##
#########################################

# Com as análises realizadas acima conseguimos ter alguns insights que nos ajudam a escolher
# os melhores steps no momento da criação da nossa receita.
# Verificando a presença de NA nos campos de workclass, occupation e native_county e observando
# a distribuição das categorias, entendo que a melhor alternativa seria realizar imputção, utilizando a
# moda.
# 
# Analisando de forma geral a correlação entre as variáveis numéricas, entendo que existe uma importancia
# entre grau de instrução, capital gain e horas por semana, em relação a nossa variável resposta.
# 
# As variáveis numericas, estão em gradezas diferentes e por conta disso podemos utilizar a normalização.
# 
# Ao explorar mais detalhadamente, podemos notar que em alguns setores como o privado, o grau de instrução 
# é extremamente determinante para dizer se haverá ganho anual acima de 50K.
# Para autonomos esta regra não ocorre da mesma maneira.
# Podemos inferir que a raça e o estado civil são imporantes na predição do valor anual, pois pessoas casadas
# tem maior incidencia valores acima de 50K/ano. Ao contrario dos casados, quem é separado ou nunca casou
# tem uma maior incidencia de valores abaixo de 50K.
# Se compararmos os sexo, o comportamento de ambos os sexos é parecido em relação ao ganho, estado civil
# e grau de instrução.
# Em relação a raça, vemos claramente uma disparidade em relação aos ganhos em pessoas brancas e as
# demais raças. Quando isso é cruzado com estado civil e grau de instrução, a disparidade fica ainda mais
# transparente. Pessoas negras, viuvas ou separadas e pessoas amerindias/eskimo tem baixissima ou nenhuma
# incidencia de valores acima de 50K.


###################################
##início do processo de modelagem##
###################################

#Split inicial de treino e teste
adults_initial_split <- adults %>% initial_split(3/4)
adults_train <- training(adults_initial_split)
adults_test <- testing(adults_initial_split)

#criação da Receita com as transformações necessárias
adults_recipe <- recipe(resposta ~ ., data = adults_train) %>%
  #remove valores com zero variancia
  step_zv(all_predictors()) %>%
  #remove coluna de Id, que não será utilizda na modelagem
  step_rm(id) %>%
  #inclui os não identificados para as variáveis categóricas
  step_unknown(all_nominal(), -all_outcomes()) %>% 
  #realiza imputação da moda nas variáveis categoricas que tem NA - acho que o de cima (unknow) anula essa mas deixei no codigo.
  step_modeimpute(all_nominal(), -all_outcomes()) %>%
  #realiza imputação da mediana nas variáveis numéricas que tem NA
  step_medianimpute(all_numeric()) %>%
  #realiza a normalização das variáveis númericas
  step_normalize(all_numeric()) %>% 
  #prepara base para receber novas variavéis não identificadas
  step_novel(all_nominal(), -all_outcomes()) %>%
  #Faz a dumificação das variáveis categóricas para o modelo processar
  step_dummy(all_nominal(), -all_outcomes())


#Glimpse do juice da receita: verifica como ficou a receitinha pronta! 
#Você não irá perceber mas eu mudei isso 500 vezes até chegar aqui!
glimpse(juice(prep(adults_recipe)))


#tunando o modelo!!! Ta chegando a hora!
#modelo escolhido foi o xgboost.
adults_lr_model <- boost_tree(
  mode = "classification", 
  min_n = tune(),
  mtry = tune(),
  #Pesquisando intensamente na internet, encontrei esse valor mágico de 250 trees! 
  #Paciência na hora de rodar!
  trees = 250,
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune()
) %>%
  set_engine("xgboost", lambda = 0, params = list(min_child_weight = 0))

#Cria o workflow com o modelo + a receita.
adults_workflow <- workflow() %>%
  add_model(adults_lr_model) %>%
  add_recipe(adults_recipe)

#cria as folds do processo de cross validation. Sim, coloquei 10 no K! Paciência parte 2!!
adults_resamples <- vfold_cv(adults_train, v = 10)

#Aqui que a magica acontece. Por Favor, de o play vá tomar um bom café pq demora!! rss
adults_tune_grid <- tune_grid(
  adults_workflow,
  resamples = adults_resamples,
  metrics = metric_set(roc_auc)
)

#Seleciona o melhor modelo da cacetada de modelos que geramos acima.
adults_select_best <- select_best(adults_tune_grid, "roc_auc")
adults_workflow <- adults_workflow %>% finalize_workflow(adults_select_best)

#Faz o teste com todo mundo para saber se valeu a pena toda a espera.
adults_last_fit <- last_fit(
  adults_workflow,
  adults_initial_split
)

#Ve as métricas, que aparentemente ficaram boas.
collect_metrics(adults_last_fit)

#plot da Area sob a curva ROC.
collect_predictions(adults_last_fit) %>% 
  roc_curve(resposta, '.pred_>50K') %>% 
  autoplot()

#Verificando a importancia das variáveis.
vip(extract_model(adults_last_fit$.workflow[[1]]))
vi(extract_model(adults_last_fit$.workflow[[1]]))

###################
##Validação Final##
###################

#Chegou a hora de validar com os dados fresquinhos e desconhecidos que chegaram.
adults_lr_fit <- fit(adults_workflow, data = adults)

predict(adults_lr_fit, new_data = adults_test, type = "prob")

adult_val <- read_rds("C:\\Users\\Erico\\Documents\\Erico\\Estudos\\Curso R\\Machine Learning\\Machine Learning\\Projeto Final\\adult_val.rds")

adult_val_sumbissao <- adult_val %>%
  mutate(
    more_than_50k = predict(adults_lr_fit, new_data = adult_val, type = "prob")$'.pred_>50K'
  ) %>%
  select(id, more_than_50k)

head(adult_val_sumbissao)

write_csv(adult_val_sumbissao, "adult_val_sumbissao_er_v7.csv")


