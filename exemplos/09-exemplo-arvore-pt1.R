library(tidymodels)
library(tidyverse)
library(rpart)
library(rpart.plot)

dados <- tribble(
  ~Pressão,	~Glicose,	~Diabetes,
  "hipertensao" ,92  ,    "nao",
  'normal'	    ,130 ,    "sim",
  "normal"	    ,130 ,    "nao",
  "normal"      ,55  ,    "nao",
  "hipertensao"	,220 ,    "sim",
  "normal"	    ,195 ,    "sim"
) %>%
  mutate(
    Diabetes = as.factor(Diabetes)
  )

diabetes_tree_model <- decision_tree(
  min_n = 1, 
  cost_complexity = -1, 
  tree_depth = 20
) %>% 
  set_mode("classification") %>%
  set_engine("rpart")

credit_tree_fit <- fit(
  diabetes_tree_model,
  Diabetes ~.,
  data = dados
)

rpart.plot(credit_tree_fit$fit, roundint=FALSE, cex = 4)
cp <- as.data.frame(credit_tree_fit$fit$cptable)
cp
