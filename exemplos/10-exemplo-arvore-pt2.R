# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(ISLR)
library(tidyverse)
library(modeldata)
library(pROC)
library(vip)
library(e1071)

# PASSO 0) CARREGAR AS BASES -----------------------------------------------
data("credit_data")
help(credit_data)
credit_data <- credit_data %>% na.omit()

credit_tree_model <- decision_tree(
  min_n = 50, 
  tree_depth =2,
  cost_complexity = 0.001
) %>% 
  set_mode("classification")

credit_tree_fit <- fit(
  credit_tree_model,
  Status ~.,
  data = credit_data
)

rpart.plot(credit_tree_fit$fit, roundint=FALSE)

cp <- as.data.frame(credit_tree_fit$fit$cptable)
cp

vip::vip(credit_tree_fit$fit)

