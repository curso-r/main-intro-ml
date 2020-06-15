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

# Me ----------------------------------------------------------------------

# PASSO 0) CARREGAR AS BASES
credit_data <- read_rds("dados/credit_data.rds") %>% na.omit()

credit_tree_model <- decision_tree(min_n = 31, tree_depth = 5) %>% set_mode("classification")

credit_tree_fit <- fit(
  credit_tree_model,
  Status ~.,
  data = credit_data
)

rpart.plot(credit_tree_fit$fit, roundint=FALSE)
cp <- as.data.frame(credit_tree_fit$fit$cptable)

