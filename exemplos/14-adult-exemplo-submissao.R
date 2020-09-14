# Pacotes ------------------------------------------------------------------
library(tidyverse)
library(tidymodels)

# CARREGAR A BASE ----------------------------------------------------------
adult <- read_rds("dados/adult.rds")

# modelo nulo --------------------------------------------------------------
adult_recipe <- recipe(resposta ~ ., adult)
adult_model <- null_model()
adult_wf <- workflow() %>% add_model(adult_model) %>% add_recipe(adult_recipe)
adult_fit <- adult_wf %>% fit(adult)

# ESCORA BASE DE VALIDACAO -------------------------------------------------
adult_val <- read_rds("dados/adult_val.rds")

adult_val_sumbissao <- adult_val %>%
  mutate(
    more_than_50k = predict(adult_fit, new_data = adult_val, type = "prob")$`.pred_>50K`
  ) %>%
  select(id, more_than_50k)

head(adult_val_sumbissao)

write_csv(adult_val_sumbissao, "adult_val_sumbissao.csv")
