# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(parsnip)
library(ISLR)
library(rsample)


# Me ----------------------------------------------------------------------

todos_os_dados <- bind_rows(dados1, dados2, dados3, dados4)

boston_split <- mc_cv(Boston, prop = 0.8, times = 1)

especificacao_arvore_n_variavel <- decision_tree(min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("rpart")