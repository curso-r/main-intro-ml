# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(parsnip)
library(ISLR)

# Me ----------------------------------------------------------------------

Hitters

# Conceito

# Precisamos passar pro R:
# 1. A f que queremos usar
# 2. Aplicar a f para um conjunto de dados

# Passo 1: Especificações de f:

especificacao_lasso <- linear_reg(penalty = tune()) %>%
  set_engine("glmnet") %>% 
  set_mode("regression")

base_treino <- Hitters %>% 
  filter(!is.na(Salary))

# Us ----------------------------------------------------------------------

# Passo 2: Ajuste do modelo

fit_lasso <- tune_grid(
  Salary~.,
  especificacao_lasso,
  grid = 2000,
  resamples = mc_cv(base_treino, prop = .75),
  metrics = metric_set(rmse, rsq, mae)
)

autoplot(fit_lasso) + 
  scale_x_log10()

pars <- parameters(especificacao_lasso) %>% 
  update(penalty = dials::penalty(range = c(.01, .5))) %>%
  grid_regular(levels = 150)

fit_lasso2 <- tune_grid(
  Salary~.,
  especificacao_lasso,
  grid = pars,
  resamples = mc_cv(base_treino, prop = .75),
  metrics = metric_set(rmse, rsq, mae)
)

autoplot(fit_lasso2) + 
  scale_x_log10()

melhores_parametros <- select_best(fit_lasso2, "rmse", maximize = FALSE)

modelo_final <- especificacao_lasso %>% 
  set_args(penalty = 1.51) %>% 
  fit(Salary ~ ., data = base_treino)

coefficients(modelo_final$fit, s = 1.51)

base_treino$Salary_pred <- predict(modelo_final, new_data = base_treino)$.pred


# You  --------------------------------------------------------------------

# 1. Faça uma regressão linear sem regularização que preve Salary e usa como covariável todas as variáveis da base Hitters.
# Compare o rmse e o mae desse modelo que ajustou com o modelo regularizado.
# 2. Encontre o melhor parametro penalty para prever mpg num modelo que usa todas as variáveis da base de dados Auto