# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(parsnip)
library(ISLR)
library(rsample)
library(dials)
library(yardstick)

# Me ----------------------------------------------------------------------

todos_os_dados <- bind_rows(dados1, dados2, dados3, dados4)

quebra_5p <- vfold_cv(todos_os_dados, v = 5)
# MC CV serve para termos o treino e o teste dentro do mesmo objeto

especificacao_arvore_n_variavel <- decision_tree(min_n = tune()) %>%
  # A especificação é igual, mas se queremos que a f possa ter qualquer n, e não um fixo,
  # usamos a função tune()
  set_mode("regression") %>%
  set_engine("rpart")

# Us ---------------------------------------------------------------------

# deixando o tune_grid() sortear valores para os hiperparâmetros
tune <- tune_grid(
  # quando for fitar vários modelos, você deve usar a função tune_grid
  y ~ x,
  especificacao_arvore_n_variavel,
  grid = 20,
  # o parâmetro levels define em quantos pontos queremos fazer a conta
  resamples = quebra_5p,
  metrics = metric_set(rmse, mae, mape, mase)
)

autoplot(tune)


# usando lista de parâmetros explicitamente com as funções parameters() + update() + min_n(range =) + grid_regular
pars <- parameters(especificacao_arvore_n_variavel) %>% 
  update(min_n = dials::min_n(range = c(10, 40))) %>%
  grid_regular(levels = 4)
# precisamos trocar os XXX pelo range de hiperparâmetros que queremos procurar

tune <- tune_grid(
  # quando for fitar vários modelos, você deve usar a função tune_grid
  y ~ x,
  especificacao_arvore_n_variavel,
  grid = pars,
  # o parâmetro levels define em quantos pontos queremos fazer a conta
  resamples = quebra_5p,
  metrics = metric_set(rmse, mae, mape, mase)
)

autoplot(tune)

# You ---------------------------------------------------------------------

# 1. Encontre o melhor min_n para a base Hitters.
# 2. [Extra] Encontra o melhor min_n para a base Hitters, mas fazendo um modelo que
# tem todas as variáveis históricas dos jogadores (elas começam com C maiúsculo).
# Lembre-se: se você quiser usar mais variáveis no seu modelo de árvore,
# basta fazer x+y+z quando for especificar a formula.
