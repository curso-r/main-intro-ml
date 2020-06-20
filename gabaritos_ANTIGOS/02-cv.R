# 1. Encontre o melhor min_n para a base Hitters.

especificacao_arvore_n_variavel <- decision_tree(min_n = tune(),
                                                 cost_complexity = tune()) %>%
  set_mode("regression") %>%
  set_engine("rpart")

#quebra_5fold <- vfold_cv(Hitters, v = 5)
quebra_mc <- mc_cv(Hitters, prop = .7, times = 2)

tunado <- tune_grid(
  HmRun ~ CHmRun,
  especificacao_arvore_n_variavel,
  grid = 20,
  resamples = quebra_mc,
  metrics = metric_set(rmse, mae)
)

autoplot(tunado)

# 2. [Extra] Encontra o melhor min_n para a base Hitters, mas fazendo um modelo que
# tem todas as variáveis históricas dos jogadores (elas começam com C maiúsculo).
# Lembre-se: se você quiser usar mais variáveis no seu modelo de árvore,
# basta fazer x+y+z quando for especificar a formula.

especificacao_arvore_n_variavel <- decision_tree(min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("rpart")

quebra_5fold <- vfold_cv(Hitters, v = 5)

tunado <- tune_grid(
  HmRun ~ CHmRun+CAtBat+CHits+CRuns+CRBI+CWalks,
  especificacao_arvore_n_variavel,
  grid = 10,
  resamples = quebra_5fold,
  metrics = metric_set(rmse, mae)
)

autoplot(tunado)
