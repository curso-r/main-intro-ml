# A base de dados 'concrete' consiste em dados sobre a composição de diferentes
# misturas de concreto e a sua resistência à compressão - 'compressive_strength'
# (https://pt.wikipedia.org/wiki/Esfor%C3%A7o_de_compress%C3%A3o)
# O nosso objetivo é prever a resitência à compressão a partir dos ingredientes.
# A coluna 'compressive_strength' é a variável resposta. A coluna 'age' nos diz
# a idade do concreto na hora do teste (o concreto fica mais forte ao longo do tempo) e
# o resto das colunas como 'cement' e 'water' são componentes do concreto em
# kilogramas por metro cúbico.

# Pacotes -----------------------------------------------------------------

library(tidymodels)

# Base de dados -----------------------------------------------------------

data(concrete, package = "modeldata")
nrow(concrete)
glimpse(concrete)

skimr::skim(concrete)
concrete %>%
  pivot_longer(cols = c(everything(), -compressive_strength),
               names_to = "var",
               values_to = "value") %>%
  ggplot(aes(x = value, y = compressive_strength)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~var, scales = "free")

# exercício 0 -------------------------------------------------------------
# Defina uma 'recipe' que normalize todas as variáveis explicativas.
# Dicas: recipe(), step_normalize(), all_numeric_predictors().

rec <- recipe(compressive_strength ~ ., data = concrete) %>%
  step_impute_mean(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

rec %>% prep(concrete) %>% juice()

# exercício 1 -------------------------------------------------------------
# Defina uma especificação de f que caracterize uma regressão linear
# (mode 'regression'). Especifique também que você deseja tunar a 'penalty' e
# a 'mixture'.
# Dicas: linear_reg(), set_engine(), set_mode(), tune().

mod <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# exercício 2 -------------------------------------------------------------
# Defina um 'workflow' que junte a receita do ex. 0 e o modelo do ex. 1.
# Dicas: workflow(), add_model(), add_recipe().

wflow <- workflow() %>%
  add_model(mod) %>%
  add_recipe(rec)

# exercício 3 -------------------------------------------------------------
# Crie um objeto que represente a estratégia de reamostragem do tipo K-Fold cross
# validation com 5 folds.
# Dica: vfold_cv().

set.seed(1)
init <- initial_split(concrete, prop = 3/4)
cv <- vfold_cv(training(init), v = 5)

# exercício 4 -------------------------------------------------------------
# Defina um grid de hiperparâmetros que você irá testar tanto de 'penalty' quanto
# de 'mixture'.
# Dica: grid_regular(), penalty(), mixture().

grid <- grid_regular(
  penalty(c(-2.5, 2)),
  mixture(),
  levels = 5
)

# exercício 5 -------------------------------------------------------------
# Execute a tunagem do modelo usando os objetos criados nos exercícios anteriores.
# Dica: tune_grid()

res <- tune_grid(
  object = wflow,
  resamples = cv,
  grid = grid,
  metrics = metric_set(rmse, rsq, mae),
  control = control_grid(verbose = TRUE)
)

# exercício 6 -------------------------------------------------------------
# Visualize os resultados dos modelos ajustados e atualize o workflow com os
# parâmetros do melhor modelo.
# Dica: autoplot(), collect_metrics(), show_best(), select_best(), finalize_workflow().

autoplot(res)
hp <- select_best(res, metric = "mae")
show_best(res, metric = "mae")

# desafio 1 ---------------------------------------------------------------
# Qual hiperparâmetro tem maior efeito no resultado do modelo? Justifique
# a sua afirmativa com um gráfico.

collect_metrics(res) %>%
  filter(.metric == "mae") %>%
  group_by(penalty) %>%
  summarise(v = mean(mean))

collect_metrics(res) %>%
  filter(.metric == "mae") %>%
  group_by(mixture) %>%
  summarise(v = mean(mean))


# exercício 7 -------------------------------------------------------------
# Ajuste o modelo na base de treino e verifique o desempenho na base de teste.

wflow_final <- finalize_workflow(wflow, hp)

# Dica: last_fit(split = ______initial_split), collect_metrics()

wflow_final %>%
  last_fit(split = init, metrics = metric_set(rmse, mae, rsq)) %>%
  collect_metrics()

# exercício 8 -------------------------------------------------------------
# Ajuste o modelo final para a base inteira salve-o em um arquivo .rds.
# Dica: fit(), saveRDS().

modelo_final <- wflow_final %>% fit(training(init))
predict(modelo_final, testing(init)) %>%
  bind_cols(testing(init)) %>%
  (metric_set(rmse, mae, rsq))(truth = compressive_strength, estimate = .pred)

saveRDS(res, "resultados_validacao_cruzada.rds")
saveRDS(modelo_final, "modelo_final.rds")

modelo_final <- wflow_final %>% fit(concrete)

novo_concreto <- tibble(cement = NA, blast_furnace_slag = 0, fly_ash = 0,
                        water = 162, superplasticizer = 2.5, coarse_aggregate = 1040,
                        fine_aggregate = 676, age = 28L, compressive_strength = 1)

predict(modelo_final, novo_concreto)
coef(extract_fit_engine(modelo_final), s = hp$penalty)

res2 <- readRDS("resultados_validacao_cruzada.rds")

factor(c("hello", "bye"), levels = c("hello", "bye"))
forcats::
