# Pacotes ------------------------------------------------------------------

library(ggplot2)
library(tidymodels)

# Dados -------------------------------------------------------------------
data("diamonds")

# base treino e teste -----------------------------------------------------
# crie as tabelas de treino e teste com proporções de 80% e 20% respectivamente.
# use initial_split(), training() e testing()
set.seed(1)





# definicao do modelo -----------------------------------------------------
# Defina um modelo de árvore de decisão para regressão usando rpart e 
# prepare para tunar os hiperparâmetros cost_complexity e min_n. 
# Deixe o tree_depth fixo em 10.
# use as funções decision_tree(), tune(), set_engine() e set_mode().






# reamostragem com cross-validation ---------------------------------------
# Crie 5 conjuntos de cross-validation
# use vfold_cv().




# tunagem de hiperparametros ----------------------------------------------
# tune os hiperparâmetros usando somente a métrica rmse. Faça um grid de tamanho 5.
# OBS: a variável resposta é 'price' e a variável explicativa é 'x'.
# use tune_grid().






# inspecao da tunagem -----------------------------------------------------
# autoplot(...)
# collect_metrics(...)
# show_best(...)

# seleciona o melhor conjunto de hiperparametros --------------------------
# extraia o conjunto de hiperparametros que apresentou o melhor rmse de 
# cross-validation.
# use select_best().




# finalizacao do modelo ---------------------------------------------------
# atualize a especificação do modelo com os hiperparametros encontrados na
# secao acima.
# use finalize_model().




# desempenho do modelo final ----------------------------------------------
# rode o ultimo ajuste para calcular o rmse na base de teste.
# use last_fit().




# collect_metrics(...)
# collect_predictions(...) %>%
#   ggplot(aes(.pred, price)) +
#   geom_point()

# modelo final ------------------------------------------------------------
# ajuste o modelo final com a base inteira (teste + treino).
# use fit().



# predicoes ---------------------------------------------------------------
# coloque uma coluna de predicoes na base original.
# use mutate(), predict()$.pred



# rpart.plot::rpart.plot(...$fit)

# guardar o modelo para usar depois ---------------------------------------
# saveRDS(..., file = "diamonds_final_model.rds")

