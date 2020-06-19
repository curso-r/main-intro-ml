# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(ISLR)

# Me ----------------------------------------------------------------------

dados1 <- readRDS("dados/dados1.rds")

plot(dados1)

# Conceito

# Precisamos passar pro R:
# 1. A f que queremos usar
# 2. Aplicar a f para um conjunto de dados

# Passo 1: Especificações de f:

especificacao_arvore <- decision_tree(min_n = 2) %>%
  set_engine("rpart") %>% 
  set_mode("regression")

# aqui vamos usar árvore. podemos dizer para o R algumas outras características dessas f.
# neste exemplo, todas as pontas das árvores devem ter no mínimo 2 dados.
# aqui definidmos a implementação do algoritmo que nós vamos usar
# aqui vamos precisar definir qual é o tipo de variável resposta que estamos usando

# Outros exemplos...

# especificacao_regressao <- linear_reg() %>% 
# set_engine("lm") %>% 
# set_mode("regression")

# random_forest <- rand_forest() %>% 
# set_engine("ranger") %>% 
# set_mode("regression)

# Passo 2: Ajuste do modelo

modelo <- especificacao_arvore %>% 
  fit(y ~ x, data = dados1)

print(modelo)

rpart.plot::prp(modelo$fit)

# Passo 3: Analisar as previsões

valores_ajustados <- predict(modelo, new_data = dados1)

dados1_com_previsao <- dados1 %>% 
  mutate(
    y_e = valores_ajustados[[1]]
  )

dados_plot_1 <- dados1_com_previsao %>% 
  gather(tipo, y, -x)

ggplot() +
  geom_point(aes(x, y), data = filter(dados_plot_1, tipo == "y")) +
  geom_step(aes(x, y), data = filter(dados_plot_1, tipo != "y"), color = 'red') +
  theme_bw()

# Como quantificar a qualidade de um modelo?

library(yardstick)

# Métricas de erro
rmse(dados1_com_previsao, truth = y, estimate = y_e)

# residuo = truth-estimate

mape(dados1_com_previsao, truth = y, estimate = y_e)
mae(dados1_com_previsao, truth = y, estimate = y_e)
mase(dados1_com_previsao, truth = y, estimate = y_e)

