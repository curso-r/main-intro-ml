# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(parsnip)
library(ISLR)

# Me ----------------------------------------------------------------------

dados1 <- readRDS("dados/dados1.rds")

plot(dados1)

especificacao_arvore <- decision_tree(min_n = 2) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

modelo <- especificacao_arvore %>% 
  fit(y~x, data = dados1)

valores_ajustados <- predict(modelo, new_data = dados1)

dados_e_previsao <- dados1 %>% 
  mutate(
    y_e = valores_ajustados[[1]]
  )

dados_plot_1 <- dados_e_previsao %>% 
  gather(tipo, y, -x)

ggplot() +
  geom_point(aes(x, y), data = filter(dados_plot, tipo == "y")) +
  geom_step(aes(x, y), data = filter(dados_plot, tipo != "y"), color = 'red') +
  theme_bw()

# Us ----------------------------------------------------------------------

library(yardstick)

# Métricas de erro
rmse(dados_e_previsao, truth = y, estimate = y_e)
mape(dados_e_previsao, truth = y, estimate = y_e)
mae(dados_e_previsao, truth = y, estimate = y_e)
mase(dados_e_previsao, truth = y, estimate = y_e)

# You ---------------------------------------------------------------------

# Exercícios

# Estude o resultado dos modelo que você fez nos exercícios anteriores na base Hitters2

Hitters2 <- readRDS("dados/Hitters2.rds")

# 1. Usando a base Hitters, ajuste uma árvore de regressão para prever os Home Runs de cada jogador em 1986,
# usando como variável explicativa o número de Home Runs que ele fez na vida.
# 2. Calcule RMSE, MAPE, MAE e MASE do modelo que você ajustou.
# 3. [Extra] Faça um gráfico comparando as suas predições e o que realmente aconteceu.