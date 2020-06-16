# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(parsnip)
library(ISLR)

# Me ----------------------------------------------------------------------

# Chegaram novos dados!
# comentário novo

# fiz um outro comentario

dados2 <- readRDS("dados/dados1_2.rds")

plot(dados2)

modelo <- fit(especificacao_arvore, y~x, data = dados1)

valores_esperados <- predict(modelo, new_data = dados2)

novos_dados <- dados2 %>% 
  mutate(
    y_e = valores_esperados[[1]]
  )

dados_plot_2 <- novos_dados %>% 
  gather(tipo, y, -x)

ggplot() +
  geom_point(aes(x, y), data = filter(dados_plot_1, tipo == "y"), color = 'red') +
  geom_step(aes(x, y), data = filter(dados_plot_1, tipo != "y"), color = 'red') +
  theme_bw() +
  geom_point(aes(x, y), data = novos_dados, color = 'blue', size = 2)
  

# Us ----------------------------------------------------------------------

rmse(dados1_com_previsao, truth = y, estimate = y_e)
rmse(novos_dados, truth = y, estimate = y_e)

mape(dados1_com_previsao, truth = y, estimate = y_e)
mape(novos_dados, truth = y, estimate = y_e)

mae(dados1_com_previsao, truth = y, estimate = y_e)
mae(novos_dados, truth = y, estimate = y_e)

# You ---------------------------------------------------------------------

# Exercícios

# Estude o resultado dos modelo que você fez nos exercícios anteriores na base Hitters2

Hitters2 <- readRDS("dados/Hitters2.rds")

# 1. Usando a base Hitters2, crie uma tabela que compara o que você previu usando o modelo do exercício 1 com o que realmente aconteceu.
# 2. Calcule RMSE, MAPE, MAE e MASE das suas previsões para os novos dados. Essas métricas são parecidas com as que você tinha visto antes?
# 3. [Extra] Faça um gráfico comparando as suas predições antes de ver os novos dados e o que realmente aconteceu.