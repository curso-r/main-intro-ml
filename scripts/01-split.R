# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(parsnip)
library(ISLR)


# Me ----------------------------------------------------------------------

# Como podemos nos proteger?
# 
# dados3 <- tibble(x = seq(0, 2000, length.out = 100)) %>%
# mutate(y = (x^2 - x/3 + 1 + rnorm(n(), sd = mean(x)^2))/100)

#saveRDS(dados3, "dados/dados_1_3.rds")

# Devido aos nossos grandes erros, recebemos mais dados ainda...
dados3 <- readRDS("dados/dados1_3.rds")

todos_os_dados <- bind_rows(dados1, dados2, dados3)

plot(todos_os_dados)

separacao <- initial_split(todos_os_dados, prop = .5)

treino <- training(separacao)
teste <- testing(separacao)
# a ideia é simular dados novos.
# o nosso "teste" vai representar o que vai acontecer quando tivermos dados novos

# Us ----------------------------------------------------------------------

# vamos ajustar no treino e melhorar no teste

modelo1 <- especificacao_arvore %>% 
  set_args(min_n = 2, cost_complexity = -10) %>% 
  fit(y~x, data = treino)

modelo2 <- especificacao_arvore %>% 
  set_args(min_n = 10, cost_complexity = -10) %>% 
  fit(y~x, data = treino)

valores_esperados_1 <- predict(modelo1, new_data = teste)
valores_esperados_2 <- predict(modelo2, new_data = teste)

novos_dados <- teste %>% 
  mutate(
    y_e1 = valores_esperados_1[[1]],
    y_e2 = valores_esperados_2[[1]]
  )

rmse(novos_dados, truth = y, estimate = y_e1)
rmse(novos_dados, truth = y, estimate = y_e2)

mae(novos_dados, truth = y, estimate = y_e1)
mae(novos_dados, truth = y, estimate = y_e2)

mape(novos_dados, truth = y, estimate = y_e1)
mape(novos_dados, truth = y, estimate = y_e2)

# You ---------------------------------------------------------------------

# dados4 <- tibble(x = seq(0, 2000, length.out = 20)) %>%
#   mutate(y = (x^2 - x + 1 + rnorm(n(), sd = mean(x)^2))/100)
# 
# saveRDS(dados4, "dados/dados1_4.rds")

dados4 <- readRDS("dados/dados1_4.rds")

modelo2 <- especificacao_arvore %>% 
  set_args(min_n = 15, cost_complexity = -10) %>% 
  fit(y~x, data = treino)

final <- dados4 %>% 
  mutate(
    y_e1 = predict(modelo1, new_data = dados4)[[1]],
    y_e2 = predict(modelo2, new_data = dados4)[[1]]
  )

rmse(final, truth = y, estimate = y_e1)
rmse(final, truth = y, estimate = y_e2)

mae(final, truth = y, estimate = y_e1)
mae(final, truth = y, estimate = y_e2)

mape(final, truth = y, estimate = y_e1)
mape(final, truth = y, estimate = y_e2)

# 1. Avalia o modelo 1 e o modelo 2 nos novos dados. Qual deles foi melhor? Tente pelo menos outros 4 valores de min_n e veja se você consegue encontrar um modelo melhor ainda.
# 2. [Extra] Encontra um bom min_n da base Hitters separando a base em 3, aplicando initial_split duas vezes. Ajuste o modelo em um pedaço, procure o melhor min_n na outra e teste na que sobrou.
