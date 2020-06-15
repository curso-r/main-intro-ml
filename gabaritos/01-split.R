dados1 <- readRDS("dados/dados1.rds")
dados2 <- readRDS("dados/dados1_2.rds")
dados3 <- readRDS("dados/dados1_3.rds")

dados4 <- readRDS("dados/dados1_4.rds")

todos_os_dados <- bind_rows(dados1, dados2, dados3)

# 1. Avalie o modelo 1 e o modelo 2 nos novos dados. Qual deles foi melhor? Varie os valores e min_n e veja se você consegue encontrar um modelo melhor ainda.

especificacao_arvore <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

modelo1 <- especificacao_arvore %>% 
  set_args(min_n = 2, cost_complexity = -10) %>% 
  fit(y~x, data = todos_os_dados)

modelo2 <- especificacao_arvore %>% 
  set_args(min_n = 10, cost_complexity = -10) %>% 
  fit(y~x, data = todos_os_dados)

modelo3 <- especificacao_arvore %>% 
  set_args(min_n = 25, cost_complexity = -10) %>% 
  fit(y~x, data = todos_os_dados)

dados4_com_previsao <- dados4 %>% 
  mutate(
    y_modelo_1 = predict(modelo1, new_data = dados4)[[1]],
    y_modelo_2 = predict(modelo2, new_data = dados4)[[1]],
    y_modelo_3 = predict(modelo3, new_data = dados4)[[1]],
  )

rmse(dados4_com_previsao, truth = y, estimate = y_modelo_1)
rmse(dados4_com_previsao, truth = y, estimate = y_modelo_2)
rmse(dados4_com_previsao, truth = y, estimate = y_modelo_3)

# 2. [Extra] Encontra o melhor min_n da base Hitters separando a base em 3. Ajuste o modelo em um pedaço, procure o melhor min_n na outra e teste na que sobrou.


# Passo 1: Separar em 3 pedaços

separacao_1 <- initial_split(Hitters, prop = .75)

treino <- training(separacao_1)
teste0 <- testing(separacao_1)

separacao_2 <- initial_split(teste0, prop = .5)

teste1 <- training(separacao_2)
teste2 <- testing(separacao_2)

# Estratégia:
# 1. Ajustar na treino
# 2. Procurar min_n na teste1
# 3. Ver se o min_n realmente é bom na teste2

# Passo 1

modelo1 <- especificacao_arvore %>% 
  set_args(min_n = 2, cost_complexity = -10) %>% 
  fit(HmRun~CHmRun, data = treino)

modelo2 <- especificacao_arvore %>% 
  set_args(min_n = 10, cost_complexity = -10) %>% 
  fit(HmRun~CHmRun, data = treino)

modelo3 <- especificacao_arvore %>% 
  set_args(min_n = 25, cost_complexity = -10) %>% 
  fit(HmRun~CHmRun, data = treino)

modelo4 <- especificacao_arvore %>% 
  set_args(min_n = 40, cost_complexity = -10) %>% 
  fit(HmRun~CHmRun, data = treino)

teste_com_previsao <- teste1 %>% 
  select(HmRun, CHmRun) %>% 
  mutate(
    HmRun_modelo1 = predict(modelo1, new_data = teste1)[[1]],
    HmRun_modelo2 = predict(modelo2, new_data = teste1)[[1]],
    HmRun_modelo3 = predict(modelo3, new_data = teste1)[[1]],
    HmRun_modelo4 = predict(modelo4, new_data = teste1)[[1]]
  )

rmse(teste_com_previsao, truth = HmRun, estimate = HmRun_modelo1)
rmse(teste_com_previsao, truth = HmRun, estimate = HmRun_modelo2)
rmse(teste_com_previsao, truth = HmRun, estimate = HmRun_modelo3)
rmse(teste_com_previsao, truth = HmRun, estimate = HmRun_modelo4)

teste2_com_previsao <- teste2 %>% 
  select(HmRun, CHmRun) %>% 
  mutate(
    HmRun_modelo1 = predict(modelo1, new_data = teste2)[[1]],
    HmRun_modelo2 = predict(modelo2, new_data = teste2)[[1]],
    HmRun_modelo3 = predict(modelo3, new_data = teste2)[[1]],
    HmRun_modelo4 = predict(modelo4, new_data = teste2)[[1]]
  )

rmse(teste2_com_previsao, truth = HmRun, estimate = HmRun_modelo1)
rmse(teste2_com_previsao, truth = HmRun, estimate = HmRun_modelo2)
rmse(teste2_com_previsao, truth = HmRun, estimate = HmRun_modelo3)
rmse(teste2_com_previsao, truth = HmRun, estimate = HmRun_modelo4)
