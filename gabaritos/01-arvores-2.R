Hitters1 <- readRDS("dados/Hitters1.rds")

especificacao_arvore <- decision_tree(min_n = 2) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

modelo_hitters <- fit(especificacao_arvore, HmRun ~ CHmRun, data = Hitters1)

# 1. Usando a base Hitters2, crie uma tabela que compara o que você previu usando o modelo do exercício 1 com o que realmente aconteceu.

Hitters2 <- readRDS("dados/Hitters2.rds")

Hitters_d <- Hitters2 %>% 
  select(HmRun,CHmRun)

HmRun_pred_2 <- predict(modelo_hitters, new_data = Hitters_d)

dados_com_previsao_2 <- Hitters_d %>% 
  mutate(
    HmRun_pred_2 = predict(modelo_hitters, new_data = Hitters_d)[[1]]
  )

dados_com_previsao_2 <- Hitters_d %>% 
  mutate(
    HmRun_pred_2 = HmRun_pred_2[[1]]
  )

# 2. Calcule RMSE, MAPE, MAE e MASE das suas previsões para os novos dados. Essas métricas são parecidas com as que você tinha visto antes?

dados_com_previsao <- Hitters1 %>% 
  mutate(
    HmRun_pred_1 = predict(modelo_hitters, new_data = Hitters1)[[1]]
  )

rmse(dados_com_previsao, truth = HmRun, estimate = HmRun_pred_1)
rmse(dados_com_previsao_2, truth = HmRun, estimate = HmRun_pred_2)

mape(filter(dados_com_previsao, HmRun > 0), truth = HmRun, estimate = HmRun_pred_1)
mape(filter(dados_com_previsao_2, HmRun > 0), truth = HmRun, estimate = HmRun_pred_2)

mae(dados_com_previsao, truth = HmRun, estimate = HmRun_pred_1 )
mae(dados_com_previsao_2, truth = HmRun, estimate = HmRun_pred_2 )

mase(dados_com_previsao, truth = HmRun, estimate = HmRun_pred_1 )
mase(dados_com_previsao_2, truth = HmRun, estimate = HmRun_pred_2 )

# 3. [Extra] Faça um gráfico comparando as suas predições antes de ver os novos dados e o que realmente aconteceu.
dados_plot <- dados_com_previsao %>% 
  select(HmRun, CHmRun) %>% 
  gather(tipo, HmRun_1986, -CHmRun)

dados_plot_2 <- dados_com_previsao_2  %>% 
  gather(tipo, HmRun_1986, -CHmRun)

ggplot() +
  geom_point(aes(CHmRun, HmRun_1986), data = filter(dados_plot, tipo == "HmRun")) +
  geom_step(aes(CHmRun, HmRun_1986), data = filter(dados_plot, tipo == "HmRun"), color = 'red') +
  theme_bw()+
  geom_point(aes(CHmRun, HmRun_1986), data = dados_plot_2, color = 'blue', size = 2)
