# Ex 1

especificacao_arvore <- decision_tree(min_n = 5) %>%
  set_engine("rpart") %>% 
  set_mode("regression")

# Ex 2

modelo <- especificacao_arvore %>% 
  fit(HmRun~CHmRun, data = Hitters1)


# Ex 3

Hitters1_com_previsao <- Hitters1 %>% 
  mutate(
    HmRun_pred = predict(modelo, new_data = Hitters1)[[1]]
  )

rmse(Hitters1_com_previsao, truth = HmRun, estimate = HmRun_pred)

# Essa função de cima é equivalente a fazer o seguinte aí embaixo:
hmrun <- Hitters1_com_previsao$HmRun
hmrun_pred <- Hitters1_com_previsao$HmRun_pred

rmse_na_mao <- sqrt(mean((hmrun-hmrun_pred)^2))

mape(Hitters1_com_previsao, truth = y, estimate = y_e)
mae(Hitters1_com_previsao, truth = HmRun, estimate = HmRun_pred)

# Essa função de cima é equivalente a fazer o seguinte aí embaixo:
hmrun <- Hitters1_com_previsao$HmRun
hmrun_pred <- Hitters1_com_previsao$HmRun_pred

mae_na_mao <- mean(abs(hmrun-hmrun_pred))

mase(Hitters1_com_previsao, truth = y, estimate = y_e)

# Ex 4

valores_ajustados <- predict(modelo, new_data = Hitters1)

dados_com_previsao <- Hitters1 %>% 
  select(HmRun,CHmRun) %>% 
  mutate(
    y_e = valores_ajustados[[1]]
  )

dados_plot_1 <- dados_com_previsao %>% 
  gather(tipo, y,-CHmRun)

ggplot() +
  geom_step(aes(CHmRun, y), data = filter(dados_plot_1, tipo == "y_e")) +
  geom_point(aes(CHmRun, y), data = filter(dados_plot_1, tipo != "y_e"),
            color = 'red') +
  theme_bw()
