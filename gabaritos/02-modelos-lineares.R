# 1. Ajuste um modelo para a variável resposta mpg com variáveis explicativas sendo horsepower, cylinders e acceleration.
# O que aconteceu com o erro quadrático médio?

especificacao_lm <- linear_reg() %>% 
  set_engine("lm")

modelo_linear <- especificacao_lm %>% 
  fit(mpg~horsepower+cylinders+acceleration, data = Auto)

print(modelo_linear)
summary(modelo_linear$fit)

valores_ajustados <- predict(modelo_linear, new_data = Auto)

Auto_com_previsao <- Auto %>% 
  mutate(
    mpg_e = valores_ajustados[[1]]
  )

rmse(Auto_com_previsao, truth = mpg, estimate = mpg_e)
mae(Auto_com_previsao, truth = mpg, estimate = mpg_e)
mape(Auto_com_previsao, truth = mpg, estimate = mpg_e)

# 2. Ajuste um modelo com todas as variáveis. Quais são relevantes para o modelo?
# Qual foi o erro quadrático médio desse modelo?

# Dica: se você quiser usar todas as variáveis, basta adicionar um ponto à direita do til.

modelo_linear <- especificacao_lm %>% 
  fit(mpg~., data = Auto)

print(modelo_linear)
summary(modelo_linear$fit)

valores_ajustados <- predict(modelo_linear, new_data = Auto)

Auto_com_previsao <- Auto %>% 
  mutate(
    mpg_e = valores_ajustados[[1]]
  )

rmse(Auto_com_previsao, truth = mpg, estimate = mpg_e)

modelo_linear2 <- especificacao_lm %>% 
  fit(mpg~., data = select(Auto, -name))

print(modelo_linear2)
summary(modelo_linear2$fit)

valores_ajustados2 <- predict(modelo_linear2, new_data = Auto)

Auto_com_previsao2 <- Auto %>% 
  mutate(
    mpg_e = valores_ajustados2[[1]]
  )

rmse(Auto_com_previsao2, truth = mpg, estimate = mpg_e)
rsq(Auto_com_previsao2, truth = mpg, estimate = mpg_e)
