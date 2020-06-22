# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(ISLR)

# Me ----------------------------------------------------------------------

glimpse(Auto)

help(Auto)

# Conceito

# Precisamos passar pro R:
# 1. A f que queremos usar
# 2. Aplicar a f para um conjunto de dados

# Passo 1: Especificações de f:

especificacao_lm <- linear_reg() %>%
  set_engine("lm")

# Us ----------------------------------------------------------------------

# Passo 2: Ajuste do modelo

modelo_linear <- especificacao_lm %>% 
  fit(mpg~horsepower, data = Auto)

print(modelo_linear)

# mpg = 39.9459 + -0.1578*(horsepower)

summary(modelo_linear$fit)

# Passo 3: Analisar as previsões

valores_ajustados <- predict(modelo_linear, new_data = Auto)

# podemos usar a função predict para aplicar o modelo em qualquer novo conjunto de dados (new_data)

Auto_com_previsao <- Auto %>% 
  mutate(
    mpg_e = valores_ajustados[[1]]
  )

Auto_para_plot <- Auto_com_previsao %>%
  select(mpg, mpg_e, horsepower) %>% 
  gather(tipo, y, -horsepower)

ggplot() +
  geom_point(aes(horsepower, y), data = filter(Auto_para_plot, tipo != "mpg_e")) +
  geom_line(aes(horsepower, y), data = filter(Auto_para_plot, tipo == "mpg_e"), color = 'red') +
  theme_bw()

library(yardstick)

# Métricas de erro
rmse(Auto_com_previsao, truth = mpg, estimate = mpg_e)
mae(Auto_com_previsao, truth = mpg, estimate = mpg_e)
mape(Auto_com_previsao, truth = mpg, estimate = mpg_e)

# You ---------------------------------------------------------------------

# Exercícios

# 1. Ajuste um modelo para a variável resposta mpg com variáveis explicativas sendo horsepower, cylinders e acceleration.
# O que aconteceu com o erro quadrático médio?
# 2. Ajuste um modelo com todas as variáveis. Quais são relevantes para o modelo?
# Qual foi o erro quadrático médio desse modelo?
# Dica: se você quiser usar todas as variáveis, basta adicionar um ponto à direita do til.