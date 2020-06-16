#install.packages("jpeg")
# Neste exercício vamos aprender a reconstruir imagens usando Machine Learning.
# Suponha que você tenha recebido as seguintes imagens, que podem ser lidas
# pelo R da seguinte forma:

# Note que uma imagem pode ser representada pelo computador por uma superposição
# de 3 matrizes - 1 para cada cor R (Red), G (Green) e B (Blue). 
# Cada elemento dessas matrizes é a intensidade daquela cor na imagem.

url_1 <- "http://curso-r.github.io/posts/assets/fig/xadrez_colorido.jpg"
img_1 <- jpeg::readJPEG(readr::read_file_raw(url_1))

plot(as.raster(img_1))

url_2 <- "http://curso-r.github.io/posts/assets/fig/purple_wave.jpg"
img_2 <- jpeg::readJPEG(readr::read_file_raw(url_2))

plot(as.raster(img_2))

# Agora suponha que para diminuir a transmissão de dados pela rede, a pessoa
# que vai transmitir a imagem da próxima vez não irá enviar a cor B das imagens.
# Então você precisa construir um modelo que recupere o azul das imagens.

# Em termos práticos, considere as seguintes funções:

# - im2df: converte uma imagem em formato de array p/ um data.frame
# - df2im: converte uma imagem no formato data.frame para o formato array

im2df <- function(img) {
  width <- dim(img)[1]
  height <- dim(img)[2]
  tibble::tibble(
    x = rep(1:width, times = width*height/width),
    y = rep(1:height, each = width*height/height),
    r = as.numeric(img[,,1]),
    g = as.numeric(img[,,2]),
    b = as.numeric(img[,,3])
  )
}

df2im <- function(df) {
  width <- max(df$x)
  height <- max(df$y)
  img <- array(dim = c(width, height, 3))
  img[,,1] <- df$r
  img[,,2] <- df$g
  img[,,3] <- df$b
  img
}

plot_img_df <- function(df, col_b) {
  df %>% 
    select(x, y, r, g, b = {{col_b}}) %>% 
    mutate(
      b = ifelse(b < 0, 0, b),
      b = ifelse(b > 1, 1, b)
    ) %>% 
    df2im() %>% 
    as.raster() %>% 
    plot()
}

# Agora vamos usar essas funções para transformar a img_1 em um data.frame

df_1 <- im2df(img_1)


# x     y     r     g     b
# <int> <int> <dbl> <dbl> <dbl>
# 1     1     1 0.290 0.196 0.192
# 2     2     1 0.482 0.302 0.298
# 3     3     1 0.455 0.282 0.278
# 4     4     1 0.459 0.286 0.282
# 5     5     1 0.459 0.286 0.282
# 6     6     1 0.459 0.286 0.282

# Observe que possuimos 5 variáveis na base: `x`, `y`, `r`, `g`, e `b`. 
# x e y indicam a posição do píxel na imagem
# r g e b indicam a intensidade de cada uma das cores nauqela imagem.
# A nossa tarefa será construir um modelo que usa as variáveis x, y , r e g para
# prever a coluna `b` da imagem.

# Por exemplo, você pode construir um modelo de regressão linear para a imagem 1 
# usando:

library(tidymodels)
library(tidyverse)

reg <- linear_reg(penalty = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("lm")

ajuste <- fit(reg, b ~ x + y + r + g, data = df_1)

df_1$b_e <- predict(ajuste, df_1)$.pred

# você pode visualizar a imagem reconstruida usando a funcao plot_img
plot_img_df(df_1, b_e)
plot_img_df(df_1, b)

rmse(df_1, truth = b, estimate = b_e)

# 1) Usando CV encontre o melhor modelo de regressão linear tunando o `lambda`
# da regularização.
# 
# Usando o melhor modelo, compare a imagem original com a imagem reconstruida.
reg <- linear_reg(penalty = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")

reamostragens <- vfold_cv(df_1, v = 5)

tunagem <- tune_grid(
  b ~ x + y + r + g,
  reg,
  resamples = reamostragens
)

collect_metrics(tunagem)
autoplot(tunagem)

melhor_lm <- select_best(tunagem, "rmse", maximize = FALSE)
reg <- finalize_model(reg, melhor_lm)

ajuste_melhor_lm <- fit(
  reg,
  b ~ x + y + r + g,
  data = df_1
)

df_1$b_e_lm <- predict(ajuste_melhor_lm, df_1)$.pred

# plot_img_df(df_1, b_e)
plot_img_df(df_1, b_e_lm)
plot_img_df(df_1, b)
rmse(df_1, b, b_e_lm)

# 2) Usando CV encontre o melhor modelo de árvore de decisão para recuperar a cor
# azul da imagem 1 tunando o hiperparâmetro `min_n`.
#
# Usando o melhor modelo, compare a imagem original com a imagem reconstruida.

# Passo 1: Criar a especificação

especificacao_arvore <- decision_tree(min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

# Passo 2: Escolher o min_n

amostras_cv <- mc_cv(df_1, prop = .8, times = 10)

grid_para_ajuste <- parameters(especificacao_arvore) %>% 
  update(min_n = dials::min_n(range = c(50L, 500L))) %>%
  grid_regular(levels = 10)

grid_de_min_n <- tune_grid(
  b ~ r + g + x + y,
  especificacao_arvore,
  resamples = amostras_cv,
  grid = grid_para_ajuste,
  metrics = metric_set(mae, rmse, rsq)
)

autoplot(grid_de_min_n)

select_best(grid_de_min_n, "rmse", maximize = FALSE)

ajuste_final <- especificacao_arvore %>% 
  update(min_n = 50) %>% 
  fit(b ~ r + g + x + y, data = df_1)

df_1$b_r <- predict(ajuste_final, df_1)$.pred

plot_img_df(df_1, b_r)
plot_img_df(df_1, b)

# 3) Qual dos dois modelos ficou melhor para recuperar a cor azul? Árvore ou regressão?
# Explique a sua resposta mostrando o erro quadrático médio dos melhores modelos.

plot_img_df(df_1, b_r)
plot_img_df(df_1, b)
plot_img_df(df_1, b_e_lm)
rmse(df_1, b, b_e_lm)
rmse(df_1, b, b_r)

# 4) Repita os exercícios 1, 2 e 3 para a imagem 2.

df_1 <- im2df(img_2)

plot_img_df(df_1, b)


##############################
# regressao linear
reg <- linear_reg(penalty = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")

reamostragens <- vfold_cv(df_1, v = 5)

tunagem <- tune_grid(
  b ~ x + y + r + g,
  reg,
  resamples = reamostragens
)

collect_metrics(tunagem)
autoplot(tunagem) + scale_x_log10()

melhor_lm <- select_best(tunagem, "rmse", maximize = FALSE)
reg <- finalize_model(reg, melhor_lm)

ajuste_melhor_lm <- fit(
  reg,
  b ~ x + y + r + g,
  data = df_1
)

df_1$b_e_lm <- predict(ajuste_melhor_lm, df_1)$.pred

# plot_img_df(df_1, b_e)
plot_img_df(df_1, b_e_lm)
plot_img_df(df_1, b)
rmse(df_1, b, b_e_lm)


##############################################
# arvore

especificacao_arvore <- decision_tree(min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

# Passo 2: Escolher o min_n

amostras_cv <- mc_cv(df_1, prop = .8, times = 10)

grid_para_ajuste <- parameters(especificacao_arvore) %>% 
  update(min_n = dials::min_n(range = c(50L, 500L))) %>%
  grid_regular(levels = 10)

grid_de_min_n <- tune_grid(
  b ~ r + g + x + y,
  especificacao_arvore,
  resamples = amostras_cv,
  grid = grid_para_ajuste,
  metrics = metric_set(mae, rmse, rsq)
)

autoplot(grid_de_min_n)

select_best(grid_de_min_n, "rmse", maximize = FALSE)

ajuste_final <- especificacao_arvore %>% 
  update(min_n = 50) %>% 
  fit(b ~ r + g + x + y, data = df_1)

df_1$b_r <- predict(ajuste_final, df_1)$.pred

plot_img_df(df_1, b_r)
plot_img_df(df_1, b)


# 5) Comente o resultado dos modelos quando comparamos as imagens.
# Será que cada modelo tem resultados diferentes por conta da
# estrutura das imagens?
plot_img_df(df_1, b_e_lm)
plot_img_df(df_1, b)
plot_img_df(df_1, b_r)
rmse(df_1, b, b_e_lm)
rmse(df_1, b, b_r)

df_1

GGally::ggpairs(df_1 %>% select(r, g, b))



df_1 <- im2df(img_1)
qplot(r, b, data = df_1 %>% sample_n(1000))


















