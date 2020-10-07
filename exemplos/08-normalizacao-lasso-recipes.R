library(vip)
library(tidyverse)
library(tidymodels)
set.seed(1)

# dados gerados -----------------------------------------------------------
rows = 1000
cols = 10
x <- as.data.frame(matrix(runif(rows*cols), ncol = cols))

dados <- x %>%
  mutate(
    V1 = V1/100,
    V2 = V2*100,
    y = 1 + 100*V1 + 0.01*V2 + 1*V3 + rnorm(rows)
  )

# receita (dataprep) ------------------------------------------------------
receita_sem_normalizacao <- recipe(y ~ ., data = dados) %>% 
  step_zv(all_predictors())

receita_com_normalizacao <- receita_sem_normalizacao %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

# modelos -----------------------------------------------------------------
mod1 <- linear_reg() %>% set_engine("lm")
mod2 <- linear_reg(penalty = 1e15, mixture = 1) %>% set_engine("glmnet")

# workflow ----------------------------------------------------------------
wf1 <- workflow() %>% add_recipe(receita_sem_normalizacao) %>% add_model(mod1)
wf2 <- workflow() %>% add_recipe(receita_sem_normalizacao) %>% add_model(mod2)
wf3 <- workflow() %>% add_recipe(receita_com_normalizacao) %>% add_model(mod2)

# ajustes -----------------------------------------------------------------
fit1 <- fit(wf1, data = dados)
fit2 <- fit(wf2, data = dados)
fit3 <- fit(wf3, data = dados)

# variaveis importantes ---------------------------------------------------
library(patchwork)

variaveis_lm <- vip(fit1$fit$fit) + ggtitle("Sem LASSO (Pesos corretos!)")
variaveis_glmnet_sem_normalizacao <- vip(fit2$fit$fit) + ggtitle("LASSO Sem Normalizacao")
variaveis_glmnet_com_normalizacao <- vip(fit3$fit$fit) + ggtitle("LASSO Com Normalizacao")

variaveis_lm + variaveis_glmnet_sem_normalizacao + variaveis_glmnet_com_normalizacao








# rascunhao
x <- runif(10)
y <- x

x <- x*100
round(lm(y ~ x)$coef, 2)
