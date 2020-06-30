library(vip)
library(tidyverse)
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
receita <- recipe(y ~ ., data = dados) %>%
  # step_center(all_predictors()) %>%
  # step_scale(all_predictors()) %>%
  step_zv(all_predictors())

# modelos -----------------------------------------------------------------
mod1 <- linear_reg() %>% set_engine("lm")
mod2 <- linear_reg(penalty = 1e15, mixture = 1) %>% set_engine("glmnet")

# workflow ----------------------------------------------------------------
wf1 <- workflow() %>% add_recipe(receita) %>% add_model(mod1)
wf2 <- workflow() %>% add_recipe(receita) %>% add_model(mod2)

# ajustes -----------------------------------------------------------------
fit1 <- fit(wf1, data = dados)
fit2 <- fit(wf2, data = dados)

# variaveis importantes ---------------------------------------------------
vip(fit1$fit$fit)
vip(fit2$fit$fit)








# rascunhao
x <- runif(10)
y <- x

x <- x*100
round(lm(y ~ x)$coef, 2)
