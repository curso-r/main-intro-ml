# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(caret)
library(recipes)

# Data --------------------------------------------------------------------

diamantes <- ggplot2::diamonds

# retirar linhas que estão erradas.
diamantes <- diamantes %>% 
  mutate(
    depth2 = round(2 * z / (x + y), 3)*100, 
    teste = near(depth, depth2, tol = 1)
  ) %>% 
  filter(teste == TRUE) %>% 
  select(-depth2, -teste)

# transformar variaveis em caracter
diamantes <- diamantes %>% 
  mutate_at(vars(cut, color, clarity), as.character)

id_train <- sample.int(nrow(diamantes), 0.8*nrow(diamantes))
train <- diamantes[id_train,]
valid <- diamantes[-id_train,]

# Receita & Train Control -------------------------------------------------

receita <- recipe(price ~ ., data = train) %>% 
  step_center(all_numeric(), -all_outcomes()) %>% 
  step_scale(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) 

tc <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Modelo linear -----------------------------------------------------------

getModelInfo("glmnet", regex = FALSE)$glmnet$parameters

lasso_1 <- receita %>% 
  train(
    data = train, 
    method = "glmnet", 
    trControl = tc, 
    tuneLength = 5,
    metric = "MAE"
  )

lasso_2 <- receita %>% 
  step_poly(x, y, z, carat) %>% 
  train(
    data = train, 
    method = "glmnet", 
    trControl = tc, 
    tuneLength = 5,
    metric = "MAE"
  )

lasso_3 <- receita %>% 
  step_ns(x, y, z, carat) %>% 
  train(
    data = train, 
    method = "glmnet", 
    trControl = tc, 
    tuneLength = 5,
    metric = "MAE"
  )

resamps <- resamples(
  list(
    LASSO = lasso_1, 
    LASSO_POLY = lasso_2, 
    LASSO_SPLINE = lasso_3
  )
)

dotplot(resamps, metric = "MAE")

saveRDS(lasso_2, "lasso.rds")


# Random Forest -----------------------------------------------------------

getModelInfo("ranger", regex = FALSE)$ranger$parameters

rf <- receita %>% 
  train(
    data = train, 
    method = "ranger", 
    trControl = tc, 
    tuneLength = 3,
    metric = "MAE"
  )

saveRDS(rf, "rf.rds")

resamps <- resamples(
  list(
    LASSO = lasso_1, 
    LASSO_POLY = lasso_2, 
    LASSO_SPLINE = lasso_3,
    RF = rf
  )
)

dotplot(resamps, metric = "MAE")

# XGBoost -----------------------------------------------------------------

getModelInfo("xgbTree", regex = FALSE)$xgbTree$parameters

tc <- trainControl(
  method = "cv", 
  number = 5, 
  verboseIter = TRUE, 
  search = "random"
  )

xgb <- receita %>% 
  train(
    data = train, 
    method = "xgbTree", 
    trControl = tc, 
    tuneLength = 10,
    metric = "MAE"
  )

saveRDS(xgb, "xgb.rds")

resamps <- resamples(
  list(
    LASSO = lasso_1, 
    LASSO_POLY = lasso_2, 
    LASSO_SPLINE = lasso_3,
    RF = rf,
    XGB = xgb
  )
)

dotplot(resamps, metric = "MAE")

# Previsao na base de teste -----------------------------------------------

previsoes <- valid %>% 
  mutate(
    LASSO = predict(lasso_2, valid),
    RF = predict(rf, valid),
    XGB = predict(xgb, valid)
  ) %>% 
  gather(modelo, predito, LASSO, RF, XGB)

previsoes %>% 
  group_by(modelo) %>% 
  summarise(MAE = mean(abs(predito - price)))

previsoes %>% 
  ggplot(aes(x = price, y = predito)) +
  geom_point() +
  facet_wrap(~modelo, ncol = 3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")



