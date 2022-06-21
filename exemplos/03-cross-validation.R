# Pacotes ------------------------------------------------------------------

library(ggplot2)
library(tidymodels)
library(ISLR2)

# Dados -------------------------------------------------------------------
data("Hitters")
#Hitters <- na.omit(Hitters)

# base treino e teste -----------------------------------------------------
set.seed(123)
hitters_initial_split <- Hitters %>% initial_split(3/4)

hitters_train <- training(hitters_initial_split)
hitters_test <- testing(hitters_initial_split)

# Dataprep ----------------------------------------------------------------

hitters_recipe <- recipe(Salary ~ ., data = hitters_train) %>%
  step_naomit(everything(), skip = TRUE) %>%
  step_rm(all_nominal()) %>%
  step_normalize(all_numeric_predictors())

# dar uma olhada no resultado da receita.
rec_prep <- hitters_recipe %>%
  prep()

rec_prep %>%
  bake(new_data = NULL)

juice(rec_prep) %>% glimpse()


# definicao do modelo -----------------------------------------------------

# OBS: repare que agora colocamos "tune()" nos hiperparâmetros para os quais
# queremos encontrar o melhor valor.
hitters_model <- linear_reg(
  penalty = tune()
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Criando o workflow ------------------------------------------------------

hitters_wflow <- workflow() %>%
  add_recipe(hitters_recipe) %>%
  add_model(hitters_model)

# tunagem de hiperparametros ----------------------------------------------

# reamostragem com cross-validation ---------------------------------------
hitters_resamples <- vfold_cv(hitters_train, v = 10)

hitters_grid <- grid_regular(
  penalty(c(-1, 2)),
  levels = 10
)
#hitters_grid <- tibble(penalty = seq(0.1, 2, length.out = 20))

hitters_tune_grid <- tune_grid(
  hitters_wflow,
  resamples = hitters_resamples,
  grid = hitters_grid,
  metrics = metric_set(rmse, rsq),
  control = control_grid(verbose = TRUE, allow_par = FALSE)
)

# inspecao da tunagem -----------------------------------------------------
# hitters_tune_grid$.metrics[[3]]
autoplot(hitters_tune_grid)
collect_metrics(hitters_tune_grid) %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean - 1.9*std_err, ymax = mean + 1.9*std_err)) +
  facet_wrap(~.metric, scales = "free_y") +
  scale_x_log10()


show_best(hitters_tune_grid, n = 1, metric = "rmse")

# seleciona o melhor conjunto de hiperparametros
hitters_best_hiperparams <- select_best(hitters_tune_grid, "rmse")

hitters_wflow <- hitters_wflow %>% 
  finalize_workflow(hitters_best_hiperparams)

# desempenho do modelo final ----------------------------------------------

hitters_model_train <- hitters_wflow %>% 
  fit(data = hitters_train)

pred <- predict(hitters_model_train, hitters_test)
bind_cols(pred, hitters_test) %>% rmse(truth = Salary, estimate = .pred)

pred <- predict(hitters_model_train, hitters_train)
bind_cols(pred, hitters_train) %>% rmse(truth = Salary, estimate = .pred)


hitters_last_fit <- hitters_wflow %>%
  last_fit(split = hitters_initial_split)

collect_metrics(hitters_last_fit)
collect_predictions(hitters_last_fit) %>%
  ggplot(aes(.pred, Salary)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# modelo final ------------------------------------------------------------

hitters_final_model <- hitters_wflow %>% fit(data = Hitters)

# predicoes ---------------------------------------------------------------

hitters_com_previsao <- Hitters %>%
  mutate(
    salary_pred = predict(hitters_final_model, new_data = .)$.pred
  )

atleta_novo <- tibble(
  AtBat = 293L, 
  Hits = 100L, 
  HmRun = 1L, 
  Runs = 30L, 
  RBI = 29L, 
  Walks = 14L, 
  Years = 1L, 
  CAtBat = 293L, 
  CHits = 66L, 
  CHmRun = 1L, 
  CRuns = 30L, 
  CRBI = 29L, 
  CWalks = 14L, 
  PutOuts = 446L, 
  Assists = 33L, 
  Errors = 50L,
  League = NA, 
  Division = NA, 
  NewLeague = NA
  )

predict(hitters_final_model, new_data = atleta_novo)

hitters_final_model %>%
  extract_fit_engine() %>%
  coef(s = 21.5)

# guardar o modelo para usar depois ---------------------------------------
saveRDS(hitters_final_model, file = "hitters_final_model.rds")

modelo <- readRDS("hitters_final_model.rds")
predict(modelo, Hitters)
