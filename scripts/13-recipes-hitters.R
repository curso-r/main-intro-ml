# pacotes 
library(tidymodels)
library(DataExplorer)
library(ISLR)
library(skimr)
library(naniar)

# carrega dados (já foi carregado pelo pacote ISLR)
glimpse(Hitters)

# split train/test
set.seed(1)
hitters_initial_split <- initial_split(Hitters)
hitters_train <- training(hitters_initial_split) %>% filter(!is.na(Salary))
hitters_test  <- testing(hitters_initial_split) %>% filter(!is.na(Salary))

# exploracao --------------------------------------------
glimpse(hitters_train)
skim(hitters_train)
skim(hitters_test)

plot_bar(hitters_train)
plot_histogram(hitters_train)
plot_histogram(hitters_train %>% mutate_if(is.numeric, log1p))
plot_qq(hitters_train)
plot_correlation(na.omit(hitters_train), maxcat = 5L)
plot_correlation(na.omit(hitters_train), type = "c")
gg_miss_var(hitters_train)
vis_miss(hitters_train)
gg_miss_case(hitters_train)

# relacao da resposta com as vars numericas
hitters_train %>%
  select_if(is.numeric) %>%
  gather(variavel, valor, -Salary) %>%
  ggplot(aes(x = valor, y = Salary)) +
  geom_point() +
  facet_wrap(~variavel, scales = "free") + 
  geom_smooth(se = FALSE) +
  scale_y_log10()

# relacao da resposta com as vars numericas (escala LOG)
hitters_train %>%
  filter(CHits > 10) %>%
  select(League, CHmRun, CHits, Salary) %>%
  gather(variavel, valor, -Salary, -League) %>%
  ggplot(aes(x = valor, y = Salary)) +
  geom_point() +
  facet_wrap(~variavel, scales = "free") + 
  geom_smooth(se = FALSE) +
  scale_x_log10() +
  scale_y_log10()

# dataprep com recipe -----------------------------------

hitters_sem_dataprep_recipe <- recipe(Salary ~ CHmRun + League, data = hitters_train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_log(all_outcomes()) # estou transformando o Salary em log(Salary)

hitters_recipe <- hitters_sem_dataprep_recipe %>%
  step_log(CHmRun, offset = 1) %>% # log(CHmRun + 1)
  step_normalize(CHmRun) 

juice(prep(hitters_sem_dataprep_recipe))
juice(prep(hitters_recipe))

# modelo ------------------------------------------------
hitters_lr_model <- linear_reg(penalty = tune()) %>%
  set_engine("glmnet")

# workflow -----------------------------------------------
hitters_lr_sem_dataprep_workflow <- workflow() %>% 
  add_recipe(hitters_sem_dataprep_recipe) %>%
  add_model(hitters_lr_model)

hitters_lr_workflow <- hitters_lr_sem_dataprep_workflow %>% 
  update_recipe(hitters_recipe)

# reamostragem -------------------------------------------
set.seed(1)
hitters_resamples <- vfold_cv(hitters_train, v = 5)

# tune grid ----------------------------------------------
hitters_lr_sem_dataprep_tune_grid <- tune_grid(
  hitters_lr_sem_dataprep_workflow,
  hitters_resamples
)

hitters_lr_tune_grid <- hitters_lr_workflow %>%
  tune_grid(hitters_resamples)

autoplot(hitters_lr_tune_grid) + scale_x_log10()

# modelo final -------------------------------------------

meu_fit <- function(tune_grid, model, metric = "roc_auc") {
  credit_best_model <- select_best(tune_grid, metric)
  credit_final_model <- finalize_model(model, credit_best_model)
  
  credit_fit <- fit(
    credit_final_model,
    Status ~ ., 
    data = credit_train
  )
  credit_fit
}

meu_fit <- function(tune_grid, workflow) {
  best <- select_best(tune_grid, metric = "rmse", maximize = FALSE)
  hitters_lr_fit <- workflow %>%
    finalize_workflow(best) %>%
    fit(hitters_train)
}

hitters_lr_sem_dataprep_fit <- meu_fit(hitters_lr_sem_dataprep_tune_grid, hitters_lr_sem_dataprep_workflow)
hitters_lr_fit <- meu_fit(hitters_lr_tune_grid, hitters_lr_workflow)

# predicoes ---------------------------------------------------
meu_predict <- function(fit, model) {
  resposta <- hitters_lr_sem_dataprep_fit %>% 
    extract_recipe() %>% 
    bake(hitters_test) %>%
    pull(Salary)
  
  # base de teste -> aplica transformacao -> faz a predicao (aplica o fit) -> base de dados boa
  predict(fit, hitters_test) %>% 
    mutate(
      modelo = model,
      observado = resposta,
      esperado = .pred
    )
}

hitters_lr_sem_dataprep_preds <- meu_predict(hitters_lr_sem_dataprep_fit, "lr sem dataprep")
hitters_lr_preds <- meu_predict(hitters_lr_fit, "lr")

hitters_preds <- bind_rows(
  hitters_lr_sem_dataprep_preds,
  hitters_lr_preds,
)

# RESULTS
hitters_comparacao_de_modelos <- hitters_preds %>%
  group_by(modelo) %>%
  summarise(
    rmse = rmse_vec(observado, esperado)
  )

# grafico
hitters_preds %>%
  ggplot(aes(x = observado, y = esperado, colour = modelo)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~modelo) +
  geom_abline(intercept = 0, slope = 1) +
  theme_minimal(16)


library(vip)
vip(hitters_lr_sem_dataprep_fit$f$fit$fit %>% class)

