# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(ISLR)
library(tidyverse)
library(modeldata)
library(pROC)
library(vip)
library(skimr)
library(naniar)

# PASSO 0) CARREGAR AS BASES -----------------------------------------------
adult <- read_rds("dados/adult.rds")
help(adult)
glimpse(adult) # German Risk 

adult %>% count(resposta)

# PASSO 1) BASE TREINO/TESTE -----------------------------------------------
set.seed(1)
adult_initial_split <- initial_split(adult, strata = "resposta", p = 0.75)

adult_train <- training(adult_initial_split)
adult_test  <- testing(adult_initial_split)

adult_test %>% count(resposta)
adult_train %>% count(resposta)

# PASSO 2) EXPLORAR A BASE -------------------------------------------------

vis_miss(adult)
skim(adult)
GGally::ggpairs(adult_train %>% select(where(is.numeric)) %>% mutate_all(log))
adult %>%
  filter(Assets > 100) %>%
  select(where(is.numeric), Status, Records) %>%
  pivot_longer(where(is.numeric)) %>%
  ggplot(aes(x = Records, y = value, fill = Status)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free_y") +
  scale_y_log10()
# 
GGally::ggpairs(adult %>% select(where(~!is.numeric(.))))

# PASSO 3) DATAPREP --------------------------------------------------------
adult_receita <- recipe(resposta ~ ., data = adult_train) %>%
  step_modeimpute(Home, Marital, Job) %>%
  step_medianimpute(Debt) %>%
  step_bagimpute(Income, Assets) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_interact(~ starts_with("Seniority"):starts_with("Records")) %>%
  step_interact(~ starts_with("Amount"):starts_with("Records")) 


ok <-juice(prep(adult_receita))

# PASSO 4) MODELO ----------------------------------------------------------
# Definição de 
# a) a f(x): logistc_reg()
# b) modo (natureza da var resp): classification
# c) hiperparametros que queremos tunar: penalty = tune()
# d) hiperparametros que não queremos tunar: mixture = 1 # LASSO
# e) o motor que queremos usar: glmnet
adult_lr_model <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")


# workflow ----------------------------------------------------------------
adult_wf <- workflow() %>%
  add_model(adult_lr_model) %>%
  add_recipe(adult_receita)


# PASSO 5) TUNAGEM DE HIPERPARÂMETROS --------------------------------------
# a) bases de reamostragem para validação: vfold_cv()
# b) (opcional) grade de parâmetros: parameters() %>% update() %>% grid_regular()
# c) tune_grid()
# d) escolha das métricas (rmse, roc_auc, etc)
# d) collect_metrics() ou autoplot() para ver o resultado
adult_resamples <- vfold_cv(adult_train, v = 5)

adult_lr_tune_grid <- tune_grid(
  adult_wf,
  resamples = adult_resamples,
  metrics = metric_set(
    accuracy, 
    kap, # KAPPA 
    roc_auc, 
    precision, 
    recall, 
    f_meas, 
    mn_log_loss #binary cross entropy
  )
)

# minha versão do autoplot()
collect_metrics(adult_lr_tune_grid)

collect_metrics(adult_lr_tune_grid) %>%
  filter(penalty < 00.01) %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
  facet_wrap(~.metric, scales = "free") +
  scale_x_log10()

# PASSO 6) DESEMPENHO DO MODELO FINAL ------------------------------------------
# a) extrai melhor modelo com select_best()
# b) finaliza o modelo inicial com finalize_model()
# c) ajusta o modelo final com todos os dados de treino (bases de validação já era)
adult_lr_best_params <- select_best(adult_lr_tune_grid, "roc_auc")
adult_wf <- adult_wf %>% finalize_workflow(adult_lr_best_params)

adult_lr_last_fit <- last_fit(
  adult_wf,
  adult_initial_split
)

# Variáveis importantes
adult_lr_last_fit_model <- adult_lr_last_fit$.workflow[[1]]$fit$fit
vip(adult_lr_last_fit_model)

# PASSO 7) GUARDA TUDO ---------------------------------------------------------
write_rds(adult_lr_last_fit, "adult_lr_last_fit.rds")
write_rds(adult_lr_last_fit_model, "adult_lr_last_fit_model.rds")

collect_metrics(adult_lr_last_fit)
adult_test_preds <- collect_predictions(adult_lr_last_fit)
# roc
adult_roc_curve <- adult_test_preds %>% roc_curve(Status, .pred_bad)
autoplot(adult_roc_curve)

# confusion matrix
adult_test_preds %>%
  mutate(
    Status_class = factor(if_else(.pred_bad > 0.6, "bad", "good"))
  ) %>%
  conf_mat(Status, Status_class)

# gráficos extras!

# risco por faixa de score
adult_test_preds %>%
  mutate(
    score =  factor(ntile(.pred_bad, 10))
  ) %>%
  count(score, Status) %>%
  ggplot(aes(x = score, y = n, fill = Status)) +
  geom_col(position = "fill") +
  geom_label(aes(label = n), position = "fill") +
  coord_flip()

# gráfico sobre os da classe "bad"
percentis = 20
adult_test_preds %>%
  mutate(
    score = factor(ntile(.pred_bad, percentis))
  ) %>%
  filter(Status == "bad") %>%
  group_by(score) %>%
  summarise(
    n = n(),
    media = mean(.pred_bad)
  ) %>%
  mutate(p = n/sum(n)) %>%
  ggplot(aes(x = p, y = score)) +
  geom_col() +
  geom_label(aes(label = scales::percent(p))) +
  geom_vline(xintercept = 1/percentis, colour = "red", linetype = "dashed", size = 1)

# PASSO 7) MODELO FINAL -----------------------------------------------------
adult_final_lr_model <- adult_lr_model %>% fit(Status ~ ., adult)
write_rds(adult_final_lr_model, "adult_final_lr_model.rds")




# EXTRA - KS ##############################################
# https://pt.wikipedia.org/wiki/Teste_Kolmogorov-Smirnov

ks_vec <- function(truth, estimate) {
  truth_lvls <- unique(truth)
  ks_test <- suppressWarnings(ks.test(estimate[truth %in% truth_lvls[1]], estimate[truth %in% truth_lvls[2]]))
  ks_test$statistic
}

comparacao_de_modelos <- collect_predictions(adult_lr_last_fit) %>%
  summarise(
    auc = roc_auc_vec(Status, .pred_bad),
    acc = accuracy_vec(Status, .pred_class),
    prc = precision_vec(Status, .pred_class),
    rec = recall_vec(Status, .pred_class),
    ks = ks_vec(Status, .pred_bad),
    roc = list(roc(Status, .pred_bad))
  ) 

# KS no ggplot2 -------
densidade_acumulada <- adult_test_preds %>%
  ggplot(aes(x = .pred_bad, colour = Status)) +
  stat_ecdf(size = 1) +
  theme_minimal()  +
  labs(title = "Densidade Acumulada")

densidade <- adult_test_preds %>%
  ggplot(aes(x = .pred_bad, colour = Status)) +
  stat_density(size = 0.5, aes(fill = Status), alpha = 0.2 , position = "identity") +
  theme_minimal() +
  labs(title = "Densidade")

library(patchwork)
densidade / densidade_acumulada

# KS "na raça" ---------
ks_na_raca_df <- collect_predictions(adult_lr_last_fit) %>%
  mutate(modelo = "Regressao Logistica",
         pred_prob = .pred_bad) %>%
  mutate(score_categ = cut_interval(pred_prob, 1000)) %>%
  arrange(modelo, score_categ, Status) %>%
  group_by(modelo, Status, score_categ) %>%
  summarise(
    n = n(),
    pred_prob_mean = mean(pred_prob)
  ) %>%
  mutate(
    ecdf = cumsum(n)/sum(n)
  )

ks_na_raca_df %>%
  ggplot(aes(x = pred_prob_mean, y = ecdf, linetype = Status, colour = modelo)) +
  geom_line(size = 1) +
  theme_minimal() 

# descobrindo onde acontece o máximo ------------
ks_na_raca_onde <- ks_na_raca_df %>%
  select(-n, -score_categ) %>%
  ungroup() %>%
  complete(modelo, Status, pred_prob_mean) %>%
  fill(ecdf) %>%
  spread(Status, ecdf) %>%
  group_by(modelo) %>%
  na.omit() %>%
  summarise(
    ks = max(abs(bad- good)),
    ks_onde = which.max(abs(bad- good)),
    pred_prob_mean_onde = pred_prob_mean[ks_onde],
    y_max = bad[ks_onde],
    y_min = good[ks_onde]
  )

ks_na_raca_df %>%
  ggplot(aes(x = pred_prob_mean, y = ecdf, colour = modelo)) +
  geom_line(size = 1, aes(linetype = Status)) +
  geom_segment(data = ks_na_raca_onde, aes(x = pred_prob_mean_onde, xend = pred_prob_mean_onde, y = y_max, yend = y_min), size = 2, arrow = arrow(ends = "both")) +
  theme_minimal() 



# ignorar
vip_ok <- vi(adult_lr_last_fit_model) %>%
  mutate(Variable = fct_reorder(Variable, abs(Importance))) %>%
  ggplot(aes(x = abs(Importance), y = Variable, fill = Sign)) +
  geom_col()

write_rds(vip_ok, "vip_ok.rds")

