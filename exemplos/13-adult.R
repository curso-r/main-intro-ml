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
httr::GET("https://github.com/curso-r/main-intro-ml/raw/master/dados/adult.rds", httr::write_disk("adult.rds", overwrite = TRUE))
adult <- read_rds("adult.rds")
help(adult)
glimpse(adult) # German Risk 

adult %>% count(resposta)

# PASSO 1) BASE TREINO/TESTE -----------------------------------------------
set.seed(1)
adult_initial_split <- initial_split(adult, strata = "resposta", p = 0.75)

adult_train <- training(adult_initial_split)
adult_test  <- testing(adult_initial_split)

# PASSO 2) EXPLORAR A BASE -------------------------------------------------

prep(adult_recipe)
glimpse(juice(prep(adult_recipe)))
# vis_miss(adult)
# skim(adult)
# GGally::ggpairs(adult_train %>% select(where(is.numeric)) %>% mutate_all(log))
# adult %>% 
#   filter(Assets > 100) %>%
#   select(where(is.numeric), resposta, Records) %>%
#   pivot_longer(where(is.numeric)) %>%
#   ggplot(aes(x = Records, y = value, fill = resposta)) +
#   geom_boxplot() +
#   facet_wrap(~name, scales = "free_y") +
#   scale_y_log10()
# 
# GGally::ggpairs(adult %>% select(where(~!is.numeric(.))))

# PASSO 3) DATAPREP --------------------------------------------------------
adult_receita <- recipe(resposta ~ ., data = adult_train) %>%
  step_modeimpute(workclass, occupation, native_country) %>%
  step_zv(all_predictors()) %>%
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes())


# juice(prep(adult_receita))

# PASSO 4) MODELO ----------------------------------------------------------
# Definição de 
# a) a f(x): logistc_reg() 
# b) modo (natureza da var resp): classification/regression
# c) hiperparametros para tunar: penalty = tune()
# d) hiperparametros para não tunar: mixture = 1 # LASSO
# e) o motor: glmnet
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
  metrics = metric_set(roc_auc)
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

# metricas
collect_metrics(adult_lr_last_fit)

# roc
adult_test_preds <- collect_predictions(adult_lr_last_fit)
adult_roc_curve <- adult_test_preds %>% roc_curve(resposta, `.pred_<=50K`)
autoplot(adult_roc_curve)

# Variáveis importantes
adult_lr_last_fit_model <- adult_lr_last_fit$.workflow[[1]]$fit$fit
vip(adult_lr_last_fit_model)


# confusion matrix
adult_test_preds %>%
  mutate(
    resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
  ) %>%
  conf_mat(resposta, resposta_class)


# PASSO 9: MODELO FINAL ------------------------------------------------------------
adult_modelo_final <- adult_wf %>% fit(adult)

# PASSO 8: ESCORA BASE DE VALIDACAO ------------------------------------------------
# PASSO 0) CARREGAR AS BASES -----------------------------------------------
httr::GET("https://github.com/curso-r/main-intro-ml/raw/master/dados/adult_val.rds", httr::write_disk("adult_val.rds", overwrite = TRUE))
adult_val <- read_rds("adult_val.rds")


adult_val_sumbissao <- adult_val %>%
  mutate(
    more_than_50k = predict(adult_modelo_final, new_data = adult_val, type = "prob")$`.pred_>50K`
  ) %>%
  select(id, more_than_50k)

write_csv(adult_val_sumbissao, "adult_val_sumbissao.csv")






bd # 100

bd_mais_predicao # 101










# risco por faixa de score ---------------------------------------------------------
adult_test_preds %>%
  mutate(
    score =  factor(ntile(`.pred_<=50K`, 10))
  ) %>%
  count(score, resposta) %>%
  ggplot(aes(x = score, y = n, fill = resposta)) +
  geom_col(position = "fill") +
  geom_label(aes(label = n), position = "fill") +
  coord_flip()

# gráfico sobre os da classe "<=50K"
percentis = 20
adult_test_preds %>%
  mutate(
    score = factor(ntile(`.pred_<=50K`, percentis))
  ) %>%
  filter(resposta == "<=50K") %>%
  group_by(score) %>%
  summarise(
    n = n(),
    media = mean(`.pred_<=50K`)
  ) %>%
  mutate(p = n/sum(n)) %>%
  ggplot(aes(x = p, y = score)) +
  geom_col() +
  geom_label(aes(label = scales::percent(p))) +
  geom_vline(xintercept = 1/percentis, colour = "red", linetype = "dashed", size = 1)



# EXTRA - KS ##############################################
# https://pt.wikipedia.org/wiki/Teste_Kolmogorov-Smirnov

ks_vec <- function(truth, estimate) {
  truth_lvls <- unique(truth)
  ks_test <- suppressWarnings(ks.test(estimate[truth %in% truth_lvls[1]], estimate[truth %in% truth_lvls[2]]))
  ks_test$statistic
}

comparacao_de_modelos <- collect_predictions(adult_lr_last_fit) %>%
  summarise(
    auc = roc_auc_vec(resposta, `.pred_<=50K`),
    acc = accuracy_vec(resposta, .pred_class),
    prc = precision_vec(resposta, .pred_class),
    rec = recall_vec(resposta, .pred_class),
    ks = ks_vec(resposta, `.pred_<=50K`),
    roc = list(roc(resposta, `.pred_<=50K`))
  ) 

# KS no ggplot2 -------
densidade_acumulada <- adult_test_preds %>%
  ggplot(aes(x = `.pred_<=50K`, colour = resposta)) +
  stat_ecdf(size = 1) +
  theme_minimal()  +
  labs(title = "Densidade Acumulada")

densidade <- adult_test_preds %>%
  ggplot(aes(x = `.pred_<=50K`, colour = resposta)) +
  stat_density(size = 0.5, aes(fill = resposta), alpha = 0.2 , position = "identity") +
  theme_minimal() +
  labs(title = "Densidade")

library(patchwork)
densidade / densidade_acumulada

# KS "na raça" ---------
ks_na_raca_df <- collect_predictions(adult_lr_last_fit) %>%
  mutate(modelo = "Regressao Logistica",
         pred_prob = `.pred_<=50K`) %>%
  mutate(score_categ = cut_interval(pred_prob, 1000)) %>%
  arrange(modelo, score_categ, resposta) %>%
  group_by(modelo, resposta, score_categ) %>%
  summarise(
    n = n(),
    pred_prob_mean = mean(pred_prob)
  ) %>%
  mutate(
    ecdf = cumsum(n)/sum(n)
  )

ks_na_raca_df %>%
  ggplot(aes(x = pred_prob_mean, y = ecdf, linetype = resposta, colour = modelo)) +
  geom_line(size = 1) +
  theme_minimal() 

# descobrindo onde acontece o máximo ------------
ks_na_raca_onde <- ks_na_raca_df %>%
  select(-n, -score_categ) %>%
  ungroup() %>%
  complete(modelo, resposta, pred_prob_mean) %>%
  fill(ecdf) %>%
  spread(resposta, ecdf) %>%
  group_by(modelo) %>%
  na.omit() %>%
  summarise(
    ks = max(abs(`<=50K`- `>50K`)),
    ks_onde = which.max(abs(`<=50K`- `>50K`)),
    pred_prob_mean_onde = pred_prob_mean[ks_onde],
    y_max = `<=50K`[ks_onde],
    y_min = `>50K`[ks_onde]
  )

ks_na_raca_df %>%
  ggplot(aes(x = pred_prob_mean, y = ecdf, colour = modelo)) +
  geom_line(size = 1, aes(linetype = resposta)) +
  geom_segment(data = ks_na_raca_onde, aes(x = pred_prob_mean_onde, xend = pred_prob_mean_onde, y = y_max, yend = y_min), size = 2, arrow = arrow(ends = "both")) +
  theme_minimal() 



