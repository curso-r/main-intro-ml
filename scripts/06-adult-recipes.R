# pacotes 
library(tidymodels)
library(tidyverse)
library(DataExplorer)
library(skimr)
library(pROC)

#####################################################################################################
# PASSO 0) CARREGAR AS BASES

# Download dos dados -----------------------------------------------------------------------------

# baixa adult.data se nao existe ainda
if(!file.exists("dados/adult.data")) 
  httr::GET("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", httr::write_disk("dados/adult.data"))

# baixa adult.test se nao existe ainda
if(!file.exists("dados/adult.test"))
  httr::GET("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test", httr::write_disk("dados/adult.test"))

# baixa adult.names se nao existe ainda
if(!file.exists("dados/adult.names"))
  httr::GET("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names", httr::write_disk("dados/adult.names"))

# Carrega dados ---------------------------------------------------------------------------------------

# prepara os nomes das colunas para colocar no cabecalho
adult_names <- tibble(name = read_lines("dados/adult.names")) %>%
  filter(
    str_detect(name, "^[^\\|].*:")
  ) %>%
  separate(name, c("name", "description"), sep = ":") %>%
  mutate(
    name = snakecase::to_snake_case(name)
  ) %>%
  add_row(name = "less_than_50k", description = "person earn more than USD 50K per year.")

# treino/teste -------------------------------------------
adult_train <- read_csv(file = "dados/adult.data", na = c("?", "", "NA"), col_names = adult_names$name)
adult_test  <- read_csv(file = "dados/adult.test", na = c("?", "", "NA"), col_names = adult_names$name, skip = 1) %>%
  mutate(
    less_than_50k = if_else(less_than_50k == "<=50K.", "<=50K", ">50K")
  )

adult <- bind_rows(
  adult_train,
  adult_test
)

adult <- adult_train %>% rename(resposta = less_than_50k)
adult_test <- adult_test %>% rename(resposta = less_than_50k)
adult$id <- 1:nrow(adult)
adult_test$id <- nrow(adult) + 1:nrow(adult_test)


submission <- adult_test %>%
  select(id) %>%
  mutate(
  more_than_50k = ifelse(adult_test$resposta == ">50K", 1, 0)
)

adult_test$less_than_50k <- NULL

write_rds(adult_test, path = "adult_val.rds")
write_rds(adult, path = "adult.rds")


# exploracao --------------------------------------------
glimpse(adult_train)
skim(adult_train)
skim(adult_test)

plot_bar(adult_train)
plot_histogram(adult_train)
plot_histogram(adult_train %>% mutate_if(is.numeric, log1p))
plot_qq(adult_train)
plot_qq(adult_train %>% mutate_if(is.numeric, log1p))
plot_correlation(na.omit(adult_train), maxcat = 5L)
plot_correlation(na.omit(adult_train), type = "c")
gg_miss_var(adult_train)
vis_miss(adult_train)
gg_miss_case(adult_train)

# relacao da resposta com as vars numericas
adult_train %>%
  select_if(is.numeric) %>%
  bind_cols(adult_train %>% select(less_than_50k)) %>%
  gather(variavel, valor, -less_than_50k) %>%
  ggplot(aes(y = valor, fill = less_than_50k, x = variavel)) +
  geom_boxplot() +
  facet_wrap(~variavel, scales = "free")

# relacao da resposta com as vars numericas (escala LOG)
adult_train %>%
  select_if(is.numeric) %>%
  bind_cols(adult_train %>% select(less_than_50k)) %>%
  gather(variavel, valor, -less_than_50k) %>%
  ggplot(aes(y = valor + 1, fill = less_than_50k, x = variavel)) +
  geom_boxplot() +
  facet_wrap(~variavel, scales = "free") +
  scale_y_log10()

# dataprep com recipe -----------------------------------
adult_sem_dataprep_recipe <- recipe(adult_train, less_than_50k ~ .) %>%
  step_novel(all_predictors(), -all_numeric()) %>%
  step_unknown(occupation, workclass, native_country) %>%
  step_naomit(less_than_50k)

adult_recipe <- adult_sem_dataprep_recipe %>%
  step_other(all_nominal(), -all_outcomes(), threshold = 0.01) %>%
  step_log(fnlwgt, age) %>%
  step_normalize(fnlwgt, age) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_zv() %>%
  step_corr(all_numeric(), -all_outcomes()) %>%
  step_other(all_nominal(), -all_outcomes(), threshold = 0.05) %>%
  step_dummy(all_nominal())


receita_preparada <- prep(adult_sem_dataprep_recipe)

base_preparada <- juice(prep(adult_sem_dataprep_recipe))
plot_bar(base_preparada)
juice(prep(adult_recipe))

# modelo ------------------------------------------------
adult_tree_model <- decision_tree(cost_complexity = tune(), min_n = 2) %>%
  set_mode("classification") %>%
  set_engine("rpart")

# workflow -----------------------------------------------
adult_tree_sem_dataprep_workflow <- workflow() %>% 
  add_recipe(adult_sem_dataprep_recipe) %>%
  add_model(adult_tree_model) 

adult_tree_workflow <- adult_tree_sem_dataprep_workflow %>% 
  update_recipe(adult_recipe)

# reamostragem -------------------------------------------
set.seed(1)
adult_resamples <- vfold_cv(adult_train, v = 3)
adult_tree_grid <- parameters(adult_tree_sem_dataprep_workflow) %>%
  grid_regular(levels = 5)

# tune grid ----------------------------------------------
adult_tree_sem_dataprep_tune_grid <- adult_tree_sem_dataprep_workflow %>%
  tune_grid(adult_resamples, grid = adult_tree_grid)

adult_tree_tune_grid <- adult_tree_workflow %>%
  tune_grid(adult_resamples, grid = adult_tree_grid)

# modelo final -------------------------------------------
meu_fit <- function(tune_grid, workflow) {
  best <- select_best(tune_grid, metric = "roc_auc")
  adult_tree_fit <- workflow %>%
    finalize_workflow(best) %>%
    fit(adult_train)
}

adult_tree_sem_dataprep_fit <- meu_fit(adult_tree_sem_dataprep_tune_grid, adult_tree_sem_dataprep_workflow)
adult_tree_fit <- meu_fit(adult_tree_tune_grid, adult_tree_workflow)

# predicoes ---------------------------------------------------
meu_predict <- function(fit, model) {
  predict(fit, adult_test, type = "prob") %>% 
    mutate(
      modelo = model,
      resposta = factor(adult_test$less_than_50k)
    )
}

adult_tree_sem_dataprep_preds <- meu_predict(adult_tree_sem_dataprep_fit, "tree sem dataprep")
adult_tree_preds <- meu_predict(adult_tree_fit, "tree")

adult_preds <- bind_rows(
  adult_tree_preds,
  adult_tree_sem_dataprep_preds
) %>%
  mutate(
    pred_prob = `.pred_<=50K`,
    pred_class = factor(if_else(pred_prob > 0.5,  "<=50K", ">50K"))
  )

# RESULTS
adult_comparacao_de_modelos <- adult_preds %>%
  group_by(modelo) %>%
  summarise(
    auc = roc_auc_vec(resposta, pred_prob),
    acc = accuracy_vec(resposta, pred_class),
    prc = precision_vec(resposta, pred_class),
    rec = recall_vec(resposta, pred_class),
    roc = list(roc(resposta, pred_prob))
  ) %>%
  mutate(roc = set_names(roc, modelo))
adult_comparacao_de_modelos


# ROC tipo 1
ggroc(adult_comparacao_de_modelos$roc)

# ROC tipo 2
adult_preds %>% 
  group_by(modelo) %>% 
  roc_curve(resposta, pred_prob) %>% 
  autoplot()

