# Tarefa 02 - Classificação com Regressão Logística e Modelos de árvore
# ANÁLISE DA BASE ADULTS

# OBJETIVO PRINCIPAL: CRIAR UM CÓDIGO QUE GERE 2 OU MAIS TIPOS DE MODELOS (EX: REGRESSÃO LINEAR E RANDOM FOREST)

# BONUS: foi colocada uma etapa de "exploração" dos dados que será pauta da próxima aula.

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

# treino/teste 
adult_train <- read_csv(file = "dados/adult.data", na = c("?", "", "NA"), col_names = adult_names$name)
adult_test  <- read_csv(file = "dados/adult.test", na = c("?", "", "NA"), col_names = adult_names$name, skip = 1) %>%
  mutate(
    less_than_50k = if_else(less_than_50k == "<=50K.", "<=50K", ">50K")
  )

#####################################################################################################
# PASSO 1) BASE TREINO/TESTE

#####################################################################################################
# PASSO 2) EXPLORAR A BASE
# PASSO 3) DATAPREP

# map(list(test = adult_test, train = adult_train), introduce) %>% enframe() %>% unnest() %>% t %>% as_tibble() %>% set_names(c("test", "train"))
# skimr::skim(adult_train)
# skimr::skim(adult_test)
# 
# plot_bar(adult_train)
# plot_histogram(adult_train)
# plot_histogram(adult_train %>% mutate_if(is.numeric, log1p))
# plot_qq(adult_train)
# plot_qq(adult_train %>% mutate_if(is.numeric, log1p))
# plot_correlation(na.omit(adult_train), maxcat = 5L)
# plot_correlation(na.omit(adult_train), type = "d")
# gg_miss_var(adult_train)
# vis_miss(adult_train)
# gg_miss_case(adult_train)


# NESSA PARTE REMOVE-SE O NA APENAS PARA SEGUIR MAIS SUAVE O EXERCÍCIO, MAS NA PRÓXIMA AULA 
# IREMOS TRATAR ESSE PROBLEMA DE MANEIRA MAIS ADEQUADA!
adult_train <- adult_train %>% na.omit() %>% mutate_if(is.character, as.factor)
adult_test <- adult_test %>% na.omit() %>% mutate_if(is.character, as.factor)

#####################################################################################################
# PASSO 4) MODELO
# Definição de 
# a) a f(x) (ou do modelo): logistc_reg(), etc..
# b) modo (ou natureza da var resp): classification
# c) hiperparametros que queremos tunar tune()
# d) hiperparametros que não queremos tunar
# e) o motor que queremos usar

adult_lr_model <- logistic_reg(penalty = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

adult_tree_model <- decision_tree(cost_complexity = tune(), min_n = 2) %>%
  set_mode("classification") %>%
  set_engine("rpart")

#####################################################################################################
# PASSO 5) TUNAGEM DE HIPERPARÂMETROS
# a) bases de reamostragem para validação: vfold_cv()
# b) (opcional) grade de parâmetros: parameters() %>% update() %>% grid_regular()
# c) tune_grid(y ~ x + ...)
# d) collect_metrics() ou autoplot() para ver o resultado

set.seed(1)
adult_resamples <- vfold_cv(adult_train, v = 5)

library(tictoc)

meu_autoplot <- function(tune_grid) {
  collect_metrics(tune_grid) %>%
    gather(hiperparametro_nome, hiperparametro_valor, -(.metric:std_err)) %>%
    ggplot(aes(x = hiperparametro_valor, y = mean)) +
    geom_point() +
    geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
    facet_wrap(hiperparametro_nome~.metric, scales = "free") 
}

# lr ------------------------------------------------------
tic("lr") # demora 20 segundos
adult_lr_tune_grid <- tune_grid(
  less_than_50k ~ .,
  adult_lr_model,
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
toc()

autoplot(adult_lr_tune_grid) + scale_x_log10()
meu_autoplot(adult_lr_tune_grid) + scale_x_log10()

# tree ------------------------------------------------------
tic("tree") # demora 5 minutos
adult_tree_parameters <- parameters(adult_tree_model) %>%
  update(cost_complexity = cost_complexity(c(-2, -0.1))) %>%
  grid_regular(levels = 10)

adult_tree_tune_grid <- tune_grid(
  less_than_50k ~ .,
  adult_tree_model,
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
toc()

autoplot(adult_tree_tune_grid) + scale_x_log10()
meu_autoplot(adult_tree_tune_grid) + scale_x_log10()

#####################################################################################################
# PASSO 6) MODELO FINAL
# a) extrai melhor modelo com select_best()
# b) finaliza o modelo inicial com finalize_model()
# c) ajusta o modelo final com todos os dados de treino (bases de validação já era)
meu_fit <- function(tune_grid, model, metric = "roc_auc") {
  adult_best_model <- select_best(tune_grid, metric)
  adult_final_model <- finalize_model(model, adult_best_model)
  
  adult_fit <- fit(
    adult_final_model,
    less_than_50k ~ ., 
    data = adult_train
  )
  adult_fit
}

adult_lr_fit <- meu_fit(adult_lr_tune_grid, adult_lr_model)
adult_tree_fit <- meu_fit(adult_tree_tune_grid, adult_tree_model)

#####################################################################################################
# PASSO 7) PREDIÇÕES
# a) escora a base de teste
# b) calcula métricas de erro
# c) usa o modelo para tomar decisões
# d) curva ROC

adult_lr_preds <- predict(adult_lr_fit, adult_test, type = "prob") %>% 
  mutate(
    modelo = "lr",
    less_than_50k = factor(adult_test$less_than_50k)
  )

adult_tree_preds <- predict(adult_tree_fit, adult_test, type = "prob") %>% 
  mutate(
    modelo = "tree",
    less_than_50k = factor(adult_test$less_than_50k)
  )

adult_preds <- bind_rows(
  adult_tree_preds,
  adult_lr_preds,
) %>%
  mutate(
    pred_prob = `.pred_>50K`,
    pred_class = factor(if_else(pred_prob > 0.5,  ">50K", "<=50K"))
  ) 

# RESULTS
adult_comparacao_de_modelos <- adult_preds %>%
  group_by(modelo) %>%
  summarise(
    auc = roc_auc_vec(less_than_50k, pred_prob),
    acc = accuracy_vec(less_than_50k, pred_class),
    prc = precision_vec(less_than_50k, pred_class),
    rec = recall_vec(less_than_50k, pred_class),
    roc = list(roc(less_than_50k, pred_prob))
  ) %>%
  mutate(roc = set_names(roc, modelo))

# ROC tipo 1
ggroc(adult_comparacao_de_modelos$roc)

# ROC tipo 2
adult_preds %>% 
  group_by(modelo) %>% 
  roc_curve(less_than_50k, pred_prob) %>% 
  autoplot()
