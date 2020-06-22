# Pacotes ------------------------------------------------------------------

library(ggplot2)
library(tidymodels)

# Dados -------------------------------------------------------------------
data("diamonds")

# base treino e teste -----------------------------------------------------
set.seed(1)
diamonds_initial_split <- diamonds %>% initial_split(3/4)

diamonds_train <- training(diamonds_initial_split)
diamonds_test <- testing(diamonds_initial_split)

# definicao do modelo -----------------------------------------------------
# passando parâmetros específicos das "engines"
diamonds_model <- decision_tree(
  cost_complexity = 0,
  min_n = tune(), 
  tree_depth = tune()
) %>% 
  set_engine("rpart") %>%
  set_mode("regression")

# reamostragem com cross-validation ---------------------------------------
diamonds_resamples <- vfold_cv(diamonds_train, v = 5)

# grid na mao -------------------------------------------------------------

# ASSIM: parameters() + update() + cost_complexity() + grid_regular()
hiperparams <- parameters(diamonds_model) %>% 
  update(
    min_n = min_n(range = c(2L, 10L)),
    tree_depth = tree_depth(c(2L, 10L))
  ) %>%
  grid_regular(levels = 10)

# ASSIM: parameters() + update() + cost_complexity() + grid_random()
hiperparams <- parameters(diamonds_model) %>% 
  update(
    min_n = min_n(range = c(2L, 5L))
  ) %>%
  grid_random(size = 5)

# OU ASSIM: expand.grid()
hiperparams <- expand.grid(
  min_n = c(5, 50, 100, 500),
  tree_depth = c(2, 6, 10, 14)
)

# tunagem de hiperparametros ----------------------------------------------
# com tune_grid()
diamonds_tune_grid <- tune_grid(
  price ~ x, 
  diamonds_model,
  resamples = diamonds_resamples,
  grid = hiperparams,
  metrics = metric_set(rmse),
  control = control_grid(verbose = TRUE, allow_par = FALSE)
)

# colocando os valores encontrados no modelo ------------------------------
diamonds_best_hiperparams <- select_best(diamonds_tune_grid, "rmse")

# com finalize_model()
diamonds_model <- diamonds_model %>% finalize_model(diamonds_best_hiperparams)

# ou com set_args()
diamonds_model <- diamonds_model %>% 
  set_args(
    cost_complexity = diamonds_best_hiperparams$cost_complexity,
    min_n = 5
  )
