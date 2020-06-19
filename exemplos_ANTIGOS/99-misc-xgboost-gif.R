library(tidyverse)
library(tidymodels)
library(gganimate)
theme_set(theme_minimal(20))

set.seed(1)
n <- 300
dados <- tibble(
  x = runif(n) - 0.5,
  y = sin(x * pi * 2) + rnorm(n, sd = 0.3)
)


gera_gif <- function(trees = 100, min_n = 1, 
                     tree_depth = 4, learn_rate = 0.1, 
                     sample_size = 1, loss_reduction = 0.1,
                     gif_name = glue::glue("slides/img/xgb_trees{trees}@min_n{min_n}@tree_depth{tree_depth}@learn_rate{learn_rate}@sample_size{sample_size}@loss_reduction{loss_reduction}.gif")) {
  
  modelo <- boost_tree(
    mode = "regression", 
    mtry = 1, 
    trees = trees, 
    min_n = min_n, 
    tree_depth = tree_depth, 
    learn_rate = learn_rate, 
    sample_size = sample_size, 
    loss_reduction = loss_reduction
  ) %>%
    set_engine("xgboost", base_score = mean(dados$y))
  
  
  ajuste <- fit(modelo, y ~ x, data = dados)
  
  # xgboost::xgb.plot.tree(model = ajuste$fit, trees = 1)
  # 
  # xgboost::xgb.plot.tree(model = ajuste$fit)
  # xgboost::xgb.model.dt.tree(model = ajuste$fit, trees = 0)
  
  dados_xgb <- dados %>% select(x) %>% as.matrix()
  
  predict_xgb <- function(step) {
    dados %>%
      mutate(
        pred = xgboost:::predict.xgb.Booster(ajuste$fit, newdata = dados_xgb, ntreelimit = step)
      )
  }
  
  df <- tibble(
    step = 1:trees,
    preds = map(step - 1, predict_xgb)
  ) %>%
    unnest(preds) %>%
    mutate(step = step)
  
  gif <- df %>%
    ggplot(aes(x = x)) +
    geom_point(aes(y = y), size = 2, alpha = 0.4) +
    stat_function(fun = ~sin(. * pi * 2), colour = "purple", size = 1.5) +
    geom_step(aes(y = pred), colour = "orange", size = 2) +
    transition_time(step) + 
    labs(title = 'Step: {frame_time}') +
    ease_aes("linear")
  
  gif <- animate(gif, nframes = trees +15, 5, end_pause = 15, height = 280, width =300*(1+sqrt(5))/2)
  anim_save(gif_name, gif)
  gif
}



# tudo 1
gera_gif( 
  trees = 100, 
  min_n = 1, 
  tree_depth = 1, 
  learn_rate = 1, 
  sample_size = 0.5, 
  loss_reduction = 0
)

# tree_depth = 2
gera_gif( 
  trees = 50, 
  min_n = 1, 
  tree_depth = 2, 
  learn_rate = 1, 
  sample_size = 1, 
  loss_reduction = 1
)

# tree_depth = 20
gera_gif( 
  trees = 50, 
  min_n = 1, 
  tree_depth = 300, 
  learn_rate = 1, 
  sample_size = 0.5, 
  loss_reduction = 0
)

# learn_rate = 0.1, 
gera_gif( 
  trees = 100, 
  min_n = 1, 
  tree_depth = 1, 
  learn_rate = 0.1, 
  sample_size = 1, 
  loss_reduction = 1
)

# sample_size = 0.5
gera_gif( 
  trees = 100, 
  min_n = 1, 
  tree_depth = 1, 
  learn_rate = 1, 
  sample_size = 0.5, 
  loss_reduction = 1
)

# loss_reduction = 0.1
gera_gif( 
  trees = 100, 
  min_n = 1, 
  tree_depth = 1, 
  learn_rate = 1, 
  sample_size = 1, 
  loss_reduction = 0.1
)

# mudando tudo
gera_gif( 
  trees = 100, 
  min_n = 1, 
  tree_depth = 2, 
  learn_rate = 0.1, 
  sample_size = 0.5, 
  loss_reduction = 0.1
)

##########################################

modelo <- boost_tree(
  mode = "regression", 
  mtry = 1, 
  trees = 100, 
  min_n = tune(), 
  tree_depth = tune(), 
  learn_rate = tune(), 
  sample_size = tune(), 
  loss_reduction = tune()
) %>%
  set_engine("xgboost", base_score = 0)

library(doParallel)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

tune_grid_res <- tune_grid(
  y ~ x,
  modelo,
  resamples = vfold_cv(dados, times = 5),
  grid = 100,
  control = control_grid(verbose = TRUE, allow_par = TRUE)
)

best_model <- select_best(x = tune_grid_res, metric =  "rmse", maximize = FALSE)
final_model <- finalize_model(modelo, best_model)
ajuste <- fit(final_model, y ~ x, data = dados)

dados_xgb <- dados %>% select(x) %>% as.matrix()

dados %>%
  mutate(
    pred = xgboost:::predict.xgb.Booster(ajuste$fit, newdata = dados_xgb, ntreelimit = 0)
  )

dados_extr <- tibble(x = seq(-1, 1, length.out = 1000)) 
dados_xgb_extr <- dados_extr %>% select(x) %>% as.matrix()
dados_extr <- dados_extr %>%
  mutate(
    pred = xgboost:::predict.xgb.Booster(ajuste$fit, newdata = dados_xgb_extr, ntreelimit = 0)
  )

dados %>%
  mutate(
    pred = xgboost:::predict.xgb.Booster(ajuste$fit, newdata = dados_xgb, ntreelimit = 0)
  ) %>%
  ggplot(aes(x = x)) +
  geom_point(aes(y = y), size = 2, alpha = 0.4) +
  stat_function(fun = ~sin(. * pi * 2), colour = "purple", size = 1.5) +
  geom_step(aes(y = pred), colour = "orange", size = 2) +
  geom_step(aes(y = pred), colour = "orange", size = 2, linetype = "dashed", data = dados_extr)

