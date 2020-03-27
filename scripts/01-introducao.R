
# Setup ------------------------------------------

library(tidymodels)
library(tidyverse)
library(MASS)
# install.packages("remotes")
# remotes::install_github("curso-r/treesnip")
library(treesnip)

# dados -----------------------

glimpse(Boston)

# modelo escolhido ------------

# arvore
model <- decision_tree(min_n = 10) %>% 
  set_mode("regression") %>% 
  set_engine("tree")

# ajuste do modelo -----------
# fit_data()

glimpse(Boston)
fit_data(model, )


# Ir p/ slides
# predicoes -------------------

# predict()


# resíduos


# metricas

# -----------------------------

# dados antigos vs dados novos (estudar os erros) - nós queremos generalizar

is <- initial_split(Boston)

tune::fit_resamples(boston_model, is, medcv ~. )
fit(is, boston_model, medv ~. )

# Overfitting

# hiperparametro: n_min

Boston

boston_split <- mc_cv(Boston, prop = 0.8, times = 1)

glimpse(MASS::Boston)
boston_model <- decision_tree(min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("tree")

pars <- parameters(boston_model) %>% 
  update(min_n = dials::min_n(range = c(2, 150)))

#boston_split <- vfold_cv(MASS::Boston, v = 5)

boston_tune <- tune_grid(
  boston_model,
  medv ~ .,
  grid = grid_regular(pars, levels = 10),
  resamples = boston_split
)

autoplot(boston_tune)



# intial_split() (treino/teste)
# fit_split()

