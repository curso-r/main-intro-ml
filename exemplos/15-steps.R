library(tidymodels)
library(embed) # steps para transformar variaveis categoricas em variaveis continuas
library(textrecipes) # steps para mexer com textos
library(timetk) # steps para mexer com series temporais
library(janitor) # funcoes auxiliares

data("diamonds")

# workflow
modelo <- linear_reg(penalty = 0, mixture = 1) %>% set_engine("glmnet")
wf <- workflow() %>% add_model(modelo)

# dados
diamonds_com_problemas_a_mais <- diamonds %>%
  mutate(
    coluninha_constante = 0.3,
    col_correlacionada_com_y = y + runif(n()),
    col_lincomb = y + x + z,
    across(where(is.ordered), as.character)
  )

# funcao recipe() ---------------------------------------------------------
rec <- recipe(price ~ ., data = diamonds_com_problemas_a_mais)

# colocando steps e preparando --------------------------------------------
rec <- rec %>% step_zv(all_predictors())
rec
prep(rec)

# juice() e bake() pra ver a base transformada --------------------------------
juice(prep(rec))
bake(prep(rec), diamonds)
bake(prep(rec), diamonds %>% mutate(coluna_extra = 1))


# seletores de varaiveis --------------------------------------------------
# pelo papel (role)
all_predictors(); all_outcomes()

# pelo tipo (type)
all_numeric(); all_nominal(); has_type("Date")

# pelo nome
starts_with(); ends_with(); contains(); matches()

# step_dummy() ------------------------------------------------------------
rec2 <- rec %>% step_dummy(all_nominal())

juice(prep(rec2)) %>% glimpse()

# step_novel --------------------------------------------------------------
rec3 <- rec %>% step_novel(color) %>% step_dummy(all_nominal())
juice(prep(rec3)) %>% glimpse()

bake(prep(rec3), diamonds %>% mutate(color = "K")) %>% glimpse()

mod <- fit(add_recipe(wf, rec2), data = diamonds_com_problemas_a_mais)
predict(mod, diamonds_com_problemas_a_mais %>% mutate(color = "K"))

mod <- fit(add_recipe(wf, rec3), data = diamonds_com_problemas_a_mais)
predict(mod, diamonds_com_problemas_a_mais %>% mutate(color = "K"))


# step_other() ------------------------------------------------------------
rec3 <- rec %>% step_other(clarity) %>% step_dummy(all_nominal())
juice(prep(rec3)) %>% glimpse()

diamonds_com_problemas_a_mais %>% 
  count(clarity) %>% 
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting()

bake(prep(rec3), diamonds %>% mutate(clarity = "nova_categ")) %>% glimpse()


# step_unknown() ----------------------------------------------------------
rec3 <- rec %>% step_unknown(clarity) %>% step_dummy(all_nominal())
juice(prep(rec3)) %>% glimpse()

bake(prep(rec3), diamonds %>% mutate(clarity = sample(c("VVS2", NA), size = n(), replace = TRUE))) %>% glimpse()

# step_modeimpute() ----------------------------------------------------------
rec3 <- rec %>% step_modeimpute(clarity) %>% step_dummy(all_nominal())
juice(prep(rec3)) %>% glimpse()

bake(prep(rec3), diamonds %>% mutate(clarity = sample(c("VVS2", NA), size = n(), replace = TRUE))) %>% glimpse()

# outras formas de imputar ---------------------------------------------------
# step_medianimpute()
# step_meanimpute()
# step_bagimpute()
# step_knnimpute()
# step_lowerimpute()
# step_rollimpute()


# steps de mudar escalas --------------------------------------------------
# step_center()
# step_scale()
# step_normalize()
# step_range()

# step_center()
rec3 <- rec %>% step_center(all_numeric())
juice(prep(rec3)) %>% summarise(across(where(is.numeric), ~round(mean(.))))

# step_scale()
rec3 <- rec3 %>% step_scale(all_numeric())
juice(prep(rec3)) %>% summarise(across(where(is.numeric), ~round(sd(.))))

# step_range()
rec3 <- rec %>% step_range(all_numeric(), min = 2, max = 3)
juice(prep(rec3)) %>% summarise(across(where(is.numeric), ~round(range(.))))

# steps de transformacao 1 para 1 coluna ----------------------------------
# step_log()
# step_sqrt()
# step_BoxCox()
# step_YeoJohnson()
# step_hyperbolic()
# step_logit()
# step_invlogit()
# step_relu()

# steps de transformacao de 1 para N colunas ------------------------------
# step_poly()
# step_bs()
# step_ns()

rec3 <- rec %>% step_poly(x, degree = 5)
juice(prep(rec3)) %>% glimpse()
juice(prep(rec3)) %>% select(where(is.numeric)) %>% cor %>% corrplot::corrplot()

rec3 <- rec %>% step_bs(x, degree = 5)
juice(prep(rec3)) %>% glimpse()

rec3 <- rec %>% step_ns(x, deg_free = 5)
juice(prep(rec3)) %>% glimpse()


# steps de transformacao de N para N colunas ------------------------------
# step_pca() # cp1 = a*x1 + b*x2 + c*x3 
# step_ica()
# step_isomap()
# step_umap()
# step_kpca()
# step_kpca_poly()
# step_kpca_rbf()
# step_interact()

# Cuidado com o outcome!
rec3 <- rec %>% step_pca(all_numeric(), threshold = 0.9)
juice(prep(rec3)) %>% glimpse()

rec3 <- rec %>% step_pca(all_numeric(), -all_outcomes(), threshold = 0.9)
juice(prep(rec3)) %>% glimpse()

# steps de eliminar variaveis ---------------------------------------------
# step_corr()
# step_zv()
# step_lincomb()
# step_rm()

rec3 <- rec %>% step_corr(all_numeric())
prep(rec3)

rec3 <- rec %>% step_lincomb(all_numeric())
prep(rec3)

rec3 <- rec %>% step_rm(all_predictors())
prep(rec3)

# steps do dplyr ----------------------------------------------------------
# step_arrange()
# step_filter()
# step_mutate()

rec3 <- rec %>% step_mutate(
  col_mutate1 = case_when(
    x + y > 2 ~ "OK",
    x - y > 10 ~ "Nao OK",
    TRUE ~ "Vamos ver"
  ),
  
  col_mutate2 = x + y - z
)
juice(prep(rec3)) %>% glimpse()

# steps de discretizacao de vars continuas --------------------------------
# step_discretize()
# step_discretize_cart()
# step_discretize_xgb()

# steps de texto ----------------------------------------------------------
# step_regex()
# step_count()
# step_embed()

# steps de funcoes janeladas ----------------------------------------------
# step_lag()
# step_window()

# steps de "continuizacao supervisionada" de variaveis categoricas --------
# step_woe()
# step_lencode_mixed()
# step_lencode_bayes()
# step_lencode_glm()

rec3 <- rec %>% step_lencode_glm(all_nominal(), outcome = "price")
juice(prep(rec3)) %>% glimpse()

# steps de distancias ------------------------------------------------------
# step_geodist()
# step_classdist()

# steps de balanceamento de base ------------------------------------------
# step_downsample()
# step_upsample()

# steps de datas ----------------------------------------------------------
# step_holiday()
# step_date()


