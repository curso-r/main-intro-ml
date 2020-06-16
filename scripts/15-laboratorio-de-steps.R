library(tidymodels)
library(tidyverse)

# laboratorio de steps
receita <- recipe(disp ~ ., data = mtcars) %>%
  step_log(mpg) %>%
  step_center(mpg)

# prep - funcao que vai treinar a receita.
receita_preparada <- prep(receita)

# juice - extrai a base usada para treinar na sua forma transformada.
juice(receita_preparada)

# bake - transforma bases novas.
receita_preparada %>% bake(new_data = mtcars_novos)

# vazamento de informacao da base de teste dentro da base de treino.
# ajustar um modelo com a base de treino e nao pode usar nada 
# que venha da base de teste.
# data leakage.

# laboratorio de steps
mtcars_treino <- mtcars
mtcars_teste <- mtcars
mtcars_full <- bind_rows(mtcars_treino, mtcars_teste)
receita <- recipe(disp ~ ., data = mtcars_full) %>%
  step_log(mpg) %>%
  step_center(mpg)


mtcars$mpg - 32.09062

mtcars <- mtcars %>%
  mutate(
    mpg = mpg - 20.09062
  )


receita_preparada <- prep(receita)

juice(receita_preparada)


mtcars$mpg %>% hist

juice(receita_preparada)$mpg %>% hist()




# dados novos
mtcars_novos <- mtcars

receita_preparada %>% bake(new_data = mtcars_novos)

receita_preparada













