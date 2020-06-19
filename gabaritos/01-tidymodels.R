# Exercícios - Tidymodels

# pacotes -----------------------------------------------------------------

library(tidymodels)
library(rpart.plot)
library(ISLR)

# dados ------------------------------------------------------------------
View(Hitters)
help(Hitters)


# exercício 1 -------------------------------------------------------------
# Defina uma especificação de f que caracterize uma árvore de regressão 
# (mode 'regression') com o tree_depth = 3 e use "rpart" como 'engine'.
# Curiosidade (veremos depois): tree_depth é a profundidade máxima da árvore.
# Dicas: decision_tree(), set_engine(), set_mode().

hitters_modelo <- decision_tree(tree_depth = 3) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# exercicio 2 -------------------------------------------------------------
# a) Usando a base Hitters, ajuste o modelo de árvore para os 'HmRun' (Home Runs em 1986) 
# de cada jogador usando como variável explicativa 'CHmRun' (o número de Home Runs que ele fez na vida).
# Dicas: fit(), uma fórmula e um data.frame.

hitters_ajuste <- hitters_modelo %>% fit(HmRun ~ CHmRun, data = Hitters)

# b) use rpart.plot(ajuste$fit) para visualizar a sua árvore. O que a intensidade
# da cor azul informa?
# OBS: troque o nome do objeto 'ajuste' para o nome do objeto que você
# criou em (a). Por exemplo: rpart.plot(hitters_ajuste$fit)

rpart.plot(hitters_ajuste$fit)

# exercicio 3 --------------------------------------------------------------
# Coloque uma coluna a mais no banco de dados com as predições.
# Dicas: predict()

Hitters_com_pred <- cbind(Hitters, predict(hitters_ajuste, Hitters))

# exercicio 4 -------------------------------------------------------------
# Calcule RMSE, MAE e R-quadrado do modelo que você ajustou.
# Dicas: use as funções do yardstick rmse(), mae(), rsq().

Hitters_com_pred %>% rmse(HmRun, .pred)
Hitters_com_pred %>% mae(HmRun, .pred)
Hitters_com_pred %>% rsq(HmRun, .pred)

# exercicio 5 [desafio] ---------------------------------------------------
# a) Faça um gráfico de dispersão entre HmRun e CHmRun e coloque a curva ajustada em cima.
# Dicas: use a tabela feita em (a). ggplot2() + geom_point() + geom_line()

Hitters_com_pred %>%
  ggplot() +
  geom_point(aes(x = CHmRun, y = HmRun)) +
  geom_line(aes(x = CHmRun, y = .pred), colour = "red", size = 1.5)

# b) Faça um gráfico de dispersão comparando as suas predições e o que realmente aconteceu.
# Dicas: use a tabela feita em (a). ggplot2() + geom_point()

Hitters_com_pred %>%
  ggplot() +
  geom_point(aes(x = .pred, y = HmRun))