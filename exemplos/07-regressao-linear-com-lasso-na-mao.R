# Data generation ----------------------------------------------

n <- 10000

x1 <- rnorm(n)
x2 <- rnorm(n)

W1 <- 0.9 
W2 <- 0.5
B <- 0.1

y <- B + W1 * x1 + W2 * x2 + rnorm(n, 0, 0.001)

# Model definition ---------------------------------------------

model <- function(w1, w2, b, x1, x2) {
  w1 * x1 + w2 * x2 + b
}

loss <- function(y, y_hat, lambda = 0) {
  mean((y - y_hat)^2) + lambda * abs(w1) + lambda * abs(w2)
}

# Estimating via Gradient Descent --------------------------------------
S <- function(w, l, g) {
  if(w > l*g) {
    w - l*g
  } else if(abs(w) <= l*g) {
    0
  } else {
    w + l*g
  }
}

dl_dyhat <- function(y_hat) {
  2 * (y - y_hat) * (-1)
}

dyhat_dw1 <- function(w1) {
  x1
}

dyhat_dw2 <- function(w2) {
  x2
}

dyhat_db <- function(b) {
  1
}

# escreva o laço que faz a estimativa de todos os parametros w1, w2 e b.
gamma = 0.5
lambda = 0.0

w1 <- runif(1)
w2 <- runif(1)
b <- 0
h <- numeric(100)
for(i in 1:100) {
  y_hat <- model(w1, w2, b, x1, x2)
  w1 <- S(w1 - gamma * mean(dyhat_dw1(x1)*dl_dyhat(y_hat)), lambda, gamma)
  w2 <- S(w2 - gamma * mean(dyhat_dw2(x2)*dl_dyhat(y_hat)), lambda, gamma)
  b  <- b  - gamma * mean(dyhat_db(b)*dl_dyhat(y_hat))
}

w1;w2;b

lm(y ~ x1 + x2)
