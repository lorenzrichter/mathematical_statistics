library(MASS)

# The James-Stein estimator is significantly better for small |mu|, large d and small n

JS <- function(X) {
  n <- dim(X)[1]
  d <- dim(X)[2]
  return((1 - (d - 2) / (n * sum(colMeans(X)^2))) * colMeans(X))
}

n <- 10000
d <- 10
mu <- rep(0, d)
Sigma <- diag(d)

K <- 100

mu_hat <- matrix(0, nrow=K, ncol=d)
mu_JS <- matrix(0, nrow=K, ncol=d)

for (k in 1:K) {
  X <- mvrnorm(n, mu, Sigma)
  mu_hat[k,] <- colMeans(X)
  mu_JS[k,] <- JS(X)
}

mean(rowSums(mu_hat^2))
mean(rowSums(mu_JS^2))
