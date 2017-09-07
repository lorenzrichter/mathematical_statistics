library(CVST)

# kernel ridge regression

X <- seq(0, 10, by=0.01)
Y <- sin(X) * X + rnorm(length(X), 0, 0.2)
d <- constructData(x=X, y=Y)

krr <- constructKRRLearner()
p <- list(kernel="rbfdot", sigma=100, lambda=.1/getN(ns))
m <- krr$learn(d, p)

X_test <- seq(0, 10, by=0.01)
Y_test <- sin(X_test) * X_test + rnorm(length(X_test), 0, 0.2)
d_test <- constructData(x=X_test, y=Y_test)

pred <- krr$predict(m, d_test)
sum((pred - d_test$y)^2) / getN(nsTest)
plot(d)
lines(d_test$x, pred, col='red', lwd=5)


# own implementation

X <- seq(0, 10, by=0.01)
Y <- sin(X) * X + rnorm(length(X), 0, 1.5)

plot(X, Y)

k <- function(x, y, sigma2) {
  return(exp(-(x - y)^2 / (2 * sigma2)))
  #return((x %*% y) ^ 5)
}

K <- matrix(0, nrow = length(X), ncol = length(X))
for (i in 1:length(X)) {
  for (j in 1:length(X)) {
    K[i,j] <- k(X[i], X[j], 0.01)
  }
}

lambda <- 0.2
alpha <- solve(K + lambda * diag(length(X))) %*% Y

X_test <- seq(0, 10, by=0.01)
Y_pred <- vector('numeric')
for (i in 1:length(X_test)) {
  k_x <- matrix(k(X_test[i], X, 0.01))
  Y_pred[i] <- t(alpha) %*% k_x
}

plot(X, Y)
lines(X_test, Y_pred, col='red', lwd=5)

Y_true <- sin(X) * X
lines(X, Y_true, col='green', lwd=2)


# try polynomial regression

n <- 1000
x <- seq(0, 10, by=0.01)
X <- cbind(rep(1, length(x)), x, x^2, x^3, x^4, x^5, x^6)
Y <- sin(x) * x + rnorm(length(x), 0, 0.2)

beta_hat = solve((t(X) %*% X)) %*% t(X) %*% Y 
regression <- function(x, beta) beta[7] * x^6 + beta[6] * x^5 + beta[5] * x^4 + beta[4] * x^3 + beta[3] * x^2 + beta[2] * x + beta[1]
beta_hat
lines(sort(x), regression(sort(x), beta_hat), col='green')


