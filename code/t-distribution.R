rowVar <- function(X) {
  return(rowSums((X - rowMeans(X))^2)/(dim(X)[2] - 1))
}

n <- 2
K <- 100000
X <- matrix(rnorm(n*K, 0, 1), nrow=K, ncol=n)

# known variance -> normally distributed
Z_n <- sqrt(n) * rowMeans(X) / 1
hist(Z_n, breaks=seq(min(Z_n)-1, max(Z_n)+1, by=0.1), freq = FALSE, xlim = c(-3, 3))
x_val <- seq(-3, 3, by=0.01)
lines(x_val, dt(x_val, df = n-1), col='red', lwd=3)
lines(x_val, dnorm(x_val), col='green', lwd=3)

# estimated variance -> t-distributed
T_n <- sqrt(n) * rowMeans(X) / sqrt(rowVar(X))
hist(T_n, breaks=seq(min(T_n)-1, max(T_n)+1, by=0.1), freq = FALSE, xlim = c(-3, 3))
lines(x_val, dt(x_val, df = n-1), col='red', lwd=3)
lines(x_val, dnorm(x_val), col='green', lwd=3)

# different distribution
n <- 3
K <- 100000
X <- matrix(rgamma(n*K, 1, 1), nrow=K, ncol=n)

T_n <- sqrt(n) * (rowMeans(X) - 1) / sqrt(rowVar(X))
hist(T_n, breaks=seq(min(T_n)-1, max(T_n)+1, by=0.1), freq = FALSE, xlim = c(-3, 3))
lines(x_val, dt(x_val, df = n-1), col='red', lwd=3)
lines(x_val, dnorm(x_val), col='green', lwd=3)
 