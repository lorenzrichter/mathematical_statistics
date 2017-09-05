# Aufgabe 1 - German tank problem
K <- 1000
ns <- c(10, 100, 1000, 10000)
theta <- 5
X <- sapply(ns, function(n) matrix(runif(K * n, 0, theta), nrow=K))

theta_1 <- sapply(X, function(A) apply(A, 1, max))
theta_2 <- theta_1 %*% diag((n + 1) / n)
theta_3 <- sapply(X, function(A) 2 * rowMeans(A))

colMeans(theta_1)
colMeans(theta_2)
colMeans(theta_3)

#empirisches quadratisches Risiko:
empRisk <- function(obs) mean((obs - theta)^2)

MLE_risk <- apply(theta_1, 2, empRisk)
UMVUE_risk <- apply(theta_2, 2, empRisk)
MM_risk <- apply(theta_3, 2, empRisk) 

risks <- data.frame(ns, MLE_risk, UMVUE_risk, MM_risk)
str(risks)
print(risks)


# Aufgabe 2
n <- 50000
X <- rnorm(n)
n_cum <- 10:n
plot(n_cum, cumsum(X[n_cum])/n_cum, log = "x", type = "l", ylab = "X_n", xlab = "n")
lines(n_cum, 1/sqrt(n_cum), lty = 2, col="red")
lines(n_cum, -1/sqrt(n_cum), lty = 2, col="red")
lines(n_cum, sqrt(2*log(log(n_cum))/n_cum), col="blue")
lines(n_cum, -sqrt(2*log(log(n_cum))/n_cum), col="blue")


# Aufgabe 3
lambda <- 0.4
nn <- c(10, 100, 500, 1000)
randomnumbers <- list()
i <- 1
K <- 1000
for (n in nn)
{
  randomnumbers[[i]] <- matrix(rpois(K*n, lambda=0.4), nrow=n, ncol=K)
  i <- i+1
}
standardizedmean <- function(X, lambda) {
  n <- length(X[,1])
  return(sqrt(n) * (colMeans(X) - lambda) / sqrt(lambda))
}
means <- lapply(randomnumbers, function(X) standardizedmean(X, lambda))
#stelle Plotparameter ein: 3x2 Bilder im naechsten Graphik device.
par(mfrow=c(2,2))
for (jj in (1:4)) hist(means[[jj]], main=paste("n=", nn[jj]), xlab="", breaks=20, frequency=FALSE)
par(mfrow=c(2,2))
pp <- seq(0, 1, by=0.01)
for (kk in (1:4)) {
  plot(quantile(means[[kk]], pp) - qnorm(pp), type="h",
       main=paste("n=",nn[kk]), xlab="", ylab="", ylim=c(-0.1, 0.1))
}
par(mfrow=c(2,2))
for (ii in (1:4)) qqnorm(means[[ii]])


# Aufgabe 4
# The Chapman-Robbins inequality suggests that there is not an unbiased estimator for lambda, 
# but for 1 / lambda.
# For computational details see https://stats.stackexchange.com/questions/100636/bias-of-the-maximum-likelihood-estimator-of-an-exponential-distribution
lambda <- 2

lambda_exp <- vector('numeric')
lambda_hat_exp <- vector('numeric')
lambda_hat_2_exp <- vector('numeric')

for (i in 2:10) { 
  lambda_hat <- vector('numeric')
  lambda_hat_2 <- vector('numeric')
  for (j in 1:10000) {
    n <- i * 1
    X <- rexp(n, lambda)
    # hist(X, breaks=50)
    lambda_hat[j] <- n / sum(X)
    lambda_hat_2[j] <- sum(X) / n
  }
  lambda_exp[i-1] <- n / (n-1) * lambda
  lambda_hat_exp[i-1] <- mean(lambda_hat)
  lambda_hat_2_exp[i-1] <- mean(lambda_hat_2)
}


i <- 2
lambda_hat <- rep(0,10)
lambda_hat_2 <- rep(0,10)
for (j in 1:10) {
  n <- i * 1
  X <- rexp(n, lambda)
  # hist(X, breaks=50)
  lambda_hat[j] <- 1#1 / mean(X)
  lambda_hat_2[j] <- 1#mean(X)
}
lambda_exp[i-1] <- n / (n-1) * lambda
lambda_hat_exp[i-1] <- mean(lambda_hat)
lambda_hat_2_exp[i-1] <- mean(lambda_hat_2)
lambda_hat

length(lambda_hat)
mean(lambda_hat)

n_val = 2:10 * 1

plot(n_val, lambda_hat_2_exp, type='l',  ylim=c(min(1 / lambda_hat_2_exp), max(lambda_hat_exp)))
lines(n_val, 1 / lambda_hat_2_exp, type='l', col='green')
lines(n_val, lambda_exp, type='l', col='red')
lambda_hat_2_exp


# Aufgabe 5
K <- 10000
n <- 100
lambda <- 2
I <- n / lambda
lambda_hat_exp <- vector('numeric')
for (i in 1:K) {
  X <- rpois(n,lambda)
  lambda_hat <- mean(X)
  lambda_hat_exp[i] <- lambda_hat
}

hist(lambda_hat_exp, breaks=seq(min(lambda_hat_exp) - 0.1, max(lambda_hat_exp) + 0.1, by=0.05), freq = FALSE)
x <- seq(min(lambda_hat_exp), max(lambda_hat_exp), by=0.01)
lines(x, dnorm(x, mean(lambda_hat_exp), sqrt(1 / I)), col='red')

# check out https://www.stat.umn.edu/geyer/5931/mle/mle.pdf
# fitting a misspecified model? http://galton.uchicago.edu/~eichler/stat24600/Handouts/s02add.pdf
