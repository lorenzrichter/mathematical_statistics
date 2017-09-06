# Aufgabe 1
n <- 10000
m <- 1000000
X <- rnorm(n, 2, 1)
Y <- rnorm(m, 0.0001, 1)

# one sample
T <- sqrt(n) * (mean(X) - 3) / sqrt(var(X))
(1 - pt(abs(T), df=n-1)) * 2

t.test(X, mu=3)

# compare t-value with different n
T_val <- vector('numeric')
p_val <- vector('numeric')

for (i in 1:10) {
  n <- i * 1000000
  X <- rnorm(n, 0.001, 1)
  T_val[i] <- sqrt(n) * mean(X) / sqrt(var(X))
  p_val[i] <- (1 - pt(abs(T_val[i]), df=n-1)) * 2
  print(n)
  print(p_val[i])
}

plot(1000000 * (1:10), T_val)
plot(1000000 * (1:10), p_val)

# two samples
S <- sqrt(((n - 1) * var(X) + (m - 1) * var(Y)) / (n + m - 2))
T <- (mean(X) - mean(Y)) / (S * sqrt(1 / n + 1 / m))
(1 - pt(abs(T), df=n+m-2)) * 2

t.test(X, Y, var.equal = TRUE)

# non-normal data
n <- 100
alpha <- 1
beta <- 2
X <- rgamma(n, alpha, beta)
plot(dgamma(seq(0, 4, by=0.1), alpha, beta), type='l')

true_mean <- alpha / beta


# Aufgabe 2
K <- 100000

counts <- vector('numeric')

for (k in 1:K) {
  n <- 100
  X <- rnorm(n, 0, 1)
  alpha <- 0.05
  t <- qt(1 - alpha / 2, df=n-1)
  I <- c(mean(X) - sqrt(var(X)) / sqrt(n) * t, mean(X) + sqrt(var(X)) / sqrt(n) * t)
  counts[k] <- (I[1] <= 0) & (I[2] >= 0)
}

mean(counts)


# Aufgabe 3
hypothesis_test <- function(X, alpha=0.05, mu=0, verbose=FALSE)	
{
  #Berechnung der Teststatistik
  n <- length(X)
  t <- sqrt(n) * (mean(X) - mu) / sqrt(var(X))
  #Konfidenzintervall, Vektor mit unterem und oberem Intervallende.
  qn <- qt(1 - alpha / 2, df=n-1)
  I <- c(mean(X) - sqrt(var(X)) * qn / sqrt(n), mean(X) + sqrt(var(X)) * qn / sqrt(n))
  ablehnung <- (abs(t) > qn)
  if (verbose) {
    if (!ablehnung) 
      message("Die Hypothese kann zum Niveau ", 100 * round(alpha, 3),"% nicht abglehnt werden.")
    else
      message("Die Hypothese wird zum Niveau ", 100 * round(alpha, 3),"% verworfen.")
    
    message("Der wahre Mittelwert liegt mit ", 100 * (1 - alpha),"% Sicherheit im Intervall [",round(I[1],3),",",round(I[2],3),"].")
  }
  return(list(ablehnung=ablehnung, I=I,t=t))
}

hypothesis_test(rnorm(100, mean=2), verbose=TRUE)
hypothesis_test(rnorm(100), verbose=TRUE)

# empirical test
K <- 1000
n <- 10
mu <- 4
X <- matrix(rnorm(n*K, mean=mu), nrow=K)	

# true mean in confidence interval?
test_1 <- apply(X, 1, function(x) hypothesis_test(x, mu=mu)[[2]])
sum((mu >= test_1[1,]) * (mu <= test_1[2,])) / K

# how often (falsely) rejected? (i.e. alpha)
test_2 <- apply(X, 1, function(x) hypothesis_test(x, mu=mu)[[1]])
sum(test_2) / K

# power of the test (i.e. 1 - beta):
test_3 <- apply(X, 1, function(x) hypothesis_test(x, alpha=0.001, mu=3.5)[[1]])
sum(test_3) / K

# power increases with n, |mu - mu_0| and alpha

ns <- c(10,50,100)
mus <- seq(3,5,length=100)

for (i in 1:3) {
  K <- 1000
  n <- ns[i]
  mu <- 4
  X <- matrix(rnorm(n*K, mean=mu), nrow=K)	
  power <- vector('numeric')
  for (mu_0 in mus) {
    test_3 <- apply(X, 1, function(x) hypothesis_test(x, alpha=0.05, mu=mu_0)[[1]])
    power <- c(power, sum(test_3) / K)
  }
  if (i == 1) plot(mus, power, type='n', xlim=c(3,5), ylim=c(0,1), xlab='mu_0', ylab='1 - beta')
  lines(mus, power, type='l', col=c('green', 'red', 'blue')[i])
}
legend(3, 0.25, legend=ns, col=c('green', 'red', 'blue'), lty=1, cex=0.8)

mus <- seq(3,5,length=1000)
sigma <- 1
mu <- 4
n <- 10
power <- function(alpha, mus, n) pt(qt(alpha/2, df=n-1) + sqrt(n) * (mu - mus) / sigma, df=n-1) + pt(qt(alpha/2, df=n-1) - sqrt(n) * (mu - mus) / sigma, df=n-1)
lines(mus, power(0.05, mus, n))



