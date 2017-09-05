# Aufgabe 1
alter <- c(21, 19, 26, 31, 22, 21, 43, 23, 22, 24)

min(alter)
mean(alter)
var(alter)
n <- length(alter)
sum((alter - mean(alter))^2) / (n-1)
var(alter)

alter[2] <- 20

semester <- c(4, 1, 10, 9, 1, 3, 3, 4, 3, 7)

cov(alter, semester)

covariance <- function(X, Y) {
  n <- length(X)
  return((sum((X - mean(X)) * (Y - mean(Y))) / (n-1)))
}

covariance(alter, semester)

cor(alter, semester)

correlation <- function(X, Y) {
  n <- length(X)
  return((sum((X - mean(X)) * (Y - mean(Y))) / (n-1)) / (sqrt(sum((X - mean(X))^2) / (n-1)) * sqrt(sum((Y - mean(Y))^2) / (n-1))))
}

correlation(alter, semester)

plot(alter, semester)

correlation(alter[-7], semester[-7])


# Aufgabe 2
data <- read.table('/Users/lorenz/ownCloud/Documents/mathematical_statistics/data/history_export_2017-09-02T15-59-53.csv',
                    sep=';', skip=10, header=TRUE)

head(data)

colnames(data)
head(data)
plot(data$Temperature.daily.mean..2.m.above.gnd.[1:365], type='l')

temp_08 <- data[data$Year == 2008,]$Temperature.daily.mean..2.m.above.gnd.
temp_16 <- data[data$Year == 2016,]$Temperature.daily.mean..2.m.above.gnd.

plot(temp_16, type='l')
boxplot(temp_08, temp_16)

hist(temp_08)
hist(data$Temperature.daily.mean..2.m.above.gnd.)
qqnorm(data$Temperature.daily.mean..2.m.above.gnd.)

boxplot(formula=Temperature.daily.mean..2.m.above.gnd. ~ Year, data=data)
help(boxplot)

cor(data$Temperature.daily.mean..2.m.above.gnd., data$Total.cloud.cover.daily.max..sfc.)
cor(data$Wind.speed.daily.mean..10.m.above.gnd., data$Total.cloud.cover.daily.max..sfc.)


# Aufgabe 3
lambda <- 0.7
d <- 2
rho <- rep(sqrt(lambda), d)

Sigma <- (1 - lambda) * diag(d) + rho %*% t(rho)

# falls rho negativ, arbeite mit komplexen Zahlen:
# lambda <- -0.3
# rho <- rep(sqrt(lambda + 0i), d)
# Sigma <- (1 - rho) * diag(d) + rho %*% t(rho)

# Konvertieren in reellwertige matrix
Sigma <- matrix(Re(Sigma), ncol=d, nrow=d)
det(Sigma)
sum(diag(Sigma))
svd(Sigma)

#definiere Dichte der bivariaten Normalverteilung mit erwartungswert 0
normalpdf <- function(x, mu, Sigma) {
  return(1 / (2 * pi * sqrt(det(Sigma))) * exp(-0.5 * (x - mu) %*% solve(Sigma) %*% (x - mu)))
}

#define grid in R^2 
x <- seq(-4, 4, by=0.1)
y <- seq(-4, 4, by=0.1)
z <- matrix(0, nrow = length(x), ncol = length(y))
mu <- c(1, 1)

for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    z[i,j] <- normalpdf(c(x[i], y[j]), mu, Sigma)
  }
}

persp(x, y, z, theta = 30, phi = 30)
contour(x, y, z)



# Aufgabe 4
alpha <- 2
beta <- 3

p <- function(x, alpha, beta) {
  return(beta^alpha * x^(alpha - 1) * exp(-beta * x) / gamma(alpha))
}

expectation <- function(x, alpha, beta) {
  return(x * beta^alpha * x^(alpha - 1) * exp(-beta * x) / gamma(alpha))
}

x <- seq(0, 10, by=0.05)
plot(x, p(x, alpha, beta), type='l')
lines(x, p(x, 1, 1), type='l', col='green')
lines(x, p(x, 0.5, 1), type='l', col='red')
lines(x, p(x, 2, 1), type='l', col='yellow')

integrate(expectation, 0, Inf, alpha, beta)
# E[X] = alpha / beta
