# Aufgabe 1
library(MASS)

# linear regression
n <- 1000
x <- runif(n, 0, 10)
beta <- c(5, 1)
noise <- rnorm(n)
Sigma <- diag(n)

A <- matrix(runif(n^2) * 2 - 1, ncol=n) 
Sigma <- 5 * t(A) %*% A + diag(n)
noise <- mvrnorm(1, rep(0, n), Sigma)

X <- cbind(rep(1, n), x)
Y <- X %*% beta + noise

beta_hat <- solve((t(X) %*% solve(Sigma) %*% X)) %*% t(X) %*% solve(Sigma) %*% Y 
beta_hat <- solve((t(X) %*% X)) %*% t(X) %*% Y 

regression <- function(x, beta) beta[2] * x + beta[1]
plot(x, Y)
lines(x, regression(x, beta), col='green')
lines(x, regression(x, beta_hat), col='red')

# alternatively
SSX <- sum((x - mean(x))^2)
SSY <- sum((Y - mean(Y))^2)
SSXY <- sum((x - mean(x))*(Y - mean(Y)))
beta_hat_2 <- c(mean(Y) - SSXY/SSX * mean(x), SSXY / SSX)

# compare with LM
fit <- lm(Y ~ x)
summary(fit)
beta_hat
beta_hat_2

# polynomial regression
n = 100
x <- runif(n, -3, 3)
beta <- c(1,-8,1,2)
noise <- rnorm(n)
X <- cbind(rep(1, n), x, x^2, x^3)
Y <- X %*% beta + noise

beta_hat = solve((t(X) %*% X)) %*% t(X) %*% Y 
regression <- function(x, beta) beta[4] * x^3 + beta[3] * x^2 + beta[2] * x + beta[1]
beta_hat
plot(x, Y)
lines(sort(x), regression(sort(x), beta), col='green')
lines(sort(x), regression(sort(x), beta_hat), col='red')

# compare with LM
fit <- lm(Y ~ x + I(x^2) + I(x^3))
summary(fit)
beta_hat



# Aufgabe 2

# storks deliver babies
x_1 = c(28750, 83860, 30520, 111000, 43100, 544000, 357000, 132000, 41900, 93000, 301280, 312680, 92390,
        237500, 504750, 41290, 779450) # area
x_2 = c(100, 300, 1, 5000, 9, 140, 3300, 2500, 4, 5000, 5, 30000, 1500, 5000, 8000, 150, 25000) # storks
x_3 = c(3.2, 7.6, 9.9, 9.0, 5.1, 56, 78, 10, 15, 11, 57, 38, 10, 23, 39, 6.7, 56) # humans
x_4 = c(83, 87, 118, 117, 59, 774, 901, 106, 188, 124, 551, 610, 120, 367, 439, 82, 1576) # birth rate


length(x_4)
data <- data.frame(x_1, x_2, x_3, x_4)

cor.test(x_2, x_4)

X <- cbind(rep(1, 17), x_2)
Y <- x_4
beta_hat <- solve((t(X) %*% X)) %*% t(X) %*% Y 
beta_hat

fit <- lm(x_4 ~ x_2)
summary(fit)
beta <- summary(fit)$coefficients[,1]

plot(x_2, x_4)
regression <- function(x, beta) beta[2] * x + beta[1]
lines(sort(x_2), regression(sort(x_2), beta), col='green')

# confounding variable land area?


# Aufgabe 3

n_1 <- 100
n_2 <- 100
n_3 <- 100

sigma <- 1

mu_1 <- 1
mu_2 <- 1
mu_3 <- 1.1
n <- n_1 + n_2 + n_3
p <- 3

Y_1 <- rnorm(n_1, mu_1, sigma)
Y_2 <- rnorm(n_2, mu_2, sigma)
Y_3 <- rnorm(n_3, mu_3, sigma)

# create design and response matrix
Y <- c(Y_1, Y_2, Y_3)
X <- c(rep(1, n_1), rep(1, n_2), rep(1, n_3))
X <- matrix(0, n, p)
X[1:n_1,1] = rep(1, n_1)
X[(n_1+1):(n_1+n_2),2] = rep(1, n_2)
X[(n_1+n_2+1):n,3] = rep(1, n_3)

fit <- lm(formula = Y ~ X)
anova(fit)

beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
Y_hat <- X %*% beta_hat

SSQ <- sum((mean(Y) - Y_hat)^2) # (p-1) * V_b
MSQ <- sum((mean(Y) - Y_hat)^2) / (p-1) # V_b
SSR <- sum((Y - Y_hat)^2) # (n-p) * V^*
MSR <- sum((Y - Y_hat)^2) / (n-p) # V^*

F <- MSQ / MSR
F
qf(0.05, p, n-p)

sum((Y - mean(Y))^2)
SSQ + SSR



# Aufgabe 4

data <- read.table('ownCloud/Documents/R-Kurs/data/creditcard.csv', header=TRUE, sep=',')

# from https://www.kaggle.com/dalpozz/creditcardfraud
# data <- read.csv('ownCloud/Documents/R-Kurs/data/creditcard.csv', header=TRUE, sep=',')

dim(data)
head(data)
sapply(data, function(x) sum(is.na(x)))
sapply(data, function(x) length(unique(x)))

# categorical variables to dummy variables with contrasts()

data[,1:30] <- scale(data[,1:30], center=TRUE, scale=TRUE)

train <- data[1:200000,]
test <- data[200001:284807,]

model <- glm(Class ~., family=binomial(link='logit'), data=train)

summary(model)

# evaluation
p_test <- predict(model, newdata=test, type='response')
p_train <- predict(model, newdata=train, type='response')
class_predictions <- ifelse(p_test > 0.5, 1, 0)
table(class_predictions)
error <- mean(class_predictions != test$Class)
print(paste('Accuracy', 1 - error))
hist(p_test[p_test > 0.01], breaks=500)

library(ROCR)
pr_test <- prediction(p_test, test$Class)
prf_test <- performance(pr_test, measure='tpr', x.measure='fpr')
pr_train <- prediction(p_train, train$Class)
prf_train <- performance(pr_train, measure='tpr', x.measure='fpr')
plot(prf_train, col='green')
plot(prf_test, col='red', add = TRUE)
lines(c(0,1), c(0,1))

auc <- performance(pr_test, measure = "auc")
auc <- auc@y.values[[1]]
auc


# SGD
n <- dim(train)[1]
m <- dim(train)[2]

beta <- rep(0, m)

logistic <- function(beta, x) {
  return(1 / (1 + exp(-x %*% beta)))
}

X <- data.matrix(cbind(rep(1, n), train[,1:30]))
Y <- train[,31]

for (j in 1:n) {
  p <- logistic(beta, X[j,])
  beta <- beta - 1 / sqrt(j) * X[j,] * (p - Y[j])
}

pred_train <- apply(X, 1, function(x) logistic(x, beta))
mean(pred_train)

X_test <- data.matrix(cbind(rep(1, dim(test)[1]), test[,1:30]))
Y_test <- test[,31]
pred_test <- apply(X_test, 1, function(x) logistic(x, beta))

pr_train <- prediction(pred_train, Y)
prf_train <- performance(pr_train, measure='tpr', x.measure='fpr')
plot(prf_train, col='blue', add=TRUE)
pr_test <- prediction(pred_test, Y_test)
prf_test <- performance(pr_test, measure='tpr', x.measure='fpr')
plot(prf_test, col='yellow', add=TRUE)

auc <- performance(pr_test, measure = "auc")
auc <- auc@y.values[[1]]
auc




