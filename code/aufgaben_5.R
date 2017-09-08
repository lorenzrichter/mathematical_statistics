# Aufgabe 1
n <- 100000
X <- matrix(runif(n*2, -1, 1), nrow=2)
in_circle <- function(x) {
  return(x[1]^2 + x[2]^2 < 1)
}
mean(apply(X, 2, in_circle)) * 4

circle <- X[,apply(X, 2, in_circle)]
plot(circle[1,], circle[2,], cex=.2)



# Aufgabe 2
# PCA with simulated data
library(MASS)
n <- 10000
mu <- c(2, 1)
Sigma <- matrix(c(1, 0.5, 0.5, 1), nrow=2, ncol=2)
data <- mvrnorm(n, mu, Sigma)
plot(data)

X <- apply(data, 2, scale, scale=TRUE, center=TRUE) 
C <- t(X) %*% X / (n-1) # cov(X)

e <- c(2.5,1) 
e <- e / norm(e, type="2")
t(e) %*% C %*% e # variance in direction e
eigen(C)
# plot PCs
plot(X)
lines(c(0,eigen(C)$vectors[1,1]),c(0,eigen(C)$vectors[2,1]),"l", col="red", lwd=5)
lines(c(0,eigen(C)$vectors[1,2]),c(0,eigen(C)$vectors[2,2]),"l", col="red", lwd=5)

t(eigen(C)$vectors) %*% C %*% eigen(C)$vectors

plot(X %*% eigen(C)$vectors, cex=.3)

# alternatively with package
PCA <- prcomp(data, center=TRUE, scale=TRUE) 
PCA
biplot(PCA)


# reduce dimensionality
n = 500
mu <- c(0, 0)
Sigma <- matrix(c(1, 0.95, 0.95, 1), nrow=2, ncol=2)
data <- mvrnorm(n, mu, Sigma)
X <- apply(data, 2, scale, scale=TRUE, center=TRUE) 
plot(X)

C <- t(X) %*% X / (n-1) # cov(X)
cov(X)
C

eigen(C)

lines(c(0,eigen(C)$vectors[1,1]),c(0,eigen(C)$vectors[2,1]),"l", col="red", lwd=5)
projection <- X %*% eigen(C)$vectors
hist(projection[,1])


# Iris data set introduced by Fisher
head(iris, 3)

iris
dict <- vector(mode="list", length=3)
names(dict) <- c("setosa", "versicolor", "virginica")
dict[[1]] <- 1; dict[[2]] <- 2; dict[[3]] <- 3
dict
# sepal = Kelchblatt
# petal = Kronblatt

# log transform 
log_iris <- log(iris[, 1:4])
species <- iris[, 5]

iris_pca <- prcomp(log_iris, center = TRUE, scale. = TRUE) 

# plot variances explained by principle components
plot(iris_pca, type = "l")

X <- apply(log_iris, 2, scale, scale=TRUE, center=TRUE) 
C <- cov(X)
eigen(C)
summary(iris_pca)
dict
dict[iris[,5]]

plot(X[,1], X[,2], col=c('green', 'red', 'yellow')[dict])

ev <- eigen(C)$vectors
ev[,1] %*% C %*% ev[,1]
sqrt(eigen(C)$values)

# plot first two principle components
biplot(iris_pca)

# fancier plotting
library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(iris_pca, obs.scale = 1, var.scale = 1, groups = species, ellipse = TRUE, circle = FALSE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

## check out randomized PCA: https://arxiv.org/pdf/1608.02148.pdf
## https://research.fb.com/fast-randomized-svd/
## https://arxiv.org/pdf/0909.4061.pdf



# Aufgabe 3
library(MASS)
library(stats)

K <- 3
ns <- c(1000, 10000, 5000)
n <- sum(ns)
mus <- list(c(-2, 2), c(3, -3), c(1, 1))
Sigmas <- list(matrix(c(1, 0.5, 0.5, 1), nrow=2, ncol=2), matrix(c(1, 0.5, 0.5, 1), nrow=2, ncol=2), 
               matrix(c(1, -0.9, -0.9, 1), nrow=2, ncol=2))

X <- vector('numeric')
for (i in 1:length(mus)) {
  X <- rbind(X, mvrnorm(ns[i], mus[[i]], Sigmas[[i]]))
}

plot(X, cex=0.2)

assignment <- rep(0, n)
m <- X[sample.int(n, K),]

for (j in 1:5) {
  for (i in 1:n) {
    distance <- vector('numeric')
    for (k in 1:K) {
      distance[k] <- sum((X[i,] - m[k,])^2)
    }
    assignment[i] <- which.min(distance) 
  }
  for (k in 1:K) {
    m[k,] <- colMeans(X[assignment == k,])
  }
  plot(X, cex=0.2, col=c('blue', 'green', 'red')[assignment])
}

# R implementation
cluster <- kmeans(X, 3)
plot(X, cex=0.2, col=c('blue', 'green', 'red')[cluster$cluster])
