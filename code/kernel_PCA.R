# http://rasbt.github.io/mlxtend/user_guide/feature_extraction/RBFKernelPCA/

# create circular data
data <- data.frame()

for (radius in c(1,2,3)) {
  n = 100
  r <- rnorm(n, radius, 0.1)
  phi <- runif(n, 0, 2 * pi)
  data <- rbind(data, data.frame(x = r * cos(phi), y = r * sin(phi), color = rep(radius, n))) 
}

plot(data$x, data$y, col=c("red","blue","green")[data$color], cex=.1)


# try linear PCA
X <- data.matrix(data[,c('x','y')])
X <- apply(X, 2, scale, scale=TRUE, center=TRUE) 
C <- t(X) %*% X / (n-1) # cov(X)

eigen(C)
# plot PCs
plot(X, cex=.1)
lines(c(0,eigen(C)$vectors[1,1]),c(0,eigen(C)$vectors[2,1]),"l", col="red", lwd=5)
lines(c(0,eigen(C)$vectors[1,2]),c(0,eigen(C)$vectors[2,2]),"l", col="red", lwd=5)
plot(X %*% t(eigen(C)$vectors), cex=.1)

# try kernel PCA
GaussKernel <- function(x, y, sigma=5) { 
  return(exp(-sum((x-y)^2) / sigma))
}

PolyKernel <- function(x, y) { 
  return((x %*% y + 1)^2)
}

K = matrix(0, nrow = dim(X)[1], ncol = dim(X)[1])

# create kernel Matrix
for (i in 1:dim(X)[1]) {
  for (j in 1:dim(X)[1]) {
    K[i,j] <- GaussKernel(X[i,], X[j,])
  }
}

# center kernel?
# -> check http://hduongtrong.github.io/2016/02/10/KMean-KMedoid-KernelPCA/

nm = matrix(1 / dim(X)[1], nrow = dim(X)[1], ncol = dim(X)[1])
K2 <- K - nm %*% K - K %*% nm + nm %*% K %*% nm 

V <- eigen(K)$vectors

transferred_data <- data.frame(x = V[,1], y = V[,2], color = c(rep(1,n), rep(2,n), rep(3,n)))
plot(transferred_data$x, transferred_data$y, col=c("red","blue","green")[transferred_data$color], cex=.3)


library(kernlab)
library(BKPC)

data(iris)
testset <- sample(1:150,20)

train <- as.matrix(iris[-testset,-5])
test <- as.matrix(iris[testset,-5])


# make training set kernel

gk <- gaussKern(train)
Ktrain <- gk$K
image(Ktrain)

# make testing set kernel

gk2 <- gaussKern(train, test, gk$theta) 
Ktest <- gk2$K


#  make training set kernel using kernelMatrix from library kernlab.


kfunc <- laplacedot(sigma = 1)
Ktrain2 <- kernelMatrix(kfunc, train)
image(Ktrain2)

# make testing set kernel using kernelMatrix {kernlab}

Ktest2 <- kernelMatrix(kfunc, test, train)



# Do KPCA:

kpcData <- kPCA(train)
kpcKern <- kPCA(Ktrain)
kpcKern2 <- kPCA(Ktrain2)


# plot the data projection on the principal components

pairs(kpcData$KPCs[ , 1 : 3], col = iris[-testset, 5])

# proportion of variance explained by each PC

plot(kpcData$Es/sum(kpcData$Es), xlab = "PC", ylab = "Proportion of variance")


# embed data from the testing set on the new space:


KPCpred1 <- predict(kpcData, test)

KPCpred2 <- predict(kpcKern, Ktest)

KPCpred3 <- predict(kpcKern2, Ktest2)

#plot the test data projection on the principal components

pairs(KPCpred3[ , 1 : 3], col = iris[testset, 5])

