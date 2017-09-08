# http://www.jmlr.org/papers/volume9/vandermaaten08a/vandermaaten08a.pdf

library(Rtsne)

## calling the installed package
train <- read.csv('/Users/lorenz/ownCloud/Documents/mathematical_statistics/data/MNIST.csv') ## Choose the train.csv file downloaded from the link above  

number <- matrix(train[100,2:785], nrow=28, ncol=28)
number <- apply(number, 2, as.numeric)
image(number)

x_val <- seq(-4, 4, 0.1)
plot(x_val, dnorm(x_val), type='l')
lines(x_val, dt(x_val, df=1), type='l')

## Curating the database for analysis with both t-SNE and PCA
Labels <- train$label
train$label <- as.factor(train$label)
## for plotting
colors <- rainbow(length(unique(train$label)))
names(colors) <- unique(train$label)

## Executing the algorithm on curated data
tsne <- Rtsne(train[,-1], dims=2, perplexity=30, verbose=TRUE, max_iter=500)

## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=train$label, col=colors[train$label])

#show handwritten digit with show_digit(matriximage$x[n,]),n is any number below 60000.
matriximage&lt;-load_image_file("train-images-idx3-ubyte")
show_digit &lt;- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}
#Prepare fore prediction visualization
matrixtest&lt;-as.list(load_image_file("t10k-images-idx3-ubyte"))
show_number &lt;- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}
show_digit(matriximage$x[2017,])
show_number(matrixtest$x[9999,])
