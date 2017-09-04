library(HistData)
library(MASS)

# which chart to use? https://i1.wp.com/www.tatvic.com/blog/wp-content/uploads/2016/12/Pic_2.png

data(VADeaths)
VADeaths

# par(mfrow=c(2,3)) # rows x cols
par(mfrow=c(1,1))
# Historam
#hist(VADeaths, breaks=10, col=c('green', 'red'), main="title")

iris
colnames(iris)
hist(iris$Sepal.Length, breaks=20, main='Verteilung der Kelchblatt-Laenge')

# Barplot
barplot(iris$Sepal.Length, col=c('green', 'blue', 'red'))
table(iris$Sepal.Length)
barplot(table(iris$Species, iris$Sepal.Length)) #Stacked Plot

Z <- rnorm(20000, mean=10, sd=3)
hist(Z, freq=FALSE, nclass=100)

barplot(table(round(rnorm(2000))))

# Boxplot - median, 1. and 3. quantile - different definitions for whiskers
# upper whisker = min(max(x), Q_3 + 1.5 * IQR) 
# lower whisker = max(min(x), Q_1 ??? 1.5 * IQR)
boxplot(iris$Sepal.Length, col='red')
boxplot(iris$Sepal.Length~iris$Species, col=c('red', 'yellow', 'blue'))

# scatter combinations
plot(iris)

# pie charts
pie(table(iris$Species))

# hexbin
library(hexbin)
X <- mvrnorm(10000, c(1,1), matrix(c(1,0.5,0.5,1),ncol = 2))
cr <- colorRampPalette(c('yellow','red'))
plot(hexbin(X, xbins=30), colramp=cr)

## q-q plot
X <- rnorm(1000, 0, 1)
qqnorm(X)
X <- rnorm(1000, 5, 10)
qqnorm(X)
X <- rgamma(1000, 2)
qqnorm(X)

# mosaicplot for categorical data
HairEyeColor
mosaicplot(HairEyeColor)

# heatmap
heatmap(HairEyeColor)
str(HairEyeColor)
data <- data.frame(HairEyeColor)
data
HairEyeColor$Hair

as.data.frame(xtabs(Freq ~ Hair + Eye, HairEyeColor))
library(ggplot2)
hec <- as.data.frame(xtabs(Freq ~ Hair + Eye, HairEyeColor))
ggplot(hec, aes(Hair, Eye)) +
  geom_tile(aes(fill = Freq)) + 
  geom_text(aes(label = Freq),colour="white") 

# 3d
library(lattice)
cloud(iris$Sepal.Length~iris$Sepal.Width*iris$Petal.Length|iris$Species, main="3D Scatterplot by Species")
xyplot(iris$Sepal.Width ~ iris$Sepal.Length, iris, groups = iris$Species, pch= 20)

# plot missing data
library(Amelia)
library(mlbench)
data(Soybean)
# create a missing map
missmap(Soybean, col=c("black", "grey"), legend=FALSE)

# more advanced plotting: 
# - ggplot (http://r4ds.had.co.nz/data-visualisation.html)
# - plotly
