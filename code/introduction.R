x <- 1
y <- 2
z <- x + y
print(z)

# elementares Rechnen
exp(x + y) == exp(x) * exp(y) # Achtung: Rundungsfehler
exp(1i * x) == cos(x) + 1i * sin(x)

# der wichtigste Befehl: help()
help(log)
log(1024, base = 2)

ls()
rm(x, y)
ls()
# entferne alle Objekte
rm(list=ls(all=TRUE))

# Vektoren
x <- c(1, 2, 3)
y <- c(4, 5, 6)

x[1]
x[1:2]
x[-1]
x[c(T,T,F)]

x < 3
all(x < 3)
any(x < 3)
which(x < 3)
x[which(x < 3)]
x[x < 3]

x + y

# Addition oder Multiplikation mit Skalar erfolgt komponentenweise
x + 2
x * 2

# komponentenweise Multiplikation
x * y
# komponentenweises Auswerten von Funktionen
exp(x)

# Skalarprodukt x^T y
x %*% y
# Aeusseres Produkt x y^T
x %o% y

# Sequenzen
seq(1:10)
1:10
seq(1, 5, by=0.5)
seq(0, 2*pi, length.out=10)
rep(1, 10)
rep(c(1,2,3),c(3,2,1))

min(x)
sum(x)
prod(x)
which(y %% 2 == 0)
sort(c(3,2,1))

# Matrizen 
A <- matrix(c(1,2,3,4,5,6), ncol=3)
A
A <- matrix(c(1,2,3,4,5,6), ncol=3, byrow=TRUE)
A
B <- cbind(c(1,2), c(2,4), c(3,6))
B
C <- rbind(1:3,4:6)
all(A == B) & all(A == C)
diag(x)
det(diag(x))

# Multiplikation
D = A %*% t(B)
print(D)
eigen(D)

# invertieren und Loesen von Gleichungssystemen
det(D)
D <- D + diag(rep(0.01, 2))
D_inv <- solve(D)
D_inv %*% D

b <- c(1,1)
sol <- solve(D, b)
sol
D %*% sol

# Funktionen
Mittelwert <- function(x=c(1:10)) { 
  y <- sum(x) / length(x)
  return(y)
}

n <- 100
X <- 1:n
Mittelwert(X)
mean(X)
(n+1)/2

# Unterschied zwischen = und <-
median(v = 1:10)
v
median(v <- 1:10)
v

# Intermezzo: Varianz, Korrelation 
# -> Aufgabe 1

# Data Frames
Wetter <- data.frame("Beschreibung"=c("heiter","sonnig", "wolkig"), 
                   "Regen"=c(250,0,400),
                   row.names= c("Montag","Dienstag","Mittwoch"))
Wetter
class(Wetter)
str(Wetter) # Data frame hat verschiedene Variablentypen
Wetter$Regen
Wetter[1,2]
Wetter[,1]
Wetter[1,]
table(Wetter)
Wetter <- rbind(Wetter, data.frame("Beschreibung"="sonnig","Regen"=0, row.names="Donnerstag"))
Wetter

attach(Wetter)
Regen
detach(Wetter)

# Matrix zu Data Frame
A_df <- data.frame(A)
A_df
as.matrix(A_df)
A

# Listen
l <- list(matrix(1:6, ncol=3), "Hallo Welt", 1:5)
l[2]

# --> Visualisierungen

# Einlesen von Daten
getwd()
setwd("/Users/lorenz/ownCloud/Documents/mathematical_statistics/data/")
dir()

data <- read.table("regressiondata.csv", header=TRUE)

# Schreiben von Daten
help(write.table)

# Bedingte Anweisungen
if(x[1] < y[1]) {
  print("kleiner")
} else {
  print("groesser gleich")
}

# Schleifen
for (i in 1:5) {
  print(i)
}

i <- 1
while (i<=5) {
  print(i)
  i <- i + 1
}

fun <- function(x) {
  return(sin(x) * x)
}
x <- seq(0,10*pi,length=100)
plot(x, fun(x))
plot(x, fun(x), pch=7)

# Grafikparameter koennen auch global gesetzt werden
par(pch=2)
par()

lines(x, fun(x))

dev.off()

# Plotten von Funktionen
plot(fun, 0, 30)

#Weitere interessante Optionen:
help(points)
help(curve) 
help(segments) 

# Verteilungen

#r Zufallszahl 
#d Dichtefunktion 
#p Verteilungsfunktion 
#q Quantilsfunktion
qnorm(0.025)
?distributions

set.seed(148)

plot(density(rexp(1000)), main="Exponential")

# Ziehen aus einer Urne mit und ohne Zuruecklegen
sample(10, size=5, replace=TRUE)
sample(letters[1:5],size=2,replace=F)

fun <- function(x) {
  return(sin(1/x))
}

dev.off()
x <- seq(0, 5, by=0.001)
plot(0,0)
lines(x, fun(x))

# var = sigma^2 / n
K <- 1000
X_var <- vector('numeric')
for (i in 1:10) {
  n <- 1000 * i
  X_mean <- vector('numeric')
  for (k in 1:K) {
    X <- rnorm(n, 0, 1)
    X_mean[k] <- mean(X)
  }
  X_var[i] = var(X_mean)
}
plot(X_var * K)
x <- 1:10
lines(x, 1 / (x))
