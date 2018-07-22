# Lecture #18 tutorial
setwd("~/Dropbox/Main/Courses/GE516/Tutorials")

# Indicator regression
dat <- read.csv("lai.ndvi.burn")[,2:4]             # read the data
head(dat)                                          # take a look
plot(dat[,"NDVI"],dat[,"LAI"],col="red",pch=16,
     xlab="NDVI",ylab="LAI")                       # scatterplot
abline(lm(LAI~NDVI,data=dat)$coefficients)         # with best fit line
summary(lm(LAI~NDVI,data=dat))                     # weak, but significant relationship 
boxplot(dat[,"LAI"]~dat[,"Burn"],col="red")        # but clearly a difference due to burning

dat[,"Burn"] <- factor(dat[,"Burn"])               # define "Burn" to be a factor
summary(lm(LAI~NDVI+Burn,data=dat))                # try a regression with an indicator for burn 
plot(dat[,"NDVI"],dat[,"LAI"],
      col=as.integer(dat[,"Burn"])+1,
      xlab="NDVI",ylab="LAI",pch=16)               # now plot it showing burn vs unburned
abline(1.7,2.8,col="red")                          # regression line for unburned (no adjustment for intercept)
abline(1.7+1.3,2.8,col="green")                    # regression line for uburned (shift intercept to account for burning)  

###################################################################
# Linear Discriminant Analysis
library(MASS)                      # will use lda function in MASS library

# first let's rotate a matrix
x <- rnorm(100,10,3)               # create a vector of random values
y <- x+rnorm(100)                  # a correlated vector
xy <- cbind(x,y)                   # create a matrix from x and y
xy                                 # take a look
plot(xy)
z <- matrix(c(1,1,1,-1),ncol=2)    # now create an "orthogonal 2 x 2 matrix
z                                  # take a quick look
a <- z[,1]                         # a, b = vectors, using same notation from lecture
b <- z[,2]
t(a)%*%b                           # mulitply the two vectors together
xy.rot <- xy%*%z                   # mulitply original matrix by orthogonal matrix
plot(xy)                           # plot original data
plot(xy.rot)                       # plot "rotated" data

# note: each column of new matrix is a (rotated) linear combination of original data
cbind(x+y,xy.rot[,1])
cbind(x-y,xy.rot[,2])

# Let's look at Eigen analysis of iris data set. (iris is bulitin to R)
head(iris)                                                # look at IRIS data
plot(iris[,1:4],col=as.numeric(iris[,5]))                 # plot data

plot(iris[,1],iris[,2],col=as.numeric(iris[,5]),          # look at two variables
     xlab="Sepal.Length",pch=16,ylab="Sepal.Width")      	# with labels

iris.S <- var(iris[,1:4])                                 # covariance matrix for iris
iris.eig <- eigen(iris.S)                                 # compute eigen values/eigen vectors
iris.eig$value                                            # look at solutions
iris.eig$vectors
iris.eig$value/sum(iris.eig$value)                        # % variance capture in each eigen vector
barplot(iris.eig$value)                                   # plot magnitude of eigenvalues

iris.rot <- as.matrix(iris[,1:4])%*%iris.eig$vectors             # compute rotated combinations of original data
plot(iris[,1],iris[,2],pch=16,col=as.numeric(iris[,5]))          # plot col 1 vs col 2 in original
plot(iris.rot[,1],iris.rot[,2],pch=16,col=as.numeric(iris[,5]))  # plot col 1 vs col 2 in rotated.
