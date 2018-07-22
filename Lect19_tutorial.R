# Lecture #19 tutorial
setwd("~/Dropbox/Main/Courses/GE516/Tutorials")
library(MASS)

# Let's do a 2-group LDF problem
iris2 <- subset(iris,iris[,5]!="virginica")                  # new dataframe w/o virginica
colMeans(subset(iris2[,1:4],iris2[,5]=="setosa"))            # get mean vectors for each group
colMeans(subset(iris2[,1:4],iris2[,5]=="versicolor"))    

# plot some of this up
boxplot(subset(iris2[,1:4],iris2[,5]=="setosa"),col="blue",main="Setosa")     
boxplot(subset(iris2[,1:4],iris2[,5]=="versicolor"),col="red",main="Versicolor")

# compute LDF to discriminate setosa from versicolor
set <- as.matrix(subset(iris[,1:4],iris[,5]=="setosa"))      # break out setosa data
set.bar <- colMeans(set)                                     # mean vector for setosa
vers <- as.matrix(subset(iris[,1:4],iris[,5]=="versicolor")) # break out versicolor
vers.bar <- colMeans(vers)                                   # mean vector for versicolor

# now compute pooled covariance matrix
n1 <- dim(set)[1]                                         # number of rows in each species
n2 <- dim(vers)[1]
W1 <- matrix(0,dim(set)[2],dim(set)[2])                   # initialize covariance matrices for each group
W2 <- W1
for (i in 1:n1) {                                         # W matrix for group 1
  delta <- matrix(set[i,]-set.bar,ncol=1)
  W1 <- W1+(delta)%*%t(delta)
}
for (i in 1:n2) {                                         # W matrix for group 2
  delta <- matrix(vers[i,]-vers.bar,ncol=1)
  W2 <- W2+(delta)%*%t(delta)
}
W1                                                        # look at resulting matrix for each group
W2
Spl <- (1/(n1+n2-2))*(W1+W2)                              # pooled covariance matrix
Spl                                                       # take a look!

lin.coefs <- solve(Spl)%*%(set.bar-vers.bar)              # compute coeficients for discriminant function
lin.coefs                                                 # take a look at result
colMeans(subset(iris2[,1:4],iris2[,5]=="setosa"))         # compare with mean vectors
colMeans(subset(iris2[,1:4],iris2[,5]=="versicolor"))

alldat <- rbind(set,vers)                                 # put all the original data in one dataframe

ldf <- alldat%*%lin.coefs                                 # compute linear discriminant = linear combo of original data
hist(ldf,col="red",main="Histogram of LDf")               # holy crap it works!
boxplot(ldf~c(rep("S",50),rep("V",50)),col="red")

## now do mulitple groups, using builtin function in MASS library
iris.lda <- lda(iris$Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,iris)  # estimate model
ldfs <- predict(iris.lda)                                                             # get LDFs
plot(predict(iris.lda)$x,col=as.numeric(iris$Species),pch=16)                         # plot 'em up!
    
#####  and extra example using "crabs" data
head(crabs)
plot(crabs[,4:7],col=as.numeric(crabs[,1]),main="By Species")  
plot(crabs[,4:7],col=as.numeric(crabs[,2]),main="By Gender")  					# plot data

# plot data
plot(crabs[,4],crabs[,5],type="n",	            # look at two variables
     xlab="FL",ylab="RW",main="Species")				# with labels
text(crabs[,4],crabs[,5],crabs[,1])

plot(crabs[,4],crabs[,5],type="n",		          # look at two variables
     xlab="FL",ylab="RW",main="Gender")					# with labels
text(crabs[,4],crabs[,5],crabs[,2])

# now estimate LDF's
crabs.lda <- lda(crabs$sex~FL+RW+CL+CW,crabs)   # lda is part of MASS library
crabs.lda								                        # look at result
names(crabs.lda)
plot(crabs.lda)						                      # plot result - note, plot centers data around 0!
barplot(crabs.lda$scaling,beside=T)             # look at coefficients

# compute manually!
z <- as.matrix(crabs[,4:7])%*%as.matrix(crabs.lda$scaling)
boxplot(split(z-mean(z),crabs$sex),col="red",notch=T)    # center data for plot

