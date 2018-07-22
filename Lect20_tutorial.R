# Lecture #20 tutorial
setwd("~/Dropbox/Main/Courses/GE516/Tutorials")

# LDA for classification

# Two class iris problem from last day
iris2 <- subset(iris,iris[,5]!="virginica")               # new dataframe w/o virginica

# Hold out 10 cases that we are going to use LDA to classify
smpl <- sample(1:100,10)                                  # row index for ten random samples
iris2.test <- iris2[smpl,]                                # data to "test" LDF classification
iris2.train <- iris2[-smpl,]                              # data to estimate LDF
dim(iris2.test)

plot(rbind(iris2.train[,1:4],iris2.test[,1:4]),pch=16, # plot data, showing points to be classified!
     col=c(as.numeric(iris2.train[,5]),rep(3,10)))            

# estimate LDF - just like last day
set <- as.matrix(subset(iris2.train[,1:4],iris2.train[,5]=="setosa"))      # break out setosa data
set.bar <- colMeans(set)
vers <- as.matrix(subset(iris2.train[,1:4],iris2.train[,5]=="versicolor")) # break out versicolor
vers.bar <- colMeans(vers)

# first get pooled covariance across groups
n1 <- dim(set)[1]                                         # number of rows in each species
n2 <- dim(vers)[1]
W1 <- matrix(0,dim(set)[2],dim(set)[2])                   # initialize covariance matrices for each group
W2 <- W1
for (i in 1:n1) {                                      # W matrix for group 1
  delta <- matrix(set[i,]-set.bar,ncol=1)
  W1 <- W1+(delta)%*%t(delta)
}
for (i in 1:n2) {                                      # W matrix for group 2
  delta <- matrix(vers[i,]-vers.bar,ncol=1)
  W2 <- W2+(delta)%*%t(delta)
}
Spl <- (1/(n1+n2-2))*(W1+W2)                              # pooled covariance matrix

# now estimate coefs for LDF
lin.coefs <- solve(Spl)%*%(set.bar-vers.bar)              # compute coeficients for discriminant function
lin.coefs                                              # take a look at result
 
# compute z-values for each group from known data, and for unknown cases
z1 <- set%*%lin.coefs                                                                # z values for setosa
z2 <- vers%*%lin.coefs                                                               # z values for versicolor
z.unknown <- as.matrix(iris2.test[,1:4])%*%lin.coefs                                 # z values for test group
compare.df <- data.frame(z.unknown,rep(mean(z1),10),rep(mean(z2),10),iris2[smpl,5])  # compare results in a single data frame
colnames(compare.df) <- c("z","zbar.setosa","zbar.versicolor","Species")             # add colnames
compare.df                                                                        # take a look

