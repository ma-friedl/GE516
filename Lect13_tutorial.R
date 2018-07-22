
setwd("~/Dropbox/Main/Courses/GE516/Tutorials")

######## Lecture 13 tutorial - T test on mean vectors #########

# Start with univariate case,sample vs population mean, population variance known 
y <- rnorm(30,0.6,1.0)                  # Create data sets for to work with
ybar <- mean(y)                         # mean of y
ybar
z <- (ybar-0.6)/(1.0/sqrt(length(y)))   # compute z
z                                       # reject Ho if |z| > 1.96 (p=0.05)
abs(qnorm(0.05/2))                      # critical value of z-score p=0.05

# Extend to  multivariate: test mean vector vs population mean; known co-variance
S <- matrix(c(10,1,1,1,10,1,1,1,10),ncol=3,byrow=T)   # covariance matrix
S
mu <- c(10,20,30)                                     # mean vector
n <- 100
Y <- mvrnorm(n,mu,S)                                  # create random sample using function from MASS library
Y
ybar <- matrix(colMeans(Y),ncol=1)                    # mean vector
ybar                       
Z2 <- n*t(ybar-mu)%*%solve(S)%*%(ybar-mu)             # Z^2 Stat
Z2
qchisq(0.95,df=3)                                     # critical value for ChiSq

# Univariate Case: population variance unknown
mu <- 2.5                                             # population mean 
sd.pop <- 1.0                                         # population SD
y <- rnorm(30,mu,sd.pop)                              # generate a sample
ybar <- mean(y)                                       # sample mean
sd.y <- var(y)^0.5                                    # sample sd
t <- (ybar-mu)/(sd.y/sqrt(length(y)))                 # t stat
abs(t)                                
abs(qt(0.05/2,length(y)-1))                           # critical value for t, DF = n-1  

# Extend to multivariate
S <- matrix(c(150,50,2,50,72,4,2,4,1),ncol=3,byrow=T)  # covariance matrix (from Rencher text)
mu <- c(15,6,3)                                        # mean vector
n <- 10                                                # number of samples to draw
p <- 3
Y <- mvrnorm(n,mu,S)                                   # random sample
ybar.samp <- colMeans(Y)                               # sample mean vec
S.samp <- var(Y)                                       # sample covariance 
ybar.samp
S.samp
T2 <- n*t(ybar.samp-mu)%*%solve(S.samp)%*%(ybar.samp-mu) # Hotelling's T2
F.stat <- T2*(n-p)/((n-1)*p)                             # Convert to F stat (see Rencher, eq 5.7, pg 119)
F.stat
qf(.95,p,n-p)                                       # critical value for F

# Comparison of means from different samples - again, start with univariate
ozone <- as.integer(na.omit(airquality$Ozone))         # let's use builtin data
grp <- sample(c(1,2),length(ozone),replace=T)          # set up some groups - random
boxplot(ozone~grp,notch=T,col="red")
ozone1 <- subset(ozone,grp==1)                         # break out the data into separate groups
ozone2=subset(ozone,grp==2)                            # note - "subset" is a useful command!
n1 <- length(ozone1)                                   # number of cases in each group
n2 <- length(ozone2)
ss1 <- sum((ozone1-mean(ozone1))^2)                    # within group sum of squares
ss2 <- sum((ozone2-mean(ozone2))^2)
Spl <- (ss1+ss2)/(n1+n2-2)                             # pooled variance  
t.ozone <- (mean(ozone1)-mean(ozone2))/(Spl*sqrt((1/n1)+1/(n2)))  # t statistic
abs(t.ozone)
abs(qt(0.05/2,n1+n2-2))                                # critical value of t

# Now extend to multivariate case: two sample T^2
air <- na.omit(airquality)[,1:4]                       # first 4 columns in airquality
grp <- sample(c(1,2),dim(air)[1],replace=T)            # same as above: break into groups
air1 <- as.matrix(subset(air,grp==1))
air2 <- as.matrix(subset(air,grp==2))
n1 <- dim(air1)[1]                                     # number of rows in each group
n2 <- dim(air2)[1]
p <- dim(air1)[2]
W1 <- matrix(0,p,p)                                    # set up matrix of covariance matrices for each group
W2 <- matrix(0,p,p)
ybar1 <- colMeans(air1)                                # mean vectors for each groupw
ybar2 <- colMeans(air2)

for (i in 1:n1) {                                   # W matrix for group 1
  delta <- as.matrix(air1[i,]-ybar1,ncol=1)
  W1 <- W1+(delta)%*%t(delta)
}

for (i in 1:n2) {                                   # W matrix for group 2
  delta <- as.matrix(air2[i,]-ybar2,ncol=1)
  W2 <- W2+(delta)%*%t(delta)
}

# take a look at result
W1
W2

# now let's compute T2 statistics
Spl <- (1/(n1+n2-2))*(W1+W2)                           # pooled covariance matrix
delt.ybars <- matrix(ybar1-ybar2)                      # difference in mean vectors 
T2 <- ((n1*n2)/(n1+n2))*t(delt.ybars)%*%solve(Spl)%*%(delt.ybars)  # Hotelling T2
F.stat <- (n1+n2-p-1)*T2/((n1+n2-2)*p)                 # convert to F
F.stat    
qf(.95,p,n1+n2-p-1)                                 # critical value for F-stat

