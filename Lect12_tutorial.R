# Lecture 12 tutorial

setwd("~/Dropbox/Main/Courses/GE516/Tutorials")

# read in the precip data
dat <- as.matrix(read.table("plainspcp.txt")) # 250 stations x 12 months of data

# Let's define a function - Univariate normal density function
var                     # before doing so, check under the hood of a builtin function
dnrm <- function(x,mu,sd) {
  1/(sqrt(2*pi)*(sqrt(sd^2))) * exp(-(x-mu)^2/(2*sd^2))
}
dnrm

# Plot some sample densities (could also use "dnorm to do this!)
x <- seq(-4,4,0.1)
plot(x,dnorm(x,mean=0,sd=1),type="l",col="red",ylim=c(0,1),ylab="P(x)")
points(x,dnrm(x,mu=0,sd=1),col="blue")
points(x,dnrm(x,mu=0,sd=2),col="blue")
points(x,dnrm(x,mu=1,sd=0.5),col="blue")

# Now let's compute MV Normal density for first row in data matrix
S <- var(dat)                                 # covaraince matrix for precip data
p <- dim(dat)[2]                              # p is the number of columns in the matrix
monmeans <- colMeans(dat)                     # mean vector for precip data
diff <- matrix(dat[1,]-monmeans,ncol=1)       # difference between value of each variable in row 1 and mean vector
diff                                          # take a look
g.y <- (1/((sqrt(2*pi)^p))*det(S)^0.5)*exp(-t(diff)%*%solve(S)%*%(diff)/2)
g.y                                        # value of density function for first row

# Now let's demonstrate the "apply" command to compute column means:
monmeans2 <- apply(dat,2,mean)         # compute means of each column
cbind(monmeans,monmeans2)              # compare with result of from colMeans
rowmeans <- apply(dat,1,mean)          # compute means of each row
rowmeans
monvars <- apply(dat,2,var)            # compute variance of each column
monvars

# Now let's compute Mahalanobis Distance for 1st row
delta <- matrix(dat[1,]-monmeans) 	    # p-dimensional difference with mean vector
Dsq <- t(delta)%*%solve(S)%*%delta      # delta must be a column vector for algebra to work
Dsq                                     # here's the result

# Ok, let's compute Mahalanobis distance for every point!
n <- dim(dat)[1]                            # number of rows in the precip data matrix
Dsq <- matrix(NA,n)								          # intialize a vector (all zeros) to store results
for (i in 1:n) {
  delta <- matrix(dat[i,]-monmeans)			    # vector of differences for each row
  Dsq[i] <- t(delta)%*%solve(S)%*%(delta)	  # compute D^2
}
Dsq		                                      # Look at result
hist(Dsq,col="blue")

# Generalized population variance
S <- var(dat)                             # covariance matrix for precip data
det(S)                                    # determinant of S - but what does this mean?
round(cor(dat),2)                         # check out correlation matrix - there is a lot of correlation here
det(var(matrix(rnorm(12*250),ncol=12)))   # so what is this?

# Let's compute covariance matrix manually
nrows <- dim(dat)[1]
ncols <- dim(dat)[2]
S <- matrix(0,dim(dat)[2],dim(dat)[2])  # initialize 12 x 12 matrix with 0's
for (i in 1:nrows) {
  S <- S+(dat[i,]-monmeans)%*%t(dat[i,]-monmeans)
}
S <- S/(nrows-1)                        # mulitply by 1/(n-1)
round(S,2)                              # compare with builtin function
round(var(dat),2)

# Assessing multivariate normality
pairs(dat)                                      # start with visual inspection
par(mfrow=c(2,2))
plot(dat[,3]~dat[,7],xlab="July",ylab="March") 
plot(dat[,3]~dat[,5],xlab="May",ylab="March")   # hmm, maybe not so linear?
qqnorm(dat[,3]); qqline(dat[,3])                # and maybe not so univariate normal?
qqnorm(dat[,5]); qqline(dat[,5])

# Now let's look at a more quanitative way to do this - compare ui's with Beta
monmeans <- colMeans(dat)                       # compute mean vector
Dsq <- matrix(0,nrows)    						          # intialize matrix to store results
for (i in 1:nrows) {
  delta <- matrix(dat[i,]-monmeans)			        # vector of differences for each row
  Dsq[i] <- t(delta)%*%solve(S)%*%(delta)	      # compute D^2
}
Dsq                                             # 250 values, one for each row in original matrix
hist(Dsq,col="red",main="Freq. Distribution of D^2") # Quick scan of freq. distribution

# Ok, now transform to u_i's
par(mfrow=c(1,2))               # reset plot window
u <- nrows*Dsq/(nrows-1)^2  				# compute ui's for each row in "dat" based on standardized distance - note vector syntax!
alpha <- 0.5*ncols  					      # 1st parameter for Beta (p=ncols)
rho <- 0.5*(nrows-ncols-1)					# second parameter for Beta
hist(u,col="red")                   # ok, maybe beta-distributed, but let's look more carefully
plot(seq(0,0.30,0.01),dbeta(seq(0,0.30,0.01),alpha,rho),ylab="P(u)",xlab="u",pch=16,col="blue")

# But,let's do this more carefully - use qqplot to compare ui's vs Beta
qqplot(rbeta(nrows,alpha,rho),u,col="red",pch=16)	# qqplot for u against Beta distribution
abline(0,1)                                       # probably not normally distributed

# Let's see if we can look for some outliers; Use X3 (see below), which is sampled from MVN PDF 
# to do this, first have to cover libraries in R: some builtin, some available as "packages" 
?library
library()                                              # list all available libraries
library(help=MASS)                                     # this library has some useful stuff!

library(MASS)                                          # load "MASS" library - comes with "base R"
S=matrix(c(10,7,7,7,10,7,7,7,10),ncol=3,byrow=T)       # Prescribe covariance matrix
S                                                      # take a look
mu=c(10,20,30)                                         # prescribe mean vector  

# use mvnorm to generate random sample - Note: mvnorm lives in MASS librabry
X3=mvrnorm(1000,mu,S)         # sample 1000 points from MVN   
X3
pairs(X3)                     # Plot

n=dim(X3)[1]  							  # n rows in X3
p=dim(X3)[2]  							  # p cols in X3
Dsq=matrix(0,n)								# intialize matrix to store results
S=var(X3)
for (i in 1:n) {
  delta=matrix(X3[i,]-colMeans(X3))			# vector of differences for each row - same as above for "dat"
  Dsq[i]=t(delta)%*%solve(S)%*%(delta)	# compute D^2
}

# Again, compare ui's with Beta - Note: repeat this a few times to see what happens!
u=n*Dsq/(n-1)^2					      # compute ui's
alpha=0.5*p						        # 1st parameter for Beta
rho=0.5*(n-p-1)					      # second parameter for Beta
qqplot(rbeta(n,alpha,rho),u,col="red",pch=16)	# qqplot for u against Beta distribution
abline(0,1)

# Finally, compute Wilk's statistic
w=1-(n*Dsq)/(n-1)^2                   # compute w
w                                     # take a look - one w_i for each row in X3
F.w=((n-p-1)/p)*(1/w -1 )             # compute F stat for each w_i
round(pf(F.w,p,n-p-1),3)              # get p value for each row, based on F distribution
plot(F.w,round(pf(F.w,p,n-p-1),3))    # look at distribution
sum(pf(F.w,p,n-p-1)>0.975) + sum(pf(F.w,p,n-p-1)<0.025)            # total number of cases, outside of p < 0.05
