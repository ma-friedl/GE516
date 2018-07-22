# Tutorial #11 - some more advanced matrix algebra!

setwd("~/Dropbox/Main/Courses/GE516/Tutorials")

# Let's look at some multivariate data
dat=as.matrix(read.table("plainspcp.txt")) # 250 stations x 12 months of data
n=dim(dat)[1]  				                     # n = number of rows
n
p=dim(dat)[2]					                     # p = number of cols
p
monmeans=colMeans(dat)  					         # monthly means for all 250 stations
monmeans
barplot(monmeans,col="blue", main="Monthly Mean Precipitation in Great Plains")				# barplot of means
boxplot(dat,notch=T,col="blue",main="Monthly Mean Precipitation in Great Plains")	    # even better!

# covariance matrix and correlation matrix
var(dat[,6])			   # variance of June data
var(dat[,6],dat[,7]) # covariance between June and July (can also use cov(x,y))
cor(dat[,6],dat[,7]) # correlation between June and July
S=var(dat)				   # variance-covariance matrix
S
round(S,2)		       # easier to look at
R=cor(dat)				   # correlation matrix
R
round(R,2)		       # easier to look at

dat2=dat[,-11]		   # remove 11th column
dat2                 # Take a look
dat3=dat[-10,10]		 # October data, with 10th station removed
dat3                 # Take a look

# Matrix algebra
dat[,2]+dat[,3]		    # add precip from Feb and March
dat[,2]%*%t(dat[,3])	# multiply precip from Feb and March

t(dat)					      # take transpose of dat matrix
t(dat)%*%dat			    # multiply transpose of matrix w/original - note this matrix is 12x12!

diag(cor(dat))		    # extract diagonal of correlation matrix
diag(var(dat))		    # extract diagonal of correlation matrix
diag(diag(cor(dat)))	# create diagonal matrix
det(var(dat))         # determinant of covariance matrix
Ds=diag(diag(S)^0.5)  # diagonal matrix w/standard deviations of original variables!
Ds

# Dot product
t(dat[,2])%*%dat[,3]  	          # dot product of precip from Feb and March

# let's compute a linear combination of precip: compute linear sum of 1/2 first 6 months; 2x last 6 months
X=dat                             # use same notation as lecture slides
a=matrix(c(rep(0.5,6),rep(2,6)))  # vector of coefficients
a                                 # check them out!
X%*%a                             # here's our linear combination - 250 values that are a weighted sum of our original matrix!
A=cbind(a,a[12:1])                # now let's add another column (now a 12x2 matrix; second col reversed)
A
X%*%A                             # we now get two linear combinations - one in each column of product!

# rank and linear dependence
qr(X)$rank                        # what's the rank of our precip data
Z=cbind(X,X%*%A)                  # add a couple of columns to our original matrix
dim(Z)                            # now have 14 cols in Z
qr(Z)$rank                        # but the rank is 12!

# normalize vectors; test for orthogonality
a=dat[,1]
b=dat[,2]
plot(a,b)                         # look at relationship
t(a)%*%b                          # test for orthogonality?
c=a/(sqrt(t(a)%*%a))
t(c)%*%c
plot(a,c)                         # same data, just re-scaled

# Inverse of a matrix
S
dim(S)                            # dimensions
qr(S)$rank                        # full rank
S.inv=solve(S)
round(S%*%S.inv,3)

