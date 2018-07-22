# Matrices and matrix algebra in R

# will use builtin "airquality" dataframe for illustration
head(airquality)
dim(airquality)
attach(airquality)

# plot daily time series of each variable
doy=121:273
plot(doy,Ozone,xlab="Day of Year",ylab="Ozone",type="b",pch=16,col="red")
plot(doy,Solar.R,xlab="Day of Year",ylab="Solar Radiation",type="b",pch=16,col="red")
plot(doy,Temp,xlab="Day of Year",ylab="Temperature",type="b",pch=16,col="red")
plot(doy,Wind,xlab="Day of Year",ylab="Wind",type="b",pch=16,col="red")

# now do pairwise scatterplots
plot(airquality[,1:4],pch=16)

# Let's do some basic matrix algebra in R
class(airquality)                  # what type of object?
Y<-as.matrix(airquality[,1:4])     # covert 4 cols of airquality to a matrix
class(Y)                           # object class of Y?
X=t(Y)                             # take transpose
X
X["Ozone",]                        # 1st row 
Y[,"Ozone"]                        # 1st column

# Ok, let's think about some square matrices
S=var(Y,na.rm=T)                   # this is the variance covariance matrix of Y!
S                                  # a square matrix
t(S)                               # and also a symmetric matrix
diag(S)                            # here is the diagonal
diag(diag(S))                      # and here is the diagonal matrix
diag(c(1,1,1,1))                   # a 4x4 identity matrix
S[lower.tri(S)]<-0                 # an "upper" trianangluar matrix
S

# Addition, subtraction of matrices
x=matrix(rnorm(n=100,mean=5,sd=1),nrow=10,ncol=10)          # create two matrices
y=matrix(rnorm(n=100,mean=10,sd=2),nrow=10,ncol=10)
x                                                           # take a quick look
y
x+y                                                         # now add them

# now multiply them:
x*y                                                         # NOTE = this does element-wise multiplication (not matrix multiplication)
5*x                                                         # multiply x by a scalar
x%*%y                                                       # note "%" notation means do matrix multiplication
y%*%x                                                       # order matters!
x=x[,1:8] 
x%*%y                                                       # no go! # rows in y != # rows in x
y=y[1:8,1:4]                                                # extract rows 1-8; now y only has 8 rows
x%*%y                                                       # now it works - result is a 10x4 matrix

# Mean vectors - generally, variables are in columns, joint observations in each row
colMeans(airquality[,1:4],na.rm=T)                          # here is your mean vector for airquality (excluding date)

# variance-covariance, correlation matrices, determinant
var(airquality[,1:4],na.rm=T)                               # here is your variance-covariance matrix
cor(airquality[,1:4],use="pairwise")                        # and here is your correlation matrix
det(var(airquality[,1:4],na.rm=T))                          # the "determinant" of the CVM of airquality
