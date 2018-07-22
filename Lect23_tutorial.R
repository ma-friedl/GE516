# Tutorial 23 - Canonical Correlation
setwd("~/Dropbox/Main/Courses/GE516/Tutorials")

# First Get some data
dat=read.table("evap.txt",header=T)
x=dat[,2:4]     # "soil" variables
y=dat[,5:11]		# "air" variables

# Let's look at relationships within x and y
pairs(x,pch=16,col="blue")
pairs(y,pch=16,col="blue")

# What happens if we do PCA on each?
x.pca=predict(princomp(x))                     # compute PC's on x 
y.pca=predict(princomp(y))                     # compute PC's on y
plot(x.pca[,1],y.pca[,1],pch=16,col="red",
     xlab="1st PC of X",ylab="1st PC of Y")    # plot 1st PC for x vs 1st PC for y

# Now do CCA
xy.cca=cancor(x,y)           # estimate model
names(xy.cca)                # what is in the result?
xy.cca$cor                   # correlation between pairs of canonical factors
xy.cca$xcoef                 # coefficients for X
xy.cca$ycoef                 # coefficients for Y (note: cols 4-7 are meaningless)

par(mfrow=c(1,2))            # plot the coefs for 1st canonical pair of CFs in a barplot
barplot(xy.cca$xcoef[,1])
barplot(xy.cca$ycoef[,2])

x.cf=as.matrix(x)%*%xy.cca$xcoef         # compute canonical factors on X
y.cf=as.matrix(y)%*%xy.cca$ycoef[,1:3]   # compute canonical factors on Y (only use 1st 3!)
round(cor(x.cf,y.cf),2)                  # What does this show us?
xy.cca$cor
par(pty="s")
plot(x.cf[,1],y.cf[,1],xlab="First Canonical Factor in X",    # look at some scatterplots!
     ylab="First Canonical Factor in Y",pch=16,col="blue")
plot(x.cf[,2],y.cf[,2],xlab="Second Canonical Factor in X",
     ylab="Second Canonical Factor in Y",pch=16,col="blue")

par(mfrow=(c(1,1)))
# now look at some plots where time series of canonical factors are overlaid
plot(1:dim(x.cf)[1],x.cf[,1]-mean(x.cf[,1]),typ="b",col="red",xlab="Time Step",ylab="Canonical Factors")
lines(1:dim(x.cf)[1],y.cf[,1]-mean(y.cf[,1]),col="blue")


#Now do it manually
Syy=var(y)			# Covariance matrix for air vars
Sxx=var(x)			# Covariance matrix for soil vars
Sxy=var(x,y)		# Covariance of soil w/each air var
Syx=var(y,x)		# Covariance of air w/each soil var

# compute eigen values and vectors
CCAy=eigen(solve(Syy)%*%Syx%*%solve(Sxx)%*%Sxy)
CCAx=eigen(solve(Sxx)%*%Sxy%*%solve(Syy)%*%Syx)

# check out results
CCAx
CCAy               # note - we get complex numbers with imaginary and real parts - just ignore

# Now use results to compute facotrs
x.cf=as.matrix(x)%*%as.matrix(CCAx$vectors)         # canonical factors on X
y.cf=as.matrix(y)%*%as.matrix(CCAy$vectors)[,1:3]   # canonical factors on Y (only use 1st 3 eigenvectors!)

par(mfrow=c(1,2))
plot(x.cf[,1],y.cf[,1],xlab="First Canonical Factor in X",    # look at some scatterplots!
     ylab="First Canonical Factor in Y",pch=16,col="blue")    # same thing, just not centered
plot(x.cf[,2],y.cf[,2],xlab="Second Canonical Factor in X",
     ylab="Second Canonical Factor in Y",pch=16,col="blue")


