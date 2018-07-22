# Multiple regression

# Set wd, read data
setwd("~/Dropbox/Main/Courses/GE516/Tutorials")
evap=read.table("evap.txt",header=T)    # read data 
names(evap)

# Compute regression manually
n=dim(evap)[1]    				                  # n = number of rows
k=dim(evap)[2]                              # k = number of columns
q=k-1                                       # number of predictor variables (first column = response!)
evap.y=evap[,1]                             # evap.y = response (evaporation)
evap.x=evap[,2:k]                           # evap.x = predictors
intercept.dummy=matrix(1,n,ncol=1)		      # set up vector of 1's
X=as.matrix(cbind(intercept.dummy,evap.x))	# set up X matrix
X                                           # take a look
Beta=solve(t(X)%*%X)%*%t(X)%*%evap.y	      # Estimate Beta!
Beta                                        # compare with coefficients from evap.lm

# what happens if we drop the vector of 1's in first column of X?
X=as.matrix(evap.x)                         # set up X matrix
head(X)                                     # take a look
Beta=solve(t(X)%*%X)%*%t(X)%*%evap.y	      # Estimate Beta!
Beta                                        # compare with coefficients from evap.lm

# put intercept back in
intercept.dummy=matrix(1,n,ncol=1)  	      # set up vector of 1's
X=as.matrix(cbind(intercept.dummy,evap.x))	# set up X matrix
Beta=solve(t(X)%*%X)%*%t(X)%*%evap.y	      # Estimate Beta

# Now let's use this to compute "fitted values
y.hat=X%*%Beta

# Plot true values vs fitted
plot(evap.y~y.hat,xlab="Fitted Evaporation",ylab="True Evaporation",pch=16,col="red")
abline(0,1)                                 # add 1:1 line
R2=cor(evap.y,y.hat)^2                      # squared correlation between fitted and observed
adj.R2=R2-(1-R2)*q/(n-q-1)
print(paste("Multiple R-squared",round(R2,4),"    Adjusted R-squared:",round(adj.R2,4)))
summary(lm(Evap~.,data=evap))

# And... estimate error variance
SSE=t(evap.y)%*%evap.y-t(Beta)%*%t(X)%*%evap.y    # sum of squared errors
SSE
sum((y.hat-evap.y)^2)                             # compare with sum of squares between preds and true values
s2=SSE/(n-q-1)                                    # mean squared error 
sqrt(s2)                                          # take square root to get residual standard error

# Lets compare sum of square differences in y using two different equations
y=evap$Evap                      # set up y as a vector
sum((y-mean(y))^2)               # traditional estimate
t(y)%*%y-length(y)*mean(y)^2     # equivalent method from lecture notes

# Now let's compute F stat:
SSE=t(y)%*%y - t(Beta)%*%t(X)%*%y       # y'y - B'X'y
SSR=t(Beta)%*%t(X)%*%y - n*mean(y)^2    # B'X'y - n*y_bar^2
F=(SSR/q)/(SSE/(n-q-1))                 # F = ratio, corrected for degrees of freedom
print(paste("SSE = ",round(SSE,1),"SSR = ",round(SSR,1),"F = ",round(F,1)))

# Finally let's do R^2: (B'X'y-ny_bar^2)/(y'y--ny_bar^2)
R2=(t(Beta)%*%t(X)%*%evap.y - n*mean(evap.y)^2)/(t(evap.y)%*%evap.y - n*mean(evap.y)^2)
R2

# compare with output from lm
summary(lm(Evap~.,data=evap))	# compare F, R^2

