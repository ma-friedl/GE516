# Multiple regression

setwd("~/Dropbox/Main/Courses/GE516/Tutorials")

# Quick examples, univariate regression using ozone data
pairs(Ozone~Solar.R+Wind+Temp,data=airquality, pch=16,col="blue")  # plot up data
summary(lm(Ozone~Solar.R,data=airquality))            # estimate models for Ozone vs Radiation
summary(lm(Ozone~Wind,data=airquality))               # vs Wind
summary(lm(Ozone~Temp,data=airquality))               # vs Temperature
round(cor(airquality,use="pairwise.complete.obs")^2,2)  # look at correlations among Ozone, radiation, wind, temperature

ozone.lm=lm(Ozone~Solar.R+Wind+Temp,data=airquality)  # model to predict ozone as a function of radiation AND temps AND wind
summary(ozone.lm)                                     # check out result and compare with univariate regression

# Let's compute maunally for the univariate case
airquality.2=na.omit(airquality)                      # get rid of any rows with missing data
x=airquality.2$Temp
y=airquality.2$Ozone                         
Sxx=sum((x-mean(x))^2)                                # sum of squares in x
Sxy=sum(y*(x-mean(x)))                                # sum products of y and x-xbar
B1=Sxy/Sxx                                            # Slope
B0=mean(y)-B1*mean(x)                                 # Intercept
summary(lm(y~x))                                      # Compare with builtin....
print(c(B0,B1))

# Let's do multivariate, using a different data set
evap=read.table("evap.txt",header=T)  # read data			   # set up predictors
names(evap)

# take a look at relationships across variables
pairs(Evap~maxst+minst+avst,data=evap,col="red",pch=16)
pairs(Evap~maxat+minat+avat,data=evap,col="red",pch=16)
pairs(Evap~maxh+minh+avh+wind,data=evap,col="red",pch=16)

# estimate multiple regression using all predictor variables
evap.lm=lm(Evap~.,data=evap)	               # use lm- note syntax: "." is shorthand for "all other cols in data
summary(evap.lm)	
# check out results - note: we plot observed vs fitted!!
plot(evap$Evap,predict(evap.lm),pch=16,col="red",         # note "predict" function: generic - works for all models in R
     xlab="True", ylab="Fitted",main="Predicted vs Observed Evaporation")
abline(0,1)

# Now, check out diagnostics using graphical methods
plot(evap.lm)

# now let's do it manually
n=dim(evap)[1]  					                  # n = number of rows
k=dim(evap)[2]                              # k = number of columns
q=k-1                                       # number of predictor variables (first column = response!)
evap.y=evap[,1]                             # evap.y = response (evaporation)
evap.x=evap[,2:k]                           # evap.x = predictors
intercept.dummy=matrix(1,n,ncol=1)		      # set up vector of 1's
X=as.matrix(cbind(intercept.dummy,evap.x))	# set up X matrix
X                                           # take a look
Beta=solve(t(X)%*%X)%*%t(X)%*%evap.y	      # Estimate Beta!
Beta                                        # compare with coefficients from evap.lm
evap.lm$coefficients  				              # compare w/result from lm
cbind(Beta,evap.lm$coefficients)

# Estimate error variance
SSE=t(evap.y)%*%evap.y-t(Beta)%*%t(X)%*%evap.y
SSE
s2=SSE/(n-q-1)                              # mean squared error - take square root to get residual standard error
s2
sqrt(s2)


