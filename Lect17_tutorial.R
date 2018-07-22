# Lecture #17 tutorial
setwd("~/Dropbox/Main/Courses/GE516/Tutorials")

# read "evap" data set
evap <- read.table("evap.txt",header=T)    # read data 

# Lets look at some subsets of the predictor variables
evap.lm <- lm(Evap~.,data=evap)                  # First set up model that includes all the predictors
summary(evap.lm)                                 # summary of full model

# Now do some supsets
evap.lm.sub1 <- lm(Evap~maxst+avst+avh, data=evap)  # Model with three "significant" predictors from full model
summary(evap.lm.sub1)                               # check out result

evap.lm.sub2 <- lm(Evap~maxst+avh, data= evap)      # Model with two "most significant" predictors from reduced model
summary(evap.lm.sub2)

evap.lm.sub3 <- lm(Evap~avh, data= evap)            # Model with one predictor from full model
summary(evap.lm.sub3)                               # compare R2, etc with more complex models....

# To perform F test, use "anova" on model results
anova(evap.lm,evap.lm.sub1)                         # This suggests that we have deleted some meaningful predictors

# Check this out, just for some super big fun!
y <- rnorm(50)
X <- matrix(rnorm(40*50),ncol=40)
summary(lm(y~X))
plot(y,predict((lm(y~X))))

# Now let's look at "some more detailed approaches"all subsets" using Cp
install.packages("leaps")                                                           # install "leaps" package
library(leaps)                                                                      # load the library
evap.leaps <- leaps(x=evap[,2:10],y=evap[,1],method="Cp")
plot(evap.leaps$size,evap.leaps$Cp,xlab="Number of Parameters",ylab="Cp",pch=16,col="blue")
abline(0,1)

# look at Cp data a bit more closely
names(evap.leaps)
cp.dat <- cbind(evap.leaps$size,abs(evap.leaps$Cp-(evap.leaps$size-1)),evap.leaps$which) # dataframe that's easier to look at
rownames(cp.dat) <- 1:length(evap.leaps$size)                                            # add rownames
colnames(cp.dat)[1:2] <- c("Size","Cp-p")                                                # add colnames for p, Cp-p-1
colnames(cp.dat)[3:11] <- colnames(evap)[2:10]                                           # add colnames for predictors  
cp.dat                                                                              # Number of parameters, Cp-p, predictors    
cp.dat[cp.dat[,"Size"]==5,]                                                         # from plot, Cp=5 looks good
summary(lm(Evap~maxst+avst+avat+avh,data=evap))                                     # how does this model do?
summary(lm(Evap~.,data=evap))                                                       # compare to full model, look at R2, Adj-R2

# Could do same thing using Adjusted R^2
evap.leaps <- leaps(x=evap[,2:10],y=evap[,1],method="adjr2")
plot(evap.leaps$size,evap.leaps$adjr2,xlab="Size",ylab="Adj. R2",pch=16,col="blue") # more subjecctive - when does AdjR2 stop increasing?

# Do Stepwise analysis - Note R uses AIC as stopping criteria
library(MASS)

# First do backward selection
evap.lm.bk <- stepAIC(evap.lm,direction="backward")   
evap.lm.bk$anova
summary(evap.lm.bk)

# Then do forward selection
evap.lm.fw <- stepAIC(evap.lm,direction="forward")     
evap.lm.fw$anova
summary(evap.lm.fw)

# Do some partial residual plots - compare models for ozone where we omit radiation, temperature
air <- na.omit(airquality)                             # let's mix this up a bit and look at Ozone again
oz.lm <- lm(Ozone~Wind+Temp+Solar.R,data=air)
plot(oz.lm)                                            # plot model diagnostics

# Now look at partial residual plots for solar radiation
oz.lm1 <- lm(Ozone~Wind+Temp,data=air)                 # just wind and temp as predictors
plot(air$Solar.R,oz.lm1$residuals)                     # plot residuals vs omitted variable
abline(lm(oz.lm1$residuals~air$Solar.R)$coefficients)
summary(lm(oz.lm1$residuals~air$Solar.R))              # model of solar radiation on residuals!

# And, partial residual plots for temperature
oz.lm2 <- lm(Ozone~Solar.R+Wind,data=air)              # radiation and wind as predictors
plot(air$Temp,oz.lm2$residuals)                        # plot temperature (omitted) vs residuals
abline(lm(oz.lm2$residuals~air$Temp)$coefficients)     # looks like there's a relationship btwn residuals and omitted variable
summary(lm(oz.lm2$residuals~air$Temp))                 # temperature explains useful variance
plot(lm(oz.lm2$residuals~air$Temp))                    # but, maybe an issue wrt linearity?
