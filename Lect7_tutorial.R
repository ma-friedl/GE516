# Script for Lecture 7 - Intro Linear Models
# Main goal today: start some statistical modeling

# Set path
setwd("~/Dropbox/Main/Courses/GE516/Tutorials")

# R uses a standard syntax for all statistical models: respone~predictors 
#   Univariate: 	y~x 
#	  Multivariate: y~x+z+w 
# This syntax can be used with or without data frames (see below) 

# Do simple example without data frames; First let's set up some data as before: 

x <- 1:100 			            # x variable (predictor)
y <- x+rnorm(100,0,10)	    # correlated y variable (response)
plot(x,y,pch=16,col="red")  # plot it

# now estimate a regression using the splus command "lm"
lm(y~x)			        # regress y (response) as a function of x (predictor)

# provides a less than helpful response; try this:
summary(lm(y~x))	 # that's more like it!

# more commonly, we assign output to another variable:
xy.lm <- lm(y~x)
xy.lm
summary(xy.lm)
class(xy.lm)           # what type of R object is this anyway?
names(xy.lm)           # note also that there are a lot of things that are hidden in R objects,e.g:
xy.lm$coefficients     # slope, intercept of estimated model
xy.lm$coefficients[1]  # intercept of estimated model
xy.lm$coefficients[2]  # slope, intercept of estimated model
xy.lm$residuals        # residuals of estimated model
xy.lm$fitted.values    # fitted values (predictions) for estimated model

# Data frames: generally, we use data frames to estimate the models
xy.df <- data.frame(x,y)		  # create dataframe with x and y
xy.df							            # check it out
xy.lm <- lm(y~x,data=xy.df)	  # estimate regression
summary(xy.lm)				        # look at result

# Now let's apply some of this to a problem.  Perform the following tasks:
# 1. Read the air quality data set ("air.txt") into a data frame: Check it out to make sure it looks good!
# 2. Estimate a linear model to where ozone is the response and radiation the predictor.
# 3. Look at the results - how do you extract the coefficients, residuals, and fitted values from the model output?
# 4. Plot the original data and the model
# 5. Do some residual analysis - hint see the "resid" and "fitted" functions
#		- plot a histogram of the residuals
#		- plot the residuals against the fitted values
#		- create a quantile- quantile plot.

# Estimate a model for Ozone as a function of Radiation from the air data set.
air <- read.table("air.txt",header=T) # read data - note, this will not work unless you have set the WD!
head(air)			                        # look at first few rows dataframe
tail(air)			                        # look at last few rows dataframe
dim(air)		                          # dimensions
names(air)		                        # variable (column) names
attributes(air)	                      # properties of data frame
class(air)

air.lm <- lm(ozone~radiation,data=air)	# estimate model - NOTE SYNTAX!

summary(air.lm)		    # check out result
names(air.lm)	        # what is stored in the results

# examine some of the output - use "$" syntax to specify elements of air.lm
air.lm$coefficients
air.lm$residuals
air.lm$fitted.values

# plot the darn thing:

plot(air$radiation, air$ozone)  # again, note the syntax for referencing columns in a dataframe
abline(air.lm)					        # abline plots a line using given slope,intercept (but note other options!)
abline(air.lm$coefficients[1],air.lm$coefficients[2])					        # more explicitly

# can use "resid" and "fitted" to extract fitted values and residuals
hist(resid(air.lm)) 						    # plot histogram of residuals 
plot(fitted(air.lm),resid(air.lm)) 	# plot residuals against fitted values 
qqnorm(resid(air.lm)) 					    # qqnorm plot of residuals 
qqline(resid(air.lm))               # More on these plots soon!

# do confidence intervals
air2=air         # create new data frame
air2[,"radiation"]=air2[,"radiation"]+rnorm(nrow(air2))  # add noise to radiation
predict(air.lm,new=air2,interval="confidence")           # confidence interval on mean ozone
predict(air.lm,new=air2,interval="prediction")           # confidence interval on future observation of ozone

# finally, can just plot output from model
par(mfrow=c(2,2))
plot(air.lm)

