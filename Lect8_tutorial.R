# regression diagnostics
# weighted regression
# transformation on response variable
# logistic regression

setwd("~/Dropbox/Main/Courses/GE516/Tutorials")

# Regression Diagnostics
# Start with same model we looked at last day
x <- 1:100   		              # x variable (predictor)
y <- x+rnorm(100,0,10)	      # correlated y variable (response)

xy.df <- data.frame(x,y)		  # create dataframe with x and y
xy.df							            # check it out
xy.lm <- lm(y~x,data=xy.df)	  # estimate regression
summary(xy.lm)				        # look at result
plot(xy.lm)                   # hit return to cylce through diagnostic plots

# Now lets compute each of thes manually
xy.lm.fit <- xy.lm$coef[1]+xy.lm$coef[2]*xy.df$x     # what the heck is this syntax?
plot(xy.lm$fit,xy.lm$residuals)                      # xy.lm.fit are the model predictions
xy.lm.resid <- xy.df$y-xy.lm.fit                     # what are we doing here?
plot(xy.lm$residual,xy.lm.resid)                     # xy.lm.resid are the model residuals

# Let's plot residuals vs fitted
hist(xy.lm$residuals)                             # looks relatively Gaussian
plot(xy.lm$fitted.values, xy.lm$residuals,        # Note, it's ok to string commands across multiple lines
     xlab="Fitted Values", ylab="Residuals",      # for the most part, pretty well behaved!
     main="Residuals vs Fitted")                  # and note, we know this sample is drawn from a normal dist!

# Ok, now let's do a QQnorm plot - super fun - Follow basic procedure in lecture notes:
# check out mean and variance of residuals
var(xy.lm$residuals)                                 # variance of residuals
mean(xy.lm$residuals)                                # mean of ressiduals

# need to standardize residuals to N(0,1)
xy.resids<-(xy.lm$residuals-mean(xy.lm$residuals))/var(xy.lm$residuals)^0.5  
resid.sorted<-sort(xy.resids)                        # then sort them
q.i <- matrix(NA,100)                                # create a couple of empty vectors
p.i <- matrix(NA,100)
for (i in 1:100) {
  p.i[i]<-(i-0.5)/100                             # compute percentiles for standard normal
  q.i[i]<-qnorm(p.i[i])                           # ues qnorm to get theoretical quanitiles
}

# put results in a dataframe and take a look
quantiles.df <- data.frame(resid.sorted,p.i,q.i)
colnames(quantiles.df) <- c("Standardized Residual","Percentile","Theoretical Quantile")
head(quantiles.df)

plot(q.i,resid.sorted,xlab="Theoretical Quantiles",     # plot it
     ylab="Standardized Residuals",main="Normal Q-Q")   # add 1:1 line
abline(0,1,lty=3)

