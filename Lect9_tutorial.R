# Lecture # 9 Tutorial - a few more examples from linear regression then ANOVA

# First, look at some transformations
Rad <- sample(5:100,250,replace=T)                       # generate some "radiation" data
Ozone <- (5+Rad^3)/10000+rnorm(length(Rad),0,sd=10)+20   # generate some "ozone" data
plot(Ozone~Rad)                                          # obviously non-linear
summary(lm(Ozone~Rad))                                   # but summary stats say it's pretty good!
abline(lm(Ozone~Rad)$coefficients)                       # and linear model doesn't look too bad either
plot(lm(Ozone~Rad))                                      # but check out the diagnostic plots

# Ok - let's try some solutions - start by transforming the response
plot(Ozone^0.33~Rad)                                  # cube root of Ozone?
abline(lm(Ozone^0.33~Rad)$coefficients)
plot(lm(Ozone^0.33~Rad))                              # look at diagnostic plots

# Ok, try transforming the predictor
Rad3 <- Rad^3                                         # new predictor = cube of original
plot(Ozone~Rad3)                                      # whoa, that's more like it!
abline(lm(Ozone~Rad3)$coefficients)
summary(lm(Ozone~Rad3))                               # very strong model
plot(lm(Ozone~Rad3))                                  # residual diagnostic plots look good!

# Logistic Regression
# first let's look at basic form of logistic eqn
x <- seq(-5,5,0.5)                                      # Set up range of arbitrary x
B0 <- 0.5                                               # coefficients in equation
B1 <- 2.0
E_x.y <- (exp(B0+B1*x))/(1+exp(B0+B1*x))                # compute E(x|y) for range of x
plot(x,E_x.y,type="l",lwd=2,col="red")                  # plot it
B1 <- -2                                                # negative slope
E_x.y <- (exp(B0+B1*x))/(1+exp(B0+B1*x))
lines(x,E_x.y,type="l",lwd=2,col="blue")                # plot it

# Let's tranform to linear
eta <- log(E_x.y/(1-E_x.y))                           # compute Eta for values of x
plot(x,eta,ylab="Eta",lwd=2,col="red",type="l")       # ok, that's pretty linear!
lm(eta~x)$coef                                        # slope and intercept of linear model (B0, B1)
eta.hat=lm(eta~x)$fitted                              # estimate model to predict Eta as f(x)
p_hat=exp(eta.hat)/(1+exp(eta.hat))                   # inverse transform back to probabilities
plot(x,p_hat)                                         # Plot result!

# In R we use "glm" because we are estimating from data!
# Lets do Ozone using "airquality data set
rm(Ozone)                                             # First, remove variable we previously created
?airquality
head(airquality)                                      # Check out this builtin data set
airq <- na.omit(airquality)                           # Get rid of missing data
attach(airq)                                          # Makes life easier - don't have to use dataframe name
hist(Ozone)                                           # look quickly at the data
plot(Temp,Ozone)
bad.ozone=as.integer(Ozone>50)                        # let's arbitrarily define Ozone > 50 as "bad", coded as 0,1
plot(Temp,bad.ozone,pch=16,ylab="Bad Ozone")          # wanto to model this relationship
ozone.glm=glm(bad.ozone~Temp,family="binomial")       # estimate glm
plot(Temp,ozone.glm$fitted.values,ylab="P(Bad)",      # plot the model
     col="red",pch=16, main="Prob. Bad O3 vs Temperature")                             

# Analysis of Variance - ANOVA
ozone.low <- subset(airq,airq[,"Temp"]<75)   # Define 3 groups in airq based on Temperature
ozone.med <- subset(airq,airq[,"Temp"]>74 & airq[,"Temp"]<85)
ozone.hgh <- subset(airq,airq[,"Temp"]>84)

# Look at the ditribution of Ozone values in each group
par(mfrow=c(2,2))
hist(Ozone,col="blue",main="All Data",xlab="Ozone PPB")
hist(ozone.low[,"Ozone"],col="blue",main="Temperatures < 75F",xlab="Ozone PPB")
hist(ozone.med[,"Ozone"],col="blue",main="Temperatures > 74F & < 85F",xlab="Ozone PPB")
hist(ozone.hgh[,"Ozone"],col="blue",main="Temperatures > 84F",xlab="Ozone PPB")

# Create a factor for each group and extimate ANOVA
oz.levels <- cut(airq[,"Temp"],breaks=c(55,74,85,99),names=c("Low","Med","High"))
oz.lm <- aov(Ozone~oz.levels)
summary(oz.lm)

# Look at separability of means in each group (I love boxplots!)
par(mfrow=c(1,1))
boxplot(Ozone~oz.levels,col="red",notch=T,ylim=c(0,150),main="Ozone Stratified by Temperature")

# Plot residuals
plot(oz.lm$fitted.values,oz.lm$fitted.values+oz.lm$residuals,
     xlab="Group Means",ylab="Ozone",pch=16,col="red",main="Analysis of Variance")

# Do t-test on each pair of means from different groups
pairwise.t.test(Ozone,oz.levels)

# Another ANOVA -  on builtin dataset "PlantGrowth"
# First, look at data
?PlantGrowth	             # look at documentation
PlantGrowth                # dry weight of plants grown under control and two treatments
names(PlantGrowth)         # Check out names of columns
class(PlantGrowth)         # what type of object is PlantGrowth
class(PlantGrowth$weight)  # what type of object is the weight column
class(PlantGrowth$group)   # What type of object is the group column

# Let's take a look at how the data map to the groups
boxplot(weight ~ group, data = PlantGrowth, main = "PlantGrowth data",
        ylab = "Dried weight of plants", col = "red", varwidth = TRUE)
stripchart(weight~group,data=PlantGrowth,vertical=T,pch=16,col="red")  # different way of looking at data

# Now do ANOVA
pg.lm <- lm(weight~group,data=PlantGrowth)
summary(pg.lm)
anova(pg.lm)

# More meaningful
pg.aov <- aov(weight~group,data=PlantGrowth)
summary(pg.aov)

# Do pairwise comparisons among means
pairwise.t.test(PlantGrowth$weight,PlantGrowth$group)

# Finally, look at some basic, useful functionality in R
# First - Vector math - avoids loops - e.g. sums of squares

# generate some data
x <- rnorm(100,100,5)
y <- rnorm(100,150,5)

# A single line computes pairwise deltas
x-y

# Another example - compute sum of squares in x
x-mean(x)      # don't need a loop!
SSx <- sum((x-mean(x))^2)
SSX

# Finally, lets look at apply - function, which allows you to apply
# any defined function over rows or columns of a matrix or dataframe

x <- matrix(rnorm(500),100,5)     # create matrix
row.means <- apply(x,1,mean)      # estimate means for each row
col.means <- apply(x,2,mean)      # estimate means for each col
row.medians <- apply(x,1,median)  # estimate medians for each row
plot(row.means,row.medians)

# Suppose we have grouped data

x <- rnorm(100)                   # generate some data
grps <- c(rep(1,50),rep(2,50))    # generate some groups
cbind(x,grps)                     # take a look
grp.means <- tapply(x,grps,mean)  # compute means of each group
grp.means                         # take a look


