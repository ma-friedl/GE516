# Basic probablility using R

# random sampling from a data set
x <- 1:100  			              # generate sequence
y <- sample(x,10)		            # randomly choose (sample) 10 cases
y
y <- sample(x,10,replace=T)		  # now with replacement
sort(y)						              # sort, repeat results - see what happens
y <- sample(x,10,prob=1/x)		  # specify sampling probability
sort(y)						              # see what happens
hist(sample(x,1000,prob=1/x,replace=T),col="blue",main="Histogram")	# plot histogram of large sample w/replacement

# number of combinations of x numbers out of n
choose(10,5)	              # n choose x

# Binomial distribution
n <- 50			                            # number of trials
p <- 0.33			                          # probability of success each trial
x <- 15
f.x <- choose(n,25)*p^25*(1-p)^(n-25)		# binomial probability 
f.x                                     # check out result
x <- seq(0,50,1)			                  # number of successes for a Bernoulli trial
f.x <- choose(n,x)*p^x*(1-p)^(n-x)		  # binomial probability - note vectore notation!
f.x                                     # check out result
plot(x,f.x,type="h",col="red",lwd=2)    # plot it!

# now use builtin function
f.x <- dbinom(x,size=50,prob=.33)				# density for each  value of x 1-50
plot(x,f.x,type="h",col="red",lwd=2)	  # plot it
plot(pbinom(x,size=50,prob=.33),type="h",col="red",lwd=3)	# plot cumulative density

y <- rbinom(5,50,prob=.33)	            # generate 5 random variables from a binomial distribution, p=0.33, n=50 
y
hist(rbinom(1000,50,prob=.33),xlim=c(0,50))  # compare with PDF we just plotted....

# normal distribtion
?rnorm				    # look at help page
x <- rnorm(1000)	# generate 1000 random samples; NOTE, by default mean=0, sd=1
mean(x)           # mean of x
var(x)^0.5        # standard deviation in 

x <- rnorm(1000,mean=300,sd=2) 	# try something different
mean(x)
var(x)^0.5

# Now let's look at the other builtin functions for the normal distribution
# Note that R has four functions for each distribution,each beginning with a "d", "p", "q", or "r", 
# followed by the name of the distribution:
#   r = random sample
#   d = density function
#   p = cumulative probability
#   q = quantile (quantile = value of X corresponding to each value incumulative prob function)

x <-rnorm(100)     # get 100 random samples from a normal distribution
d.x <-dnorm(x)     # calculate value of the PDF for each value of x
plot(x,d.x)        # plot it
curve(dnorm(x),from=-4,to=4)  # cleaner way to plot it

x <-seq(-3,3,0.1)       # create a seqence of values for X
p.x <-pnorm(x)          # compute cumulative probability for for each value of q
plot(x,p.x,col="red")   # plot it

p <-seq(0,1,0.01)  # probabilities, from 0 to 1
p                  # look at values
q.x <-qnorm(p)     # quantiles associated with each probability 
plot(q.x,p)        # plot cumulative probability associated with each quantile
plot(qnorm(p,mean=5,sd=2),p)

# Standard normal distribution
x<-rnorm(1000, mean=100, sd=5)   # 1000 samples - note meand and standard deviation
mean(x)                          # check out sample mean
var(x)                           # sample variance
sd(x)                            # and standard deviation
hist(x,col="red")                # plot histogram
x.sn <- (x-mean(x))/sd(x)        # transform to standard normal distribution: compute "Z" values
mean(x.sn)                       # variance of transformed variable
var(x.sn)                        # mean of transformed variable
hist(x.sn,col="blue")            # histogram of z values

# Ok, now let's demonstrate central limit theorem
x <- rlnorm(1000)                # 1000 random samples from log-normal distribution
hist(x, col="blue")              # histogram - not very normal!
x.mns <- matrix(data=NA,1000)    # create an empty matrix with 1000 elements
for (i in 1:1000) {              # loop 1000 times
  x.mns[i] <-mean(rlnorm(1000))  # each time compute mean of random sample and store it
}
x.mns                           # so what are these #'s?
hist(x.mns,col="red")           # check it out - how cool is that?

# Do again, but where underlying data are drawn from a binomial distribution with n=50, p=0.2
x <- rbinom(1000,50,p=0.3)       # 1000 random samples from log-normal distribution
hist(x, col="blue")              # histogram - not very normal!
x.mns <- matrix(data=NA,1000)    # create an empty matrix with 1000 elements
for (i in 1:1000) {              # loop 1000 times
  x.mns[i] <- mean(rbinom(1000,50,p=0.3))  # each time compute mean of random sample and store it
}
x.mns                           # so what are these #'s?
hist(x.mns,col="red")           # Again, even tho underlying data are binomial, distribution of mean is normally distributed

# Finally, lets illustrate t-test using "snow" example
# First generate some data and plot distributions
sno.b <- rnorm(100,mean=40,sd=6)   # snowfall in Boston
hist(sno.b, col="blue")            # plot histogram
sno.a <- rnorm(100,mean=46,sd=6)   # snowfall in Amherst
hist(sno.a,col="red",add=T)        # plot on top of Boston
hist(sno.b, add=T)                 # plot outline of Boston histogram on top (kludge)

# do t-test
t.test(sno.b,sno.a)

# ok,so now check this out
ntrials <- 10000              # we're going to do something with 10000 trials
t.vals <- matrix(NA,ntrials)  # create vector to store t values for each trial
for (i in 1:ntrials) {
  sno.b <- rnorm(100,mean=43,sd=6)  # 100 random values from nornal distribution with mean =43, standard deviation =6
  sno.a <- rnorm(100,mean=46,sd=6)  # 100 random values from nornal distribution with mean =46, standard deviation =6
  t.vals[i] <- t.test(sno.b,sno.a)$statistic  # do t test for each trial, save t value
}

# Look at distribution of t-values
hist(t.vals)
range(pt(t.vals,df=198))       # what is range of p-values associated with estimated t-values?
sum(pt(t.vals,df=198)>0.05)    # how many p-values are larger than 0.05?

# Finally, let's do a simple example of a p-value - this time from a normal population
x <- rnorm(10000)                 # 10000 random samples from normal distribution
hist(x, col="blue")               # histogram
mean(x)                           # compute mean

# Lets compute a bunch of means in a loop!
x.mns <- matrix(data=NA,10000)       # create an empty matrix with 10000 elements
x.mns                                # take a look
for (i in 1:10000) {                 # loop 10000 times
  x.mns[i] <- mean(rnorm(10000))     # each time compute mean of random sample and store it
}
hist(x.mns,col="red")                # looks like a normal distribution!

# let's convert to standard normal, and look at tails of distribution
x.mns <- (x.mns-mean(x.mns))/sd(x.mns)  # convert to standard normal
hist(x.mns,col="blue")                  # looks about right
sort(x.mns)[1:250]                      # but what is this?
sort(x.mns)[9751:10000]                 # or this?

