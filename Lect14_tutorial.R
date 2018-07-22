# Lecture #14 Tutorial

setwd("~/Dropbox/Main/Courses/GE516/Tutorials")
library(MASS)                                       # load MASS library - we'll need this!

# Paired observation test - do univariate case first!
x=rnorm(100,10,10)                                  # one sample
y=x+rnorm(100,0.5,5)                                # second sample, correlated w/first sample, shift mean by 5 units
n=length(x)
boxplot(x,y,notch=T,col="red")                      # compare distributions and look at covariance - means look different?
plot(x,y,pch=16,col="red")

# compute paired t-stat
d=x-y                                               # take difference
d.bar=mean(d)                                       # mean of d
S.d=sqrt( (1/(n-1)) * sum((d-d.bar)^2) )            # standard deviation in d
t=d.bar/(S.d/sqrt(n))                               # t value
abs(t)
abs(qt(0.05/2,length(d)-1))                         # critical value of t-stat
# compare with builtin function
t.test(x,y,paired=T)                                # depending on specific random sample, can go either way!

# Extend to multivariate paired observations test;  make up some data and do test
S=matrix(c(10,1,1,1,10,1,1,1,10),ncol=3,byrow=T)    # covariance matrix
S
mu.x=c(10,20,30)                                    # set up a couple of mean vectors - similar!
n=50
p=length(mu.x)
X=mvrnorm(n,mu.x,S)                                 # generate random draws from multivariate normal
X
Y=X+matrix(rnorm(n*p,1,2),ncol=3)                   # create a matrix with correlated columns
par(mfrow=c(1,3),pty="s")
plot(X[,1],Y[,1]); plot(X[,2],Y[,2]); plot(X[,3],Y[,3])   # doesn't look independent!
colMeans(X)
colMeans(Y)                                         # are these mean vecs statistically different from each other
d=X-Y                                               # difference matrix
d.bar=apply(d,2,sum)/(n-1)                                   # mean vector on difference matrix
S.d=matrix(0,p,p)                                   # initialize matrix for S.d
for (i in 1:n) {                                    # compute S.d
  delta=as.matrix(d[i,]-d.bar,ncol=1)
  S.d=S.d+(delta)%*%t(delta)
}
S.d=S.d/(n-1)                                       # normalize for degrees of freedom
T2=n*t(d.bar)%*%solve(S.d)%*%(d.bar)                # T2

# convert to F and compute critical value of F
F.stat=T2*((n-p)/((n-1)*p))
F.stat
qf(.95,p,n-p)

# MANOVA - demonstrate using "rootstock" data from Rencher text
rootdat=read.table("T6.2.ROOT.txt")    # read data
dimnames(rootdat)=list(c(1:48),c("Rootstock","y1","y2","y3","y4"))   # set up dimnames
head(rootdat)                          # take a look

attach(rootdat)  						           # This way can refer to variables in dataframe
Rootstock
Rootstock=factor(Rootstock)				     # define rootstock variable as a factor
Rootstock
rootdat[,1]=Rootstock				     	     # put factor in 1st column of data frame

# do some boxplots of data
par(mfrow=c(2,3))
# boxplots of each variable for each group
boxplot(rootdat[Rootstock==1,2:5],col="red")
boxplot(rootdat[Rootstock==2,2:5],col="red")
boxplot(rootdat[Rootstock==3,2:5],col="red")
boxplot(rootdat[Rootstock==4,2:5],col="red")
boxplot(rootdat[Rootstock==5,2:5],col="red")
boxplot(rootdat[Rootstock==6,2:5],col="red")

par(mfrow=c(2,2))
# now do boxplots of each variable, by each group
boxplot(y1~Rootstock,notch=T,col="red",main="Trunk Girth at Four Years")	
boxplot(y2~Rootstock,notch=T,col="red",main="Extension Growth at Four Years")
boxplot(y3~Rootstock,notch=T,col="red",main="Trunk Girth at Fifteen Years")
boxplot(y4~Rootstock,notch=T,col="red",main="AGB at Fifteen Years")

# do MANOVA; y1-y4 depedent on Rootstock
rt.manova=manova(cbind(y1,y2,y3,y4)~Rootstock,data=rootdat)	
summary(rt.manova, test="Pillai")		# Extract test statistic for  
summary(rt.manova, test="Wilks")		# pillai, wilks, hotelling
summary(rt.manova, test="Hotelling")

# Now do it manually
# First, define functions to compute E and H matrices 
getH=function(x,grps) {     # function to compute H matrix
  # x= n x p matrix with data; grps = vector with group ID for each row in x
  k=length(unique(grps))    # number of groups
  p=dim(x)[2]               # number of variables
  n=dim(x)[1]/k             # number of cases in each group - assume balanced!
  
  yidot=matrix(0,p,k)       # sum for each variable in each group
  for (i in 1:p){
    yidot[i,]=tapply(x[,i],grps,sum)    # tapply = useful variant on apply function
  }
  
  yibardot=yidot/n                      # mean for each variable in each grp
  ybardotdot=matrix(colMeans(x),ncol=1) # overall mean for each variable
  
  H=matrix(0,p,p)           # initialize
  for (i in 1:k) {          # compute H 
    y=matrix(yibardot[,i],ncol=1)
    vec=matrix(y-ybardotdot,ncol=1)
    H=H+(vec)%*%t(vec)
  }
  H=n*H	
  
  return(H)
}

getE=function(x,grps) {     # subroutine to compute E matrix
  k=length(unique(grps))    # number of groups
  p=dim(x)[2]               # number of variables
  n=dim(x)[1]/k             # number of cases; again assume balanced
  
  yidot=matrix(0,p,k)       # sum for each variable in each group
  for (i in 1:p){			
    yidot[i,]=tapply(x[,i],grps,sum)	
  }
  
  yibardot=yidot/n          # mean for each variable in each grp
  
  E=matrix(0,p,p)           # intialize 
  for (i in 1:(k*n)) {      # compute E 
    indx=grps[i]
    yij=matrix(x[i,],ncol=1)
    ybar=matrix(yibardot[,indx],ncol=1)
    vec=matrix(yij-ybar,ncol=1)
    E=E+((vec)%*%t(vec))
    }
  return(E)                 # return result
}

getH                                   # check them out
getE
H=getH(cbind(y1,y2,y3,y4),Rootstock)   # compute H
H                                      # check it out
E=getE(cbind(y1,y2,y3,y4),Rootstock)   # compute E
E                                      # check it out
det(E)/det(E+H)                        # wilk's statistic




