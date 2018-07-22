# Basic graphics controls, loops, and I/O

# Set your working directory!
setwd("~/Dropbox/Main/Courses/GE516/Tutorials")

# Basic Graphics - the plot command: 
x <- 1:100  
y <- x+rnorm(100,0,10)            # Add some random noise to x; will come back to this
plot(x,y)                         # plot it! 

# Add some more sophisticated options.  
plot(x,y,xlab="Temperature",ylab="Precipitation", main = "Climate Data")              # add labels
abline(0,1)                                                                           # plot line with intercept=0, slope=1
abline(h=50,col="red")                                                                # horizontal and vertical lines, add color
abline(v=50,col="blue")

# different types of lines
plot(x,y,xlab="Temperature",ylab="Precipitation", main = "Climate Data", type="l")    # line  
plot(x,y,xlab="Temperature",ylab="Precipitation", main = "Climate Data", type="b")    # both  
plot(x,y,xlab="Temperature",ylab="Precipitation", main = "Climate Data", type="h")    # height  
plot(x,y,xlab="Temperature",ylab="Precipitation", main = "Climate Data", type="n")    # don't plot points  
points(x,y,pch=16,col="red")                                                          # add the points in red

# The "par" command allows a very (!) wide suite of options to be used for creating graphics.  
?par 
par(mfrow=c(2,2),pty="s")                                                                   # two rows and two columns for plotting; square plots
plot(x,y,xlab="Temperature", ylab="Precipitation", main = "Climate Data", type="l",cex=0.5)  # cex changes font size on text
plot(x,y,xlab="Temperature", ylab="Precipitation", main = "Climate Data", type="b",cex=0.5)                 
plot(x,y,xlab="Temperature", ylab="Precipitation", main = "Climate Data", type="h",cex=0.5)         
plot(x,y,xlab="Temperature", ylab="Precipitation", main = "Climate Data", type="n",cex=0.5)  
points(x,y,pch=1,cex=0.5) 

# one more example for you to explore on your own w/richer options: 
x <- seq(0,2*pi,length=21)  
y <- sin(x)                                # hopefully obvious; note trig functions expect radians! 
par(mfrow=c(1,1))                       # reset the number of rows and columns in the display
plot(x,y, axes=F, type="b",pch="x",xlab="",ylab="") 
# do some sophisticated labeling of axes using "axis" 
axis(1,at=c(0,1,2,pi,4,5,2*pi),labels=c(0,1,2,"Pi",4,5,"2 Pi"),pos=0)  
axis(2,at=c(-1,-0.5,0.25,0.5,0.75,1),adj=1)  
abline(h=c(-1,-0.5,0.5,1),lty=3) 

##############################################################################
#
# Another big topic: programming control structures - brace brackets, if/else, loops
#

x <- matrix(rnorm(100),ncol=10)         # a matrix to work with in example
x.row.means1 <- matrix(NA,10)
x.row.means2 <- matrix(NA,10)
x.row.means3 <- matrix(NA,10)

for (i in 1:10) {                      # do everything inside the brackets for each value in i
  print(paste("Mean of row",i,"=",mean(x[i,])))
  x.row.means1[i] <- mean(x[i,])
  x.row.means2[i] <- mean(x[i,x[i,]>0]) 
  if (i==2 | i==4 | i==6 | i==8 | i==10) {
    print(paste("Even Row",i))
    x.row.means3[i] <- mean(x[i,])       
  }
  else {
    x.row.means3[i] <- mean(x[i,])
  }
}

# check out results
x.row.means1
x.row.means2
x.row.means3

# Finally, let's do some t-tests - First generate data with known properties and plot
x <- rnorm(1000,0.5,1.0)
y <- rnorm(1000,0.6,1.0)
boxplot(x,y,notch=T,col=c("blue","red"))

# Now do t-test
t.test(x,mu=1.0)						            # H0: mean = 1.0; two-sided
t.test(x,alternative="less",mu=1.0)		  # H1: mean of x is < 1.0	
t.test(x,alternative="greater",mu=1.0)	# H1: mean of x is > 1.0
t.test(x,y,alternative="less")			    # H1: mean of x < mean of y

# Finally: getting data in/out of R
# Main functions: scan and read.table, write, write.table
# write and scan: write/read a vector of data

x <- rnorm(100)  		  # create random data
write(x,"test.txt")	  # write as vector to a file
rm(x) 					      # remove it
x <- scan("test.txt")	# read it back in
x						          # take a look
x <- matrix(x,ncol=10)	          # now turn it into a matrix
write.table(x,file="test.txt")	  # write as matrix to a file
y <- read.table("test.txt")			  # read it as a matrix

# More complicated example: read/write a matrix/dataframe with headers
x <- read.table("air.txt", header=T) 	# Note: this creates a dataframe!!
x									
write.table(x,"test.txt")

# Read/write excel-format files - read/write.csv
write.csv(x,"test.csv")
read.csv("test.csv")

