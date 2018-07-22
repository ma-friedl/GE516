#  Preliminaries before beginning
# 		Commands window
# 		Drop down menus
# 		Workspaces and working directories
# 		Getting help

# Note: hashtag, aka pound sign, identifies text as a comment
# Today: R as a glorified and rather complicated calculator

# First, let's play powerball!
n.combinations.fiveballs <- choose(59,5)
n.combinations.fiveballs
n.powerball <- 35
n.total.combinations <- n.combinations.fiveballs*n.powerball
n.total.combinations

# First - Start with Basics: Objects, Operators, and Assignment
#	R interprets everything entered at prompt 
x                   # sorry, don't know what x is!
7  
8*6/(7+5) + 15^2  	# note precedence, operators: "^" is to the power of

# To create and assign a value to an object: use "=" (or more appropriately "<-"). 

x <- 15  		# create a numeric object "x", give it a value of 15
y <- 15  
x  			  # look at values
y  
x*y  		  # "*" is the multiplication operator
z <- x*y  
z   
x <- 3  		  # if you assign a new value, it overwrites the old one
x  

# Functions vs operators

x <- 1:10	 #  ":" is the sequence operator
x
mean(x)	   # mean is a "built-in" function
?mean		   # get help for mean
help(mean) # same thing

# Navigating commands in R
# 	Use up/down arrows to scroll forwards/backwards among	previous commands; or look at "History" tab 
# 	House keeping - To see the list of objects "in your chapter"environment" use the "objects" or "ls" command: 

ls()  	# list objects that have been created in this session (also listed in "Environment" panel)
 
# To remove an object, use "rm" 

rm(x)  # Remove x - typically don't need to do this
ls()
  
# Some simple functions: c, seq, rep 

x <- c(1,3,4,5)  	      # concatentate - create a list of numeric objects
y <- c(6,7,8,9)  
m <- c("a","b","c")  
x  
y  
m  

# Create sequences using the ":" or "seq" function, providing the upper  
# bound, the lower bound, and the increment as input variables: 

?seq               # look at help page for seq
x <- seq(-5,25)		 # seq from -5, 25 default increment = 1
x
x <- -5:25         # shorthand for conventional sequences
x					         # get the same thing
x <- seq(-5,25,2)  # specify the increment
x 
x <- seq(-5,25,length.out=50)  # more complicated; 50 uniform increments!
x   
 
# To generate repeating patterns, use the "rep" command 
?rep
rep(3,5)  
rep(c(1,2,3),2)     # note the nested syntax!!!!  
x <- c(1,2,3)
rep(x,2)            # same thing as above
 
# Logical/Boolean syntax is also included in the basic language: 
x <- seq(1,20,5)  
x
x>7  
indx <- x>5
indx

# lists of objects in R are indexed:
x <- seq(5,200,3)
x
x[1:6]           # subset first 6 elements in x
x[35:37]         # same idea, different subset

# Note, this syntax can be used to select logical subsets of vectors or other data structures: 
x                # look at x
x[x>10]          # select elements of x > 10
indx <- x>10
x[indx]          # same thing!
sum(x[x>10])     # use builtin function
 
# Working with Data in R - R is "object oriented" - expects everything to have a "class"
# Data Types and Functions: Lists, matrices, and data frames.  

# Simplest datatype is a list of elements
x <- c(1,2,3)              # list of integers 
x
x <- c(1.1,2.2,3.3)        # list of floating point numbers  
x
x <- c(1,2,"x")            # list of characters - note R tries to be "smart" (sometimes, not so much)
x

# Matrices - numeric data only. Use the matrix command to create a matrix.  
x <- 3:8                            # create a list of integers  
x                                   # see what it looks like
?matrix                             # check out matrix command
matrix(x)                           # convert it to a matrix - by default, one column  
matrix(x,3,2)                       # convert x to a matrix with 3 rows, 2 cols  
matrix(x,ncol=2)                    # a couple of different ways  
matrix(x,ncol=3)                    # note the way the columns are filled  
matrix(x,ncol=3,byrow=T)            # compare with above! 

# Ok, let's look at some random numbers, and simple plots
?rnorm              # builtin function to generate random numbers from normal distribution - note default parameters
x <- rnorm(100)			# generate 100 random numbers
plot(x,dnorm(x))  	# density function
plot(x,pnorm(x))		# distribution function

# compute mean of 100 random numbers - noe neted syntax
mean(rnorm(100))		# 95 times out of 100, should be within +/- 1.96 * standard error of mean

y <- NULL             # create an empty object
for (i in 1:1000) y <- c(y,mean(rnorm(100)))
y
hist(y,xlab="Mean of Y",col="red",main="1000 Estimates of Mean of Y Based on Random Samples; 
     n=100, Population Mean, Variance = 0,1")





