#######################################################################################
# Today: More useful tools and features:
#   Finish up from last tutorial
#		Adding names to dimensions of matrices (and data frames)
#		Matrices and indexing matrices, functions, "apply" command
#		Data frames, lists, missing values, plotting

######## More basic R ########

# Set up a "working directory" - the folder where R will, by default, read or write data files
getwd()                                           # show the current working directory
setwd("~/Dropbox/Main/Courses/GE516/tutorials/")  # set new working directory
getwd()

# matrices, dimnames, colnames and rownames 
data <- c(227,8,1.3,1534, 58, 1.2, 2365, 82, 1.8)   # create some data 
data                                                # look at it
country.data <- matrix(data, nrow=3, byrow=T)       # convert it to a matrix 
country.data                                        # look at new matrix
dim(country.data)                                   # what are the dimensions? 
dimnames(country.data)                              # currently no names for the dimensions 
countries <- c("austria","france", "germany")       # create a list of row names  
variables <- c("GDP", "POP", "Inflation")           # list of column names  
countries                                           # list of character strings
variables                                           # list of character strings
rownames(country.data) <- countries                 # assign the row names  
colnames(country.data) <- variables                 # assign the column names  
country.data  

# Indexing/Accessing elements in Matrices- Note that this syntax applies to data frames & matrices. 
country.data[2,3]       # 2=row, 3=column

# Similarly, if dimnames have been defined, can use the names: 
country.data["austria", "POP"] 

# To access an entire row or column, indicate row/column name/index followed by a comma
country.data[2,]        # all the data in row 2  
country.data[,2]        # all the data in column 2 

# or use col or row names
country.data[,"POP"]
country.data["austria",]

# If you use an assignment operator, the value in the matrix will be overwritten. 
country.data[1,2]  
country.data[1,2] <- 10    # assign a new value to row 1, column 2  
country.data[1,2] 

# Vector operations vs matrix algebra and math operators 
x <- 1:10  
y <- 2:11 
x+y					  # add all the matching in x and y elements  
x*y					  # multiply all the matching elements 
x %*% y 			# Multiply two vectors using matrix notation produces a scalar result (dot product): "%" tells R do matrix algebra

# Merging matrices: rbind and cbind 
x <- 1:10  
y <- 20:30  
z <- c(x,y)              # concatenate or combine x and y, put the result in z 
z
x <- matrix(1:6,nrow=2)  # create matrices with two rows
y <- matrix(2:7,nrow=2) 
x
y
z <- cbind(x,y)         # bind the columns of y to x  
z  
z <- rbind(x,y)         # bind the rows of y to x  
z 

# Some very (!!!) useful generic functions on matrices 
colMeans(country.data)			# compute the column means  
rowMeans(country.data)			# compute the row means  

# Apply command: apply(data, dim, function) - also EXTREMELY USEFUL
?apply
apply(country.data, 2, max)      # compute the maximun in columns of country.data. 

# Other Data Types: Data Frames - THE SINGLE MOST IMPORTANT AND COMMON TYPE OF R DATA OBJECT
EU <- c("EU", "EU", "non-EU")                   	# a list of character strings 
EU
country.frame <- data.frame(country.data, EU)    # add list to the data frame  
country.frame        			                  	# data frame now includes numeric and character data 

ger.lang <- c("Austria","Belgium","Germany","Switzerland")  
country.list <- list(country.frame,ger.lang)  
country.list 							                    # list object is a "compound" structure

# Functions 
mn <- function(x) {sum(x)/(length(x)-1)}    # define the function, create object "mn"  
mn                                       # x is the input variable 
y <- 1:100  
mn(y)                                    # compute the mean of y 

# Missing Values - identified by the symbol "NA": R provides routines to automatically remove them (na.omit): 
x <- matrix(1:20,ncol=1)     # create a vector  
x[c(10,11)]=NA            # new vector with NA's  
x                         # see what it looks like  
y=na.omit(x)              # remove NA's  
y                         # check out result 




