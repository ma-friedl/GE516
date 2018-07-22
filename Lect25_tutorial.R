# Lecture 25 tutorial script - tree-based models
setwd("~/Dropbox/Main/Courses/GE516/Tutorials")

install.packages("tree")                   # install package (rpart is also widely used)
library(tree)                              # load library

# Estimate a Classification Tree - response is a factor, R estimates a classification tree!
iris.tree <- tree(Species~.,data=iris)        # ok, lets estimate a tree - note standard model syntax
iris.tree                                     # look at result
plot(iris.tree)                               # plot the tree - easier to visualize this way!
text(iris.tree, cex=0.8)                      # add text
plot(iris[,"Petal.Length"],                   # visualize in 2-D
     iris[,"Petal.Width"], 
     pch=16,col=iris[,5])

# now let's look at predictions
Preds <- predict(iris.tree,type="class")
conf.matrix <- table(Preds, iris[,"Species"]) # confusion matrix
conf.matrix
1-sum(diag(conf.matrix))/dim(iris)[1]         # error rate on classification
Preds <- predict(iris.tree)                   # don't specify class
Preds                                         # check it out - likelihoods for each class!

# now lets look at cross-validation
iris.tree.cv <- cv.tree(iris.tree)            # cross-validate the result
plot(iris.tree.cv)                            # and plot that
iris.prune <- prune.tree(iris.tree,best=3)    # 3 leaf nodes!
plot(iris.prune)                              # plot it up
text(iris.prune)
Preds <- predict(iris.prune,type="class")     # predictions based on pruned tree
table(Preds, iris[,"Species"])             # confusion matrix

# Ok, now let's do a Regression Tree!
evap <- read.table("evap.txt",header=T)       # let's look at our old friend evaporation
evap.tree <- tree(Evap~.,data=evap)           # response is continuous, therefore a regression tree!
plot(evap.tree)
text(evap.tree,cex=0.8)
plot(evap[,"Evap"],predict(evap.tree),col="red",pch=16,  # plot predicted vs observed
     xlab="Measured Evap", ylab="RT Prediction")
abline(0,1)
r2 <- round(cor(evap[,"Evap"],predict(evap.tree)^2),2)      # add squared correlation (R^2)
text(10,45,paste("R^2 =",r2))

# now cross-validate and explore over-fitting
evap.tree <- tree(Evap~.,data=evap,mindev=0,minsize=2)     # use fine control to grow a large tree
plot(evap.tree,type="uniform")                          # uniform allows lower levels to be visible
text(evap.tree,cex=0.8)                                 # one leaf node for every case 
plot(evap[,"Evap"],predict(evap.tree),col="red",pch=16, # plot predicted vs observed
     xlab="Measured Evap", ylab="RT Prediction")        # really good, or over-fitted!
abline(0,1)

# so let's do some cross-validation
plot(cv.tree(evap.tree))                 # output from cross-validation - little benefit beyond 5 leaf nodes
evap.prune <- prune.tree(evap.tree,best=4)  # prune large tree to have 4 leaf nodes
plot(evap.prune)                         # plot tree
text(evap.prune)
plot(evap[,"Evap"],predict(evap.prune),col="red",pch=16,  # plot predicted from prunes tree vs observed
     xlab="Measured Evap", ylab="RT Prediction")          
r2 <- round(cor(evap[,"Evap"],predict(evap.prune)^2),2)      # add squared correlation (R^2)
text(10,45,paste("R^2 =",r2))
abline(0,1)

# finally, let's do some ensemble trees
n <- dim(evap)[1]                  # n = number of rows in evap
preds <- matrix(0,n,100)           # set up empty matrix to store predictions
# now do bootstrap aggregation
for (i in 1:100) {
	indx <- sample(1:n,replace=T)                # select random set of rows, with replacement!
	dat <- evap[indx,]                           # put those rows in a temporary dataframe
	tree.tmp <- tree(Evap~.,data=dat)            # estimate a tree
	preds[,i] <- predict(tree.tmp,newdata=evap)  # predict on original data, and save result
	}                                         # repeat 100 times

round(preds,2)                                    # check out result: 46 rows, each with 100 unique predictions
hist(preds[1,])                                   # distribution of predictions on first case
evap[1,1]                                         # true value
preds.fin <- rowMeans(preds)                         # compute "ensemble" prediction based on average of bootstrapped predictions
plot(evap[,"Evap"],preds.fin,col="red",pch=16,    # plot predicted from prunes tree vs observed
     xlab="Measured Evap", ylab="RT Prediction")  # ta-da!  beats linear regression!                     
r2=round(cor(evap[,"Evap"],preds.fin)^2,2)        # add squared correlation (R^2)
text(10,45,paste("R^2 =",r2))
abline(0,1)






