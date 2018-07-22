# Lecture #21 tutorial
setwd("~/Dropbox/Main/Courses/GE516/Tutorials")

#######################################################################################
# Ok, K-group classification via LDA - first just check out "lda" function in MASS, note syntax, defaults for priors
# Note: lda = linear discriminant analysis; qda = quadratic discriminant analysis
library(MASS)  

# will use the builtin "crabs" data set
?crabs
head(crabs)
pairs(crabs[,4:8], col=crabs[,1],pch=16)
pairs(crabs[,4:8], col=crabs[,2],pch=16)

# first do a linear discriminant analysis
crabs.lda <- lda(crabs$sex~FL+RW+CL+CW+BD,crabs)   # classify sex based on body measurements
crabs.lda  							                           # look at result
plot(crabs.lda)						                         # plot result - note, plot centers data around 0!
barplot(crabs.lda$scaling,beside=T)                # look at coefficients

# Now do lda classification
# Use "predict" to get predictions for class values & posterior probabilities, plus z values:
names(predict(crabs.lda))                         # predict is a generic function that "predicts" results from an estimated model
predict(crabs.lda)$class  			                  # extract class predictions
round(predict(crabs.lda)$posterior,3)		          # extract posterior probs
predict(crabs.lda)$x					                    # extract values of LDF (i.e., z-values)
boxplot(predict(crabs.lda)$x~predict(crabs.lda)$class,col="red",notch=T) # Look at boxplots
apply(predict(crabs.lda)$posterior,1,which.max)                          # extract max posterior in each case
as.factor(c("F","M"))[apply(predict(crabs.lda)$posterior,1,which.max)]   # do same thing but express as M or F (same as line 23)

# Now, look more carefully at results; first, set up matrix with class labels and posterior probs for comparison
lda.out <- data.frame(crabs[,2],round(predict(crabs.lda)$posterior,3))      # True class, posterior probs
colnames(lda.out) <- c("Sex","P(F)","P(M)")
lda.out[c(1:10,60:70),]

# Let's specify non-equal priors
crabs.lda <- lda(crabs$sex~FL+RW+CL+CW,crabs,prior=c(0.1,0.9))           # jack up prior probability of M
lda.out2 <- data.frame(crabs[,2],round(predict(crabs.lda)$posterior,3))  # True class, posterior probs
colnames(lda.out2) <- c("Sex","Post.P(F)","Post.P(M)")
head(data.frame(lda.out,lda.out2))

# Now, let's do classification table on results
conf.table <- table(crabs$sex,predict(crabs.lda)$class)                 # table generates cross-tabulation
conf.table                                                              # predicted (cols) vs true (rows)

# Estimate misclassification error rate
error.rate <- 1-sum(diag(conf.table))/sum(conf.table)                   # error rate is 1-sum of diagonals/sum of all cases
error.rate

# Finally, do a Jacknife cross-validation to asses quality of classification rules
preds <- factor(rep(c("F"),200))	  # set up vector for each result as factor, arbitrariliy initialialize as "F"
levels(preds) <- c("F","M")			    # assign "levels" to factor
for (i in 1:200) {                  # cylce through each row in matrix, remove case, estimate model, & predict
  outdat <- crabs[i,]							  # extract indep point
  indat <- crabs[-i,]							  # set up training data
  tmp.lda <- lda(indat$sex~FL+RW+CL+CW,indat)	       # now estimate LDF
  preds[i] <- predict(tmp.lda,newdata=outdat)$class  # predict on case that was held out (outdat)
}

# continegncy table of results
cv.table <- table(crabs$sex,preds)
cv.table
cv.error.rate <- 1-sum(diag(cv.table))/sum(cv.table)
cv.error.rate

########### PCA   ############
# Ok, let's start by looking doing a PCA on the "Evap" data set
dat <- read.table("evap.txt",header=T)
x <- dat[,2:11]                          # just look at predictors of evap
R <- cor(x)
S <- var(x)

# Compare with variance/correlation matrices on original data
round(R,2)            # Correlation matrix 
round(S,2)            # Covariance matrix 

# Ok, let's compute eigenvectors and eigenvalues and 1st PC (for e.g.)
eig <- eigen(S)                                       # Compute eigenvals/vecs
z1.S <- as.matrix(x)%*%as.matrix(eig$vectors[,1])     # First PC from S
eig <- eigen(R)                                       # Compute eigenvals/vecs
z1.R <- as.matrix(x)%*%as.matrix(eig$vectors[,1])     # First PC from R
plot(z1.R,z1.S)                                       # Compare result

# Now let's use builtin function
evap.pca <- princomp(x)                            # Estimate PCA on original data
names(evap.pca)                                    # Look at result
barplot(evap.pca$loadings[,1])                     # Loadings for 1st PC
z.scores <- evap.pca$scores                        # PC scores 
plot(z1.S,z.scores[,1])                            # What's up here?
round(var(z.scores),2)                             # Check out covariance matrix on PCs
diag(var(z.scores))                                # Look at diagonal
eigen(S)$values                                    # Compare with result from eigen applied to S
evap.pca$sdev                                      # Compare eigenvectors
diag(var(z.scores))^0.5                            # $sdev give the square roots of the eigen values = standard deviations of the PC's
barplot(eigen(S)$values/sum(eigen(S)$values))              # Plot proportion of variance explained by each component 
barplot(cumsum(eigen(S)$values)/sum(eigen(S)$values))      # cumulative proportion explained
barplot(eigen(R)$values/sum(eigen(R)$values))              # Same, but for R, Plot proportion of variance explained by each component 
barplot(cumsum(eigen(R)$values)/sum(eigen(R)$values))      # cumulative proportion explained







