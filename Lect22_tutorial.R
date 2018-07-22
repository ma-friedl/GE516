# Lecture 22 tutorial
setwd("~/Dropbox/Main/Courses/GE516/Tutorials")

# install and load a couple of libraries
install.packages("akima")
install.packages("maps")
library(akima)                                          # load libraries
library(maps)

# loar data, compute R and S
dat <- read.table("evap.txt",header=T)
x.orig <- dat[,2:11]                                         # just look at predictors of evap
x.orig <- sweep(data.matrix(x.orig),2,colMeans(x.orig))      # let's center the data (i.e. translation)
round(colMeans(x.orig),2)
R <- cor(x.orig)                                             # compute correlation and covariance matrices            
S <- var(x.orig)

# Compare covariance and correlation matrices
round(R,2)
round(S,2)
x <- scale(x)                                           # scale centers and normalizes variances of columns
round(var(x),2)

# Ok, let's compute eigenvectors and eigenvalues and 1st PC (for e.g.) - note that data are not centered!
eig <- eigen(S)                                         # Compute eigenvals/vecs
barplot(eig$values)                                     # barplot of eigen values
barplot(eig$vectors[,1])                                # barplot of first eigenvector
PC.S <- as.matrix(x.orig)%*%eig$vectors                 # PCs from S
eig <- eigen(R)                                         # Compute eigenvals/vecs
barplot(eig$values)                                     # barplot of eigenvlaues
barplot(eig$vectors[,1])                                # Barplot of first eigen vector
PC.R <- as.matrix(x)%*%eig$vectors                      # PCs from R  
plot(PC.S[,1],PC.R[,1],pch=16,col="red",xlab="PC1.S",ylab="PC1.R")  # Plot first PC from R vs first PC from S

# Spatial vs temporal PCA using plains precip data
precip <- read.table("allpcp.txt")				      # superset of previous data
head(precip)                                    # take a quick look
dim(precip)									                    # 304 stations, 172 months
precip <- precip[,18:161]						            # Jan 82-Dec93 - 12 year period
dim(precip)                                     # 304 stations, 144 months
pos <- read.table("stns.long.lat.304")[,1:2]    # longitude and latitude of each station

# now extract locations and set up for map.
?map										                    # check out map library
map('state', fill = TRUE, col = palette())  # for fun !?

map("usa",xlim=c(-130,-65),fill=T,col="lightblue")			# make an empty map of USA
points(pos[,1],pos[,2],col="red",cex=0.5,pch=16)				# put station locations on map

dat <- cbind(pos,precip)						        # set up data frame with position + precip
clean <- na.omit(dat)							          # remove stations with missing data in some months
dim(clean)										              # see what's left
pos <- clean[,1:2]								          # extract just lat and long
precip <- (clean[,3:146])						        # precip data - rows = stations; cols = time

# before doing PCA, let's do some more basic stuff
mns <- rowMeans(precip)						         # mean monthly precip at each stn

# plot it as a surface
par(pty = "s")
pcp.srf <- interp(pos[,1],pos[,2],mns)		  # interpolate to a regular grid
image(pcp.srf)								              # plot it
map("state",xlim=c(-130,-65),fill=F,add=T,col="blue")
contour(pcp.srf,nlevels=6,add=T,interior=T)	# add contours
title("Mean Monthly Precip in Great Plains")						

# now do same for variance
vars <- apply(precip,1,var)   					    # compute mean variance
pcp.srf <- interp(pos[,1],pos[,2],vars)		  # estimate surface/image
image(pcp.srf)								              # plot it
map("state",xlim=c(-130,-65),fill=F,add=T,col="blue")
contour(pcp.srf,add=T,nlevels=4)	          # add contours
title("Variance in Monthly Precip")			    # add a title

# coefficient of variation in precip.
pcp.srf <- interp(pos[,1],pos[,2],vars^0.5/mns)	# standard deviation, normalized by mean
image(pcp.srf)
map("state",xlimc(-130,-65),fill=F,add=T,col="blue")
contour(pcp.srf,add=T,nlevels=6,cex=0.9)
title("CV in Precip")

# Now do PCA
# First look at spatial variance: n = # stations, p = # of months
pcp.pca <- princomp(precip)					                  # estimate PCA - default uses covariance matrix
names(pcp.pca)                                        # see what's in result of princomp
pcp.pca$sdev[1:10]						                        # Square root: 1st 10 eigen values
round(pcp.pca$loadings[,1:10],2)	                    # 1st 10 eigen vectors
plot(pcp.pca,main="Eigenvalues: Spatial PC")          # scree plot
pcp.pca$sdev[1:10]^2/sum(pcp.pca$sdev^2)              # proportion of variance explained by PCA

par(mfrow=c(1,2))							                        # now lets map the PCA results
pca.out <- pcp.pca$scores  					                  # put PC's in pca.out
pcp.srf <- interp(pos[,1],pos[,2],pca.out[,1])        # first PC
image(pcp.srf)
contour(pcp.srf,add=T)
title("1st PC - Spatial")

pcp.srf <- interp(pos[,1],pos[,2],pca.out[,2]) # second PC!
image(pcp.srf)
contour(pcp.srf,add=T)
title("2nd PC - Spatial")

# do the same, but using correlation matrix
pcp.pca <- princomp(precip,cor=T)                  # cor=T tells princomp to use correlation matrix!
pca.out <- pcp.pca$scores                          # get actual components
pcp.srf <- interp(pos[,1],pos[,2],pca.out[,1])     # first PC
image(pcp.srf)
contour(pcp.srf,add=T)
title("1st PC - Spatial")

pcp.srf <- interp(pos[,1],pos[,2],pca.out[,2])
image(pcp.srf)
contour(pcp.srf,add=T)
title("2nd PC - Spatial")                       # Compare results - different, but not entirely!

# Now do temporal PCA
precip <- t(precip)                                # take transpose: rows are months,cols are stns
pcp.pca <- prcomp(precip,center=T,scale=T)         # NOTE: prcomp (princomp chokes with p>n; center=T,scale=T equiv to using R instead of S)
names(pcp.pca)                                     # names in pca object slightly different from output from princomp
barplot(pcp.pca$sdev[1:10]^2,main="Eigenvalues")                         # look at eigen values
barplot(pcp.pca$sdev[1:10]^2/sum(pcp.pca$sdev^2),main="% Var Explained") # variance explained

par(mfrow=c(1,1),pty="m")
pcp.pcs <- pcp.pca$x                               # these are the actual PCs
plot(pcp.pcs[,1],type="l",ylab="1st PC",        # plot first PC as time series
     col="red",lwd=2)
lines(pcp.pcs[,2],type="l",ylab="1st PC",        # and so on...
     col="blue",lwd=2)

# Now map these results
par(pty="s")                                         # square plot for map
pca.loadings <- pcp.pca$rotation                        # get the eigenvectors
pcp.srf <- interp(pos[,1],pos[,2],pca.loadings[,1])     # plot loading at each station
image(pcp.srf)
contour(pcp.srf,add=T)
title("1st PC - Temporal")

pcp.srf <- interp(pos[,1],pos[,2],pca.loadings[,2])
image(pcp.srf)
contour(pcp.srf,add=T)
title("2nd PC - Temporal")           








