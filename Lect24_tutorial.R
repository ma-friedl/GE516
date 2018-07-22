# Lecture #24 tutrial: Hierarchical Clustering
setwd("~/Dropbox/Main/Courses/GE516/Tutorials")
library(maps)

# First get the data
dat <- scale(read.table("plainspcp.txt"))
pos <- read.table("stns250.txt")[,1:2]

# Take a quick look/visualize via PC's
pcp.pcs <- predict(princomp(dat))
plot(pcp.pcs[,1],pcp.pcs[,2], pch=16,
     col="red",xlab="PC1",ylab="PC2")          # not a lot of evidence of clusters!

# Now compute distances
pcp.dist <- dist(dat)  					               # compute euclidian distances ; note euclidian is default, others available
pcp.dist										                   # lower triangular matrix; n*(n-1)/2 elements

# Now estimate hierarchical clustering
?hclust                                        # builtin function for clustering
pcp.clust <- hclust(pcp.dist,method="average") # estimate dendrogram
plot(pcp.clust)							                   # plot dendrogram - YUCK
identify(pcp.clust)                            # click on a branch to visualize cluster sub-tree (hit esc to escape)
plot(pcp.clust)                                # make a clean plot
rect.hclust(pcp.clust,k=5)                     # look at "agglomeration" into 5 clusters
rect.hclust(pcp.clust,k=3,border="blue")       # same thing, but now for 3 clusters

# Let's make this more manageable - 
smalltree <- cutree(pcp.clust,k=5)  			        # allocate each stn to 1 of 5 clusters
smalltree                                         # look at cluster assignments
hist(smalltree)								                    # look at # stns/cluster
stnmeans <- rowMeans(dat)						              # compute mean monthly precip at each stn
boxplot(stnmeans~smalltree,col="blue",notch=T)	  # boxplots of stn means in each cluster
pcp.pcs <- predict(princomp(dat))                 # compute PCs for visualization

par(pty="s")
plot(pcp.pcs[,1],pcp.pcs[,2],pch=16,col=smalltree,xlab="PC1",ylab="PC2")		  # visualize in PC space

# Estimate using ward's method
pcp.clust <- hclust(pcp.dist,method="ward.D")    # estimate dendrogram
plot(pcp.clust)  						                     # plot dendrogram - YUCK

# Looks like 5 clusters
smalltree <- cutree(pcp.clust,k=5)  			                                    # allocate stns to 5 clusters
hist(smalltree)								                                                # look at # stns/cluster					          
boxplot(stnmeans~smalltree,col="blue",notch=T)	                              # boxplots of stn means in each cluster
plot(pcp.pcs[,1],pcp.pcs[,2],pch=16,col=smalltree,xlab="PC1",ylab="PC2")		  # visualize in PC space

# Now lets do k-means
pcp.kmeans <- kmeans(dat,centers=5)                                               # prescribe 5 clusters in advance!
boxplot(stnmeans~pcp.kmeans$cluster,col="blue",notch=T)                           # boxplots of stn means in each cluster
plot(pcp.pcs[,1],pcp.pcs[,2],pch=16,col=pcp.kmeans$cluster,xlab="PC1",ylab="PC2") # visualize in PC space

# Finally look at how clusters map in space and compare results from hclust vs k-means for 5 clusters
par(mfrow=c(1,2))
library(maps)
map("usa",xlim=c(-110,-95))                                     
text(pos[,1],pos[,2],smalltree,col=smalltree)    
map("usa",xlim=c(-110,-95))                                     
text(pos[,1],pos[,2],pcp.kmeans$cluster,col=pcp.kmeans$cluster)     


