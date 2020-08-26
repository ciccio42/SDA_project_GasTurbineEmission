# Chapter 10 Lab 1
#### Principal Components Analysis (PCA) #####
# In this lab, we perform PCA on the USArrests data set, which is part of
# the base R package. The rows of the data set contain the 50 states, in
# alphabetical order.
states=row.names(USArrests); states
names(USArrests)
apply(USArrests, 2, mean)
# We notice that the variables have vastly different means.
# Note that the apply() function allows us to apply a function-in this case, the mean() function-to each row or column of the data set. The second input here denotes whether we wish to compute the mean of the rows, 1, or the columns, 2. We see that there are on average three times as many rapes as murders, and more than eight times as many assaults as rapes. We can also examine the variances of the four variables using the apply() function. 
apply(USArrests, 2, var)
# Not surprisingly, the variables also have vastly different variances: the UrbanPop variable measures the percentage of the population in each state living in an urban area, which is not a comparable number to the number of rapes in each state per 100,000 individuals. If we failed to scale the variables before performing PCA, then most of the principal components that we observed would be driven by the Assault variable, since it has by far the largest mean and variance. Thus, it is important to standardize the variables to have mean zero and standard deviation one before performing PCA.

# perform principal components analysis using the prcomp()
pr.out=prcomp(USArrests, scale=TRUE)
# By default, the prcomp() function centers the variables to have mean zero. By using the option scale=TRUE, we scale the variables to have standard deviation one.
names(pr.out)
pr.out$center
pr.out$scale
# The center and scale components correspond to the means and standard deviations of the variables that were used for scaling prior to implementing PCA.
pr.out$rotation
# The rotation matrix provides the principal component loadings; each column of pr.out$rotation contains the corresponding principal component loading vector. This function names it the rotation matrix, because when we matrix-multiply the X matrix by pr.out$rotation, it gives us the coordinates of the data in the rotated coordinate system.
# These coordinates are the principal component scores.
# We see that there are four distinct principal components. This is to be expected because there are in general min(n ??? 1, p) informative principal components in a data set with n observations and p variables.
dim(pr.out$x)
# Matrix x has as its columns the principal component score vectors. That is, the kth column is the kth principal component score vector.

# We can plot the first two principal components as follows
biplot(pr.out, scale=0)
# The scale=0 argument to biplot() ensures that the arrows are scaled to represent the loadings; other values for scale give slightly different biplots with different interpretations.
# Notice that this figure is a mirror image of Figure 10.1. Recall that
# the principal components are only unique up to a sign change, so we can
# reproduce Figure 10.1 by making a few small changes:
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
# The prcomp() function also outputs the standard deviation of each principal component:
pr.out$sdev
pr.var=pr.out$sdev^2; pr.var # variance
# To compute the proportion of variance explained (PVE) by each principal component, we divide the variance explained by each principal component by the total variance explained by all four principal components:
pve=pr.var/sum(pr.var); sum(pve)
# We see that the first principal component explains 62.0% of the variance in the data, the next principal component explains 24.7% of the variance, and so forth.

# We can plot the PVE explained by each component, as well as the cumulative PVE, as follows:
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
# Note that the function cumsum() computes the cumulative sum of the elements of a numeric vector. For instance:
a=c(1,2,8,-3)
cumsum(a)


# PCA on the NCI60 Data #####
# Unsupervised techniques are often used in the analysis of genomic data. In particular, PCA and hierarchical clustering are popular tools.
# We illustrate these techniques on the NCI60 cancer cell line microarray data, which consists of 6,830 gene expression measurements on 64 cancer cell lines.
# The NCI60 data
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)
# Each cell line is labeled with a cancer type.
# We want to see the extent to which these cancer types agree with the results of PCA.
# We first perform PCA on the data after scaling the variables (genes) to have standard deviation one, although one could reasonably argue that it is better not to scale the genes.
pr.out=prcomp(nci.data, scale=TRUE)
# We now plot the first few principal component score vectors, in order to visualize the data. The observations (cell lines) corresponding to a given cancer type will be plotted in the same color, so that we can see to what extent the observations within a cancer type are similar to each other. We first create a simple function that assigns a distinct color to each element of a numeric vector. The function will be used to assign a color to each of the 64 cell lines, based on the cancer type to which it corresponds.
Cols=function(vec){
  cols=rainbow(length(unique(vec))) # Note that the rainbow() function takes as its argument a positive integer, and returns a vector containing that number of distinct colors
  return(cols[as.numeric(as.factor(vec))])
} 
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z3")
# Cell lines corresponding to a single cancer type do tend to have similar values on the first few principal component score vectors. This indicates that cell lines from the same cancer type tend to have pretty similar gene expression levels.
# It would not have been possible to visualize the data without using a dimension reduction method such as PCA, since based on the full data set there are 'choose(6830,2)' possible scatterplots, none of which would have been particularly informative.

# We can obtain a summary of PVE of the first few principal components using the summary() method for a prcomp object
summary(pr.out)
# Using the plot() function, we can also plot the variance explained by the first few principal components.
plot(pr.out)
# the height of each bar in the bar plot is given by squaring the corresponding element of pr.out$sdev

# Plot the PVE of each principal component (i.e. a scree plot) and the cumulative PVE of each principal component
pve= pr.out$sdev^2/sum(pr.out$sdev^2) #100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")
# equivalently:
plot(summary(pr.out)$importance[2,],  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(summary(pr.out)$importance[3,], type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")
# We see that together, the first seven principal components explain around 40% of the variance in the data. This is not a huge amount of the variance. However, looking at the scree plot, we see that while each of the first seven principal components explain a substantial amount of variance, there is a marked decrease in the variance explained by further principal components. That is, there is an elbow in the plot after approximately the seventh principal component. This suggests that there may be little benefit to examining more than seven or so principal components (though even examining seven principal components may be difficult).