### Required packages for the exercise-2

## load the packages
library("rgl") # this package is for 3D visualization
library("cluster") # for additional cluster analysis techniques
library("ISLR2")

# install.packages("ISLR2")
# install.packages("rgl")

# Performing K-Means in higher dimensional datasets
set.seed(135792468)
x <- matrix(rnorm(75*3), ncol = 3)
x[1:25, 1] <- x[1:25, 1] + 5
x[51:75, 2] <- x[51:75, 2] - 6

# a little bit of exploration of our generated 3D dataset
dim(x)
head(x)

# Each number 1:3 are replicated 25 times and assigned to truth
truth <- rep(1:3, each = 25)

# pairs of scatter plots for all the features
pairs(x, col = truth)

# plotting the 3D representation of our dataset
plot3d(x, col = truth, size=1, type="s")


# We are now running the kmeans function on x with 3 clusters
km_out <- kmeans(x, centers = 3, nstart = 20)

# The arg nstart sets the number of iterations for which the kmeans algorithm
# runs - so in this case a total of 20 times the algorithm runs

# let's plot the clusters 
plot3d(x, col = km_out$cluster, size = 1, type = "s")

# Adding the cluster centers
plot3d(km_out$centers, add = TRUE, col = 1:3, type = "s")

# repeating the above with 2 clusters - 
# How to describe two clusters in one sentence? -- Find out the answer


# We copy the above code and adapt it to the following criteria
# Generating a new matrix - this time with 10 columns instead of 3
set.seed(135792468)
y <- matrix(rnorm(75 * 10), ncol = 10)
y[1:25, 1] <- y[1:25, 1] + 5
y[51:75, 2] <- y[51:75, 2] - 6

head(y)
dim(y)
hist(y)
# Running kmeans clustering on x with 3 clusters
km_out_x <- kmeans(x, centers = 3, nstart = 30)
km_out_x$centers

# let's again plot for the x matrix
plot3d(x, col = km_out_x$cluster, size = 1, type = "s")
plot3d(km_out_x$centers, col = 1:3, add = TRUE, type = "s")


# running kmeans with 3 clusters on y
km_out_y <- kmeans(y, centers = 3, nstart = 30)

# As our dataset now contains 10 dimensions, it is not sensible to use plot3d
# plot3d(y, col = km_out_y$cluster, size = 1, type = "s")

# using table function to find out number of correctly and incorrectly assigned clusters
# first we have to construct a true cluster values
true_clusters <- rep(1, 75)
true_clusters[26:50] <- 2
true_clusters[51:75] <- 3
# we now have the true cluster values
true_clusters

# just using table to compare the true clusters and the ones from ourput of kmeans
table(true_clusters, km_out_y$cluster)

# dicussion on the contingency table from the above code
# The diagonal elements represent correctly assigned clusters
# the off-diagonal elements represent the incorrectly assigned clusters
# Following the discussion, please check the output and add comments











