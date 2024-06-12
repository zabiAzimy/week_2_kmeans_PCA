### Performing K-Medoid clustering using PAM algorithm

# loading the required libraries
library("cluster")
library("ISLR")

# install.packages("ISLR") # uncomment this if not installed

# We are going to use USArrest dataset here
data("USArrests")
head(USArrests)
dim(USArrests)

# replacing the state names with abbreviations
row.names(USArrests) <- state.abb

# scatter plot matrix
pairs(USArrests)

# Let's try PAM clustering with 4 clusters
# but before that we should scale the data
clustmat <- scale(USArrests)
clustmat

# simply using pam function that belongs to cluster package
pam_out <- pam(clustmat, k = 4)

# investigate about the output attributes
names(pam_out)

# Creating the plot for the clustering
clusplot(pam_out, labels = 3, col.p = pam_out$clustering)

# The above cluster plot automatically plots the first two PCs
# For each cluster there is an eclipse and all the points belonging to 
# that cluster are inside that eclipse

# We are now using a different distance metric
# By default the PAM algorithm is using Euclidean distance
# We are using Manhattan distance now

pam_out_man <- pam(clustmat, k = 4, metric = "manhattan")
clusplot(pam_out_man, labels = 3, col.p = pam_out_man$clustering)


# making a silhouette plot - used to assess the appropriateness
sp <- silhouette(pam_out_man)
plot(sp, col = 1:4)
mean(sp[, "sil_width"])
abline(v = mean(sp[, "sil_width"]))

# let's asses the best number of clusters - Using mean silheutte width
avesw_vec <- rep(NA, 7)
avesw_vec

for(k in 2:7){
  avesw_vec[k] <- mean(silhouette(pam(clustmat, k = k, metric="manhattan"))[, "sil_width"])
}
avesw_vec

# Talk around the plot - How to assess it and decide about the best value of k?
plot(1:7, avesw_vec, type = "b",
     ylab = "Average sillheutte width",
     xlab = "Number of cluster",
     ylim = c(0, 0.6), col = "blue")

# looking at the above plot, we should be able to decide a good value for k

# Re-running the PAM algorithm with optimal number of clusters - suppose that is 3
pam_out_op <- pam(clustmat, k = 3)
clusplot(pam_out_op, labels = 3, col.p = pam_out_op$clustering)

# creating the sillheutte plot
sp <- silhouette(pam_out_op)
plot(sp, col = 1:3)

# Drawing a vertical line 
abline(v = mean(sp[, "sil_width"]))


# a comparison of K-Means and PAM clustering
km_out <- kmeans(USArrests, centers = 2, nstart = 20)
# look at the ouput of the following contingency table and talk around it
table(km_out$cluster, pam_out$clustering)

km_out <- kmeans(clustmat, centers = 2, nstart=20)
table(km_out$cluster, pam_out$clustering)

# silheutte plot for k-means
# as the silhoeutte function is part of cluster package and kmeans is part of base R installation
# we need to modify a bit 
sp <- silhouette(km_out$cluster, dist(USArrests))
plot(sp, col = 1:2)




