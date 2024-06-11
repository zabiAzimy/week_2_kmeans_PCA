## PCA and clustering on Hamburg Decathalon data

# a: Load the data
decathlon <- read.csv("data/DecathlonHamburg2017.csv")

head(decathlon)
dim(decathlon)

# b: the matrix scatter plot
pairs(decathlon)

# c: Use K-Means directly on the unscaled data
km_out <- kmeans(decathlon, centers = 5, nstart = 20)
pairs(decathlon, col = km_out$cluster)

# d: Using prcomp to get the PCs for this dataset
pr.out <- prcomp(decathlon, scale = TRUE)

# Obtaining biplot for the first two PCs
biplot(pr.out, scale = TRUE)

# e: Making a 3D plot
plot3d(pr.out$x[, 1:3], size = 1, type = "s")

# f: Cumulative variance plot for the PCs and decide about the number of PCs
# Sdev explained
pr.out$sdev

# Variance explained
pr.var <- pr.out$sdev ^ 2

# proportion of variance explained
pve <- pr.var / sum(pr.var)
pve

plot(cumsum(pve),
      xlab = "Principal components",
      ylab = "Proportion of variance Explained",
      xlim = c(1, 10),
      type = "b",
      col = "Blue") # looking at the plot, we can say that there is an elbow 
# at 2 PCs. So it can be a good choice to take 2 PCs only

# g: Use K-Means clustering on the PCs and choose a good value for k
km_out <- kmeans(pr.out$x, centers = 4, nstart = 30)

biplot(pr.out$x, scale = 0) # I am not getting this one

# scatter plot matrix
pairs(pr.out$x, col = "blue")
