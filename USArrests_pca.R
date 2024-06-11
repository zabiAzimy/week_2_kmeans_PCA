# Principal component analysis on USArrests dataset
data("USArrests")

# let's a bit explore the dataset
head(USArrests)
dim(USArrests)

# the dataset containin four columns
names(USArrests)

# The rows of dataset contains the 50 states
states <- row.names(USArrests)
states

# briefly examining the data
# The arg 2 specifies the column means 
apply(USArrests, 2, mean)

# WE can also look at the variance of each column
apply(USArrests, 2, var) # looking at the output of this code, we observe 
# a very big difference in variance of each column
# As we are going to perform PCA - Then we should first scale the data

# Using the function prcomp() that also does the scaling for us
pr.out <- prcomp(USArrests, scale = TRUE)

# checking the ouput of prcomp 
names(pr.out)

# the center correspond to the mean of variables used for scaling 
pr.out$center

# the scale correspond to the standard deviation used for scaling
pr.out$scale

# the rotation provides the principal component loadings - There are 4 values 
# in the rotation vector that corresponds to each column in the dataset
pr.out$rotation

# after matrix multiplication of X matrix by pr.out$rotation we get the co-ordinates 
# of data in the rotated coordinate system. These coordinates are the principal 
# component scores.

dim(pr.out$x)

# plotting the first two principal components
biplot(pr.out, scale = 0)

# prcomp() function also outputs the standard deviation of each principal component
pr.out$sdev

# of course the variance explaned by each principal component is the squared of sdev
pr.var <- pr.out$sdev^2

# finding the total variance explained by each principal component
# proportion of variance explained
pve <- pr.var / sum(pr.var)
pve

# The plot of pve explained by each component
par(mfrow = c(1, 2))
plot(pve, xlab = "Principal component",
          ylab = "Proportion of variance explained",
          ylim = c(0,1),
          type = "b"
          )

# Cumulative PVE
plot(cumsum(pve), xlab = "Principal component",
                  ylab = "Cumulative proportion of variance explained",
                  ylim = c(0, 1), type = "b", col = "Green")



























