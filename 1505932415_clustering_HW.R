# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))
library(cluster)
library(rattle.data)
library(NbClust)

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)
glimpse(wine)

# ---- Exercise 1: ----
# Remove the first column from the data and scale it using the scale() function
?scale
wine %>%
  select(Type) %>%
  n_distinct()

wine = wine[,2:14]
head(wine)
wine = scale(wine)
head(wine)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 
?apply
?var

    
wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
	              
               	for (i in 2:nc){
		              set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine)




# ---- Exercise 2: ----
# * How many clusters does this method suggest?
# ---3 clusters. The graph shows a steep change between clusters 2 and 4. 

# * Why does this method work? What's the intuition behind it?
# ---With only 1 or 2 clusters, the sum of means can easily be affected by outliers. 

# * Look at the code for wssplot() and figure out how it works
# sum the variance of the columns * (number of rows - 1)
# for each cluster (from 2 to 15), sum the clusters' withinss

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
?NbClust

set.seed(1234)
nc <- NbClust(wine, min.nc = 2, max.nc = 15, method = "kmeans")
str(nc)
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")



# ---- Exercise 3: ----
# How many clusters does this method suggest?
# ---three clusters

# ---- Exercise 4: ----
# Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km
?kmeans
fit.km <- kmeans(wine, centers = 3)
class(fit.km)
typeof(fit.km)
str(fit.km)
fit.km

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
# Considering that there are three types of wines in this dataset, this method works very well in selecting the 
# right amount of clusters. 

# for 3 clusters
table(fit.km$cluster)

# for 5 clusters
fit.km5 <- kmeans(wine, centers = 5)
table(fit.km5$cluster)

# for 8 clusters
fit.km8 <- kmeans(wine, centers = 8)
table(fit.km8$cluster)

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
# It appears to be good clustering although some overlap occurs between the two of them. This seems to imply that 
# some types of wine share characteristics. 

#clusplot( ... )
?clusplot
fit.km
fit.pam <- pam(wine, k = 3)
fit.pam
class(fit.pam)
typeof(fit.pam)

clusplot(fit.pam)

clusplot(fit.km)


