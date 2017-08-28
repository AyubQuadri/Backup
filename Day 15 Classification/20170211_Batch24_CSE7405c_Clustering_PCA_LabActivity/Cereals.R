###Author: Ayub Quadri ###
###Problem: Clustering Activity

rm(list = ls())

#set working directory
  setwd("C:/Users/quadris/Desktop/My Learning/INSOFE CPEE/Day 15 Classification/20170211_Batch24_CSE7405c_Clustering_PCA_LabActivity")
  
#Read the data
  cereals <- read.csv("Cereals.csv",header = T,sep = ",")
  str(cereals)
  
#Pre-Processing
  #remove name column
  cereals <- cereals[-1]
  #check for null values
  sapply(cereals,function(x)sum(is.na(x)))
  
  #replace NA with 0
  cereals[is.na(cereals)] <- 0
  
  ##scaling
  cereals <-scale(cereals)
  str(cereals)

###-------------------------    Hierarchical Clustering     ------------------------###
  # Ward's method 
  # distance matrix euclidean
  d <- dist(cereals,method = "euclidean") 
  d
  fit <- hclust(d, method="ward.D2")
  plot(fit) # display dendogram
  
  groups <- cutree(fit, k=6) # cut tree into 6 clusters
  groups
  
  # draw dendogram with red borders around the 6 clusters
  rect.hclust(fit, k=6, border="red") 
  mydata_clusters=data.frame(cereals,groups)

  ###-------------------------    K- means Clustering     ------------------------###
  
  # K-Means Cluster Analysis with k = 5
  fit <- kmeans(cereals, 5) # 5 cluster solution
  fit$withinss
  sum(fit$withinss)
  fit$betweenss
  #study the mdoel
  fit$cluster
  fit$tot.withinss
  
  # get cluster means
  aggregate(cereals,by=list(fit$cluster),
            FUN=mean)
  
  # append cluster label to the actual data frame
  cereals <- data.frame(cereals, 
                       fit$cluster)
  write.csv(cereals,"kmeans_2.csv")
  head(cereals)
  
  # K-means:  Determine number of clusters by considering the withinness measure
  wss <- 0
  for (i in 1:15) {
    wss[i] <- sum(kmeans(cereals,centers=i)$withinss)
  }
  
  # Ploting the within sum of square error for different clusters
  plot(1:15, wss, 
       type="b", 
       xlab="Number of Clusters",
       ylab="Within groups sum of squares") 
  
  
  
  # For unseen data, we compute its distance from all the cluster centroids
  # and assigns it to that cluster that is nearest to it
  
  test_datapoint <- cereals[sample(1:nrow(cereals),1),]
  closest.cluster <- function(x) {
    cluster.dist <- apply(fit$centers, 1, function(y) sqrt(sum((x-y)^2)))
    print(cluster.dist)
    return(which.min(cluster.dist)[1])
  }
  
  # Predicting which cluster the new data point belongs to based on the distance.
  closest.cluster(test_datapoint)
  
  # Checking the cluster stability
  # Building the clusters on all data
  fit1 <- kmeans(cereals, 10)
  # Getting the cluster numbers
  x <- fit1$cluster
  # Building the clusters on 90 % of data
  fit2 <- kmeans(cereals[1:74,], 10)
  # Getting the cluster numbers
  y <- fit2$cluster
  unique(y)
  # Loading the required libraries
  install.packages('dtwclust')
  library(dtwclust)
  library(flexclust)
  # Checking whether the same data points are falling into same cluster 
  # when we cluster on all the data or 90% of data.
  randIndex(x[1:74], y)
  library(mclust)
  # Checking whether the same data points are falling into same cluster 
  # when we cluster on all the data or 90% of data.
  adjustedRandIndex(x[1:74], y)
  
