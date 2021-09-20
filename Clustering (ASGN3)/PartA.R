library(dplyr)
# Read the Data
fram <- read.csv("/media/yomna/New\ Volume/DEBI/uOttawa/DS/Asgn3/framingham.csv",header=TRUE);
str(fram)
glimpse(fram)

fram <- data.frame(fram)
fram$age<- scale(fram$age)

fram = fram[,(names(fram) %in% c("age","male", "TenYearCHD"))]

# ---------------------------------------------

#K-Means Clustering Algorithm
library(ggplot2)
library (cluster)
library (vegan)
data(varespec)
dis = vegdist(varespec)
set.seed(917)

#Run k-means cluster of the dataset
Cluster_kmean_4 <- kmeans(fram, 4, nstart = 20)

#Tabulate the cross distribution
table(Cluster_kmean_4$cluster,fram$TenYearCHD)

Cluster_kmean_4$cluster <- factor(Cluster_kmean_4$cluster)

ggplot(fram, aes(age, male, color = as.factor(TenYearCHD))) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = Cluster_kmean_4$cluster)+ 
  scale_color_manual(values = c('black', 'red'))


# ----------------------------------------------------------
fram[,1:2]
# Elbow Curve

wss <- (nrow(fram)-1)*sum(apply(fram[,1:2],2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(fram[,1:2],centers=i)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# k means
set.seed(917)

#Run k-means cluster of the dataset
Cluster_kmean_6 <- kmeans(fram[,1:2], 6, nstart = 20)

#Tabulate the cross distribution
table(Cluster_kmean_6$cluster,fram$TenYearCHD)

Cluster_kmean_6$cluster <- factor(Cluster_kmean_6$cluster)

ggplot(fram, aes(age, male, color = as.factor(TenYearCHD))) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = Cluster_kmean_6$cluster) #+ 
  scale_color_manual(values = c('black', 'red'))
  
# ---------------------------------------------------------------


silhouette_score <- function(k){
  km <- kmeans(fram[,1:2], centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(fram[,1:2]))
  mean(ss[, 3])}
k <- c(4,5,6,7,8,9)
k
avg_sil <- sapply(k, silhouette_score)
avg_sil
#The best cluster is 2 (it has the highest silhouette score)   

plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
