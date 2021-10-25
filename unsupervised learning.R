library(tidyverse)
library(corrplot)
library(gridExtra)
library(GGally)
library(knitr)
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(factoextra)
library(cluster)
library(devtools)


### please put the csv file on your working directory

seed <-read.csv("seed.csv", header = TRUE)

seed %>%
  gather(Attributes, value, 1:7) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE, bins=20) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title="Seed Attributes - Histograms") +
  theme_bw()


seed %>%
  gather(Attributes, value, 1:7) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_density(colour="black", alpha=0.5, show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Seed Attributes - Density plots") +
  theme_bw()





scaled_seed <-scale(seed)

scaled_seed

summary(seed)

summary(scaled_seed)

head(seed)



# Looking unspervised learning from the observations viewpoints:



##Hierarchical Clustering
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

ac <- function(x) {
  agnes(seed, method = x)$ac
}
sapply(m, ac)



ad <- function(x) {
  agnes(scaled_seed, method = x)$ac
}
sapply(m, ad)

## Dendrogram with ward scaled data:

clust <- agnes(scaled_seed, method = "ward")
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram")

## Dendrogram with ward original data:

clust <- agnes(seed, method = "ward")
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 

### From now on we work on scaled data: 


## The chosen linkage method can influence a lot the consequences of our clustering. 


# Dendrogram with complete linkage:
clust <- agnes(scaled_seed, method = "complete")
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 

# Dendrogram with average linkage:
clust <- agnes(scaled_seed, method = "average")
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram")

# Dendrogram with single linkage:
clust <- agnes(scaled_seed, method = "single")
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram")



#Choosing the optimal number of clusters:

#The Gap Statistic Method

gap_stat <- clusGap(scaled_seed, FUN = hcut, nstart = 25, K.max = 10, B = 50)



fviz_gap_stat(gap_stat) # according to this criterion, the optimum number of clusters is 3.

### gap statistics for k-means 

gap_statkmeans <- clusGap(scaled_seed, FUN = kmeans, nstart = 25, K.max = 10, B = 50)



fviz_gap_stat(gap_statkmeans) # according to this criterion, the optimum number of clusters is 3.


distant_matrix_seed <- dist(seed, method = "euclidean")
distant_matrix_scaled_seed <- dist(scaled_seed, method = "euclidean")

## The following two groups shows how scaling can result in the number of each cluster. 
### Firstly scaled data:
final_clust <- hclust(distant_matrix_scaled_seed, method = "ward.D2" )
groups <- cutree(final_clust, k=3)
table(groups)
## original data:
final_clust <- hclust(distant_matrix_seed, method = "ward.D2" )
groups <- cutree(final_clust, k=3)
table(groups)

#### Elbow Method
fviz_nbclust(scaled_seed, hcut ,method = "wss", k.max = 24) + theme_minimal() + ggtitle("the Elbow Method")# it is 3 here

fviz_nbclust(scaled_seed, kmeans ,method = "wss", k.max = 24) + theme_minimal() + ggtitle("the Elbow Method") ###3 


#K-Means
km=kmeans(seed,3)
autoplot(km, seed, frame=TRUE)


km=kmeans(scaled_seed,3)
autoplot(km, scaled_seed, frame=TRUE)


#### The Silhouette Method for hcut
fviz_nbclust(scaled_seed,hcut , method = "silhouette", k.max = 24) + theme_minimal() + ggtitle("The Silhouette Plot") # here it says 2 clusters 


#### The Silhouette Method for  kmeans
fviz_nbclust(scaled_seed,kmeans , method = "silhouette", k.max = 24) + theme_minimal() + ggtitle("The Silhouette Plot") # here it says 2 clusters 
