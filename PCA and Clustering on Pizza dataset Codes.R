
#PCA and Clustering on Pizza dataset

#PCA

#Install libraries
install.packages("NbClust")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages("factoextra")
install.packages("gridExtra")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("factoextra")
install.packages("magrittr")
install.packages("dbscan")
library(dbscan)
library(magrittr)
library(factoextra)
library(NbClust)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(grid)
library(factoextra)
library(corrplot)
library(ggcorrplot)

setwd("C:/Users/rahil/Desktop/UL papers/PCA")
#Load data
pizza_data <- read.csv("C:/Users/rahil/Desktop/UL papers/PCA/pizza.csv")
head(pizza_data)
str(pizza_data)
pizza_data <- pizza_data[,3:9]
head(pizza_data)

dim(pizza_data)

pizza_data %>% 
  gather(key = value_groups, value = value) %>% 
  ggplot(aes(x=value)) +
  geom_histogram(color="pink") +
  facet_wrap(.~value_groups, scales = "free")

#Data Preparation
norm_data <- scale(pizza_data)
head(norm_data)
#correlation
corr <- cor(norm_data, method = "pearson")
ggcorrplot(corr)



#Clustering for pizzadata
#K-means

pizza_dist <- dist(norm_data, method = "euclidean")
fviz_dist(pizza_dist, show_labels = F)
#elbow method
fviz_nbclust(norm_data, FUNcluster = kmeans, nstart=35, "wss") +
  geom_vline(xintercept = 3, linetype=2)



nb <- NbClust(norm_data, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")


km_data <- kmeans(norm_data, centers = 3, nstart = 35)

fviz_cluster(list(data=norm_data, clusters=km_data$cluster))

#MDS

mds_pizza <- cmdscale(dist(pizza_data), k = 2)
summary(mds_pizza)

mds_plot <- data.frame(mds_pizza)

ggplot(data = mds_plot, aes(x = X1, y = X2) ) +
  geom_point(color = "blue") +
  labs(x = "Component 1", y = "Component 2", title = "MDS Plot of Pizza Ingredients") +
  theme_bw()

#Computation of PCA 

#Perform PCA
pca_data <- prcomp(norm_data)
summary(pca_data)

pca_data$rotation[,1:2] #relationship between PC1 and PC2 with each column
#Eigenvalues computation
eigenvalues <- summary(pca_data)$importance[2,]
eigenvalues
fviz_eig(pca_data, choice = "variance", addlabels = TRUE, barfill = "steelblue", linecolor = "red")
#Visualisations
fviz_cos2(pca_data, choice = "var", axes = 1:2)
pca1<- fviz_contrib(pca_data, choice = "var", axes = 1, top = 6, title = "PCA1")
pca2<- fviz_contrib(pca_data, choice = "var", axes = 2, top = 6, title = "PCA2")
grid.arrange(pca1, pca2,  ncol=2)


#DBSCAN
dbscan_pizza <- mds_plot
model <- dbscan(dbscan_pizza, eps = 2.8, MinPts = 5)


fviz_cluster(model, dbscan_pizza, geom = "point", 
             xlab = "First Dimension", ylab = "Second Dimension", 
             ggtheme = theme_classic()) + 
  ggtitle("DBSCAN Clustering of Pizza Ingredients")



