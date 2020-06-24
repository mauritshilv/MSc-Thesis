# The purpose of this R script is to use a clustering method to create the new 'Premiumization Level' variable
# Author: Maurits Hilverda
# Date: June 23, 2020

# Set the desired working directory
setwd("/Users/mauritshilverda/Documents/Studie/MSc/THESIS/python/premium_K_means_R")

# Install and activate package required to read excel file
library("readxl")
data <- read_excel("/Users/mauritshilverda/Documents/Studie/MSc/THESIS/DATA/CombinedData_VersionJune21.xlsx")
# Check data
summary(data)

# As the initial centroids are defined randomly,we define a seed for purposes of reprodutability
set.seed(123)

# Only select desired columns
data2 <- data[, c(1,3,4,5)]
# Select Property IDs as rownames
library("tidyverse")
data3 <- data2 %>% remove_rownames %>% column_to_rownames(var="Property_ID")
# Check class of variables
class(data3$Property_Type)
class(data3$Listing_Type)
class(data3$Average_Daily_Rate)
# Convert 'character' variables to factors
data3$Property_Type <- as.factor(data3$Property_Type)
data3$Listing_Type <- as.factor(data3$Listing_Type)
# Remove unneeded datasets to free up space
remove(data)
remove(data2)

# Activate clustering algorithms and visualization packages
library(cluster)  
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)

# Compute Gower distance (features are automatically standardized)
gower_dist <- daisy(data3, metric = "gower")
gower_mat <- as.matrix(gower_dist)
# Print most similar properties
data3[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
# Print most dissimilar clients
data3[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

# Print the silhouette figure to identify the best choice of number of clusters to use
# Note: we do not need to evaluate the performance of >8 clusters since this is unfeasible for interpretation anyway
sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)

# The silhouette figure learns us that the clustering performance gradually improves when having more than three clusters
# Since having more than 6 clusters improves not that much, and interpretation becomes much more difficult we choose to use 6 clusters

# Execute the clustering with 6 predefined clusters
k <- 6
pam_fit <- pam(gower_dist, diss = TRUE, k)
clustering_results <- pam_fit$clustering
# Check the structure of the results
class(clustering_results)
# Force results into a dataframe
Clustering_Results_Dataframe <- as.data.frame(clustering_results)
Clustering_Results_Dataframe$Property_ID <-rownames(Clustering_Results_Dataframe)
# Export the results to an excel file
library("writexl")
write_xlsx(Clustering_Results_Dataframe,"/Users/mauritshilverda/Documents/Studie/MSc/THESIS/python/premium_K_means_R\\Clustering_Results.xlsx")

# Group the observations by the cluster they belong to and give a summary for each cluster
pam_results <- data3 %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
Cluster_Summary <- pam_results$the_summary

# Print a visualization in a lower dimensional space
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))
