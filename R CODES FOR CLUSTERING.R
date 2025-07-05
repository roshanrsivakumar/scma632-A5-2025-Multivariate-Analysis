# Load required packages
library(cluster)
library(factoextra)
library(dplyr)
library(pheatmap)

# Read data
survey_data <- read.csv("Survey.csv", header = TRUE)
responses <- survey_data[, 20:46]  # Response variables (Likert scale)
scaled_data <- scale(responses)    # Standardize data

# Determine optimal clusters (Elbow method)
fviz_nbclust(scaled_data, kmeans, method = "wss") 

# K-means clustering (K=4)
set.seed(123)
km_result <- kmeans(scaled_data, centers = 4, nstart = 25)
fviz_cluster(km_result, data = scaled_data, palette = "jco")

# Hierarchical clustering
hc_result <- hclust(dist(scaled_data), method = "ward.D2")
fviz_dend(hc_result, k = 4, cex = 0.5, palette = "jco")

# Heatmap
pheatmap(t(scaled_data), cutree_cols = 4, show_colnames = FALSE)

# Add clusters to original data
survey_data$cluster <- km_result$cluster

# Cluster interpretation (mean responses per cluster)
cluster_means <- survey_data %>%
  group_by(cluster) %>%
  summarise(across(20:46, mean, na.rm = TRUE))

print(cluster_means)