library(readr)
data <- read.table("marketing_campaign.csv",header = TRUE,sep='\t')

data <- na.omit(data)

data <- as.data.frame(data) # Convert to a data frame if it's not
data <- sapply(data, as.numeric) # Convert all columns to numeric
data <- scale(data)

# Hierarchical clustering using single linkage
single_linkage_clusters <- hclust(dist(data), method = "single")

# Hierarchical clustering using complete linkage
hc <- hclust(dist(data), method = "complete")

# Create the dendrogram
dendrogram <- as.dendrogram(single_linkage_clusters)

# Plot the dendrogram
plot(dendrogram, main = "Dendrogram for Customer Personality Analysis")

# To cut the dendrogram into clusters and visualize them:
num_clusters <- 4  # You can adjust this based on your analysis
cluster_assignments <- cutree(hc, k = num_clusters)
table(cluster_assignments)