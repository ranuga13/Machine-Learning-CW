library(readxl)
library(factoextra)
library(NbClust)
library(cluster)
library(UniversalCVI)

# 1st SUBTASK


Whitewine <- read_excel("Whitewine_v6.xlsx")
View(Whitewine)
head(Whitewine)

# Check for missing data
sum(is.na(Whitewine))

# Function to create boxplots for all features
create_boxplots <- function(dataframe) {
  for (column in names(dataframe)) {
    boxplot(dataframe[[column]], main = column, ylab = "Value")
  }
}

#Function to detect and remove outliers
remove_outliers <- function(dataframe) {
  # Initialize an empty vector to store outlier indices
  outlier_indices <- c()
  
  
  for (col in names(dataframe)) {
    
    Q1 <- quantile(dataframe[[col]], 0.25)
    Q3 <- quantile(dataframe[[col]], 0.75)
    
    IQR <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.3 * IQR
    
    # Find indices of outliers
    col_outlier_indices <- which(dataframe[[col]] < lower_bound | dataframe[[col]] > upper_bound)
    
    # Append outlier indices to the vector
    outlier_indices <- c(outlier_indices, col_outlier_indices)
  }
  
  # Remove duplicate indices
  outlier_indices <- unique(outlier_indices)
  
  cleaned_data <- dataframe[-outlier_indices, ]
  
  return(cleaned_data)
}

cleaned_whitewine <- remove_outliers(Whitewine)

dim(Whitewine)
dim(cleaned_whitewine)

create_boxplots(Whitewine)
create_boxplots(cleaned_whitewine)

#par(mfrow=c(1,2))

# Loop to get before and after boxplot for each feature
for (feature_name in colnames(Whitewine)) {
  boxplot(Whitewine[[feature_name]], main = paste("Before:", feature_name), ylab = feature_name)
  boxplot(cleaned_whitewine[[feature_name]], main = paste("After:", feature_name), ylab = feature_name)
}

cleaned_quality <- cleaned_whitewine$quality
cleaned_quality

# Taking only the first 11 features
cleaned_chemical_properties <- subset(cleaned_whitewine, select = -quality)
length(cleaned_chemical_properties)

# Scaling chemical properties
scaled_cleaned_chemical_properties <- as.data.frame(scale(cleaned_chemical_properties))
head(scaled_cleaned_chemical_properties)


# NB Clust
nb_results <- NbClust(scaled_cleaned_chemical_properties, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
print(nb_results)

# Silhouette
fviz_nbclust(scaled_cleaned_chemical_properties, kmeans,method = "silhouette")

# Elbow Method
fviz_nbclust(scaled_cleaned_chemical_properties, kmeans,method = "wss")

# Gap Stat
fviz_nbclust(scaled_cleaned_chemical_properties, kmeans, method = "gap_stat")


# Optimal k from the automated methods
optimal_k <- 2

set.seed(100)

# Perform k-means clustering
kmeans_result <- kmeans(scaled_cleaned_chemical_properties, centers = optimal_k, nstart = 100)

fviz_cluster(kmeans_result, data = scaled_cleaned_chemical_properties, geom = "point", stand = FALSE)

# Display cluster centers
print(kmeans_result$centers)

# Display clustered results
cluster_labels <- kmeans_result$cluster
print(cluster_labels)


# Total sum of squares (TSS)
tss <- kmeans_result$tot.withinss + kmeans_result$betweenss

# Between-cluster sum of squares (BSS)
bss <- kmeans_result$betweenss

# Within-cluster sum of squares (WSS)
wss <- kmeans_result$tot.withinss

# Ratio of BSS to TSS
bss_tss_ratio <- bss / tss

# Display evaluation metrics
print(paste("BSS:", bss))
print(paste("WSS:", wss))
print(paste("BSS/TSS Ratio:", bss_tss_ratio))

# Calculate silhouette widths
sil_width <- silhouette(kmeans_result$cluster, dist(scaled_cleaned_chemical_properties))

fviz_silhouette(sil_width, geom = "bar", col = kmeans_result$cluster)






# 2nd SUBTASK


# Perform Principal Component Analysis (PCA)
pca_result <- prcomp(cleaned_chemical_properties, scale. = TRUE)
summary(pca_result)

fviz_eig(pca_result, addlabels = TRUE)

pca_result$rotation

# Get cumulative proportion
cumulative_proportion <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
print(cumulative_proportion)

# Identify the number of principal components explaining >85% of variance
selected_pc <- which(cumulative_proportion > 0.85)[1]
print(selected_pc)


plot(cumulative_proportion[0:11], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 7, col="blue", lty=5)
abline(h = 0.8882769 , col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC7"),
       col=c("blue"), lty=5, cex=0.6)


# Create transformed dataset
transformed_pca_df <- as.data.frame(pca_result$x[, 1:selected_pc])
head(transformed_pca_df)
View(transformed_pca_df)


nb_results <- NbClust(transformed_pca_df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
print(nb_results)
fviz_nbclust(transformed_pca_df, kmeans,method = "silhouette")
fviz_nbclust(transformed_pca_df, kmeans,method = "wss")
fviz_nbclust(transformed_pca_df, kmeans, method = "gap_stat")


optimal_k <- 2
set.seed(100)
kmeans_result <- kmeans(transformed_pca_df, centers = optimal_k, nstart = 100)
fviz_cluster(kmeans_result, data = transformed_pca_df, geom = "point", stand = FALSE)

print(kmeans_result$centers)
cluster_labels <- kmeans_result$cluster
print(cluster_labels)

tss <- kmeans_result$tot.withinss + kmeans_result$betweenss
bss <- kmeans_result$betweenss
wss <- kmeans_result$tot.withinss
bss_tss_ratio <- bss / tss

print(paste("BSS:", bss))
print(paste("WSS:", wss))
print(paste("BSS/TSS Ratio:", bss_tss_ratio))

sil_width <- silhouette(kmeans_result$cluster, dist(transformed_pca_df))
fviz_silhouette(sil_width, geom = "bar", col = kmeans_result$cluster)

# Calculate the Calinski-Harabasz index
ch_index <- CH.IDX(transformed_pca_df, kmax = 10, kmin = 2, method = "kmeans", nstart = 100)
ch_index

plot(ch_index$k, ch_index$CH, type = "b", xlab = "Number of clusters", ylab = "Calinski-Harabasz index", main = "Calinski-Harabasz Index Plot")

