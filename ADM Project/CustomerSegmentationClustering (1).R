# Import data
data <- read.csv('Mall_Customers.csv')
pacman::p_load('caret', 'corrplot', 'cluster')

# Data preprocessing
data$Gender <- ifelse(data$Gender == 'Male', 0, 1)

#find and remove outliers
boxplot(data)

# Remove outliers
detect_outlier <- function(x) {
  Quantile1 <- quantile(x, probs=.25)
  Quantile3 <- quantile(x, probs=.75)
  IQR = Quantile3 - Quantile1
  x > Quantile3 + (IQR * 1.5) | x < Quantile1 - (IQR * 1.5)
}

remove_outlier <- function(dataframe, columns = names(dataframe)) {
  for (col in columns) {
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  return(dataframe)
}

data <- remove_outlier(data, colnames(data))

boxplot(data)

# Remove CustomerID from table
featureData <- subset(data, select = -c(CustomerID))

# Normalize data
featureData$Age <- scale(featureData$Age)
featureData$Annual.Income..k.. <- scale(featureData$Annual.Income..k..)
featureData$Spending.Score..1.100. <- scale(featureData$Spending.Score..1.100.)


#data after preprocessing
View(featureData)

# Exploratory data analysis
cor(featureData)
corrplot(cor(featureData))

# Plot clusters on Income and spending score
ggplot(data) + geom_point(aes(x=data$Annual.Income..k.., y=data$Spending.Score..1.100.), size=3) + theme_minimal() + labs(x = "Income", y = "Spending Score")

# Plot clusters on Age and Spending Score
ggplot(data) + geom_point(aes(x=data$Age, y=data$Spending.Score..1.100.), size=3) + theme_minimal() + labs(x = "Age", y = "Spending Score")

# Plot clusters on Age and Income
ggplot(data) + geom_point(aes(x=data$Age, y=data$Annual.Income..k..), size=3) + theme_minimal() + labs(x = "Age", y = "Income")


str(featureData)  # Check the structure and data types of the featureData dataframe
summary(featureData)  # Get a summary to see if there are any NA values or unexpected data types


# K-means clustering
trials <- list()
within_cluster_SS <- numeric(9)
for(i in 2:10) {
  trials[[i-1]] <- kmeans(featureData, centers=i, nstart = 10, iter.max = 50)
  within_cluster_SS[i-1] = trials[[i-1]]$tot.withinss
}

# Plot within_cluster_SS to find elbow point
plot(2:10, within_cluster_SS, xlab = 'Number of Clusters', ylab = 'within Cluster SS')

# Remove the Cluster factor column before clustering
featureDataForClustering <- featureData[, !(names(featureData) %in% c("Cluster"))]


 
featureData$Cluster <- factor(trials[[5]]$cluster)

# Cross-validation over number of clusters
set.seed(123)
folds <- createFolds(featureDataForClustering[, "Annual.Income..k.."], k = 5)
silhouette_scores <- matrix(nrow = 9, ncol = 5)

for(k in 2:10) {
  for(i in seq_along(folds)) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train_data <- featureDataForClustering[train_indices, ]
    kmeans_result <- kmeans(train_data, centers = k, nstart = 20)
    test_data <- featureDataForClustering[test_indices, ]
    
    test_clusters <- sapply(1:nrow(test_data), function(idx) {
      row <- test_data[idx, ]
      # Compute distances from the current row to each cluster center
      distances <- apply(kmeans_result$centers, 1, function(center) sum((row - center)^2))
      which.min(distances)
    })
    
    dists <- dist(test_data)
    sil_scores <- silhouette(test_clusters, dists)
    silhouette_scores[k - 1, i] <- mean(sil_scores[, "sil_width"])
  }
}

# Average silhouette scores across folds for each k
average_silhouette <- rowMeans(silhouette_scores)
plot(2:10, average_silhouette, type = 'b', xlab = 'Number of Clusters', ylab = 'Average Silhouette Score', main = 'K-Means Clustering Validation')

# Determine the best number of clusters
best_k <- which.max(average_silhouette) + 1
final_kmeans <- kmeans(featureDataForClustering, centers = best_k, nstart = 20)
silhouette_avg <- silhouette(final_kmeans$cluster, dist(featureDataForClustering))

# Calculating the average silhouette score for the final chosen clustering
avg_sil_width <- mean(silhouette_avg[, "sil_width"])
cat("Average silhouette width for the best chosen k (", best_k, " clusters): ", avg_sil_width, "\n")

total_within_ss <- final_kmeans$tot.withinss
cat("Total within-cluster sum of squares for the best chosen k (", best_k, " clusters): ", total_within_ss, "\n")


# Plot results with optimal cluster
final_kmeans <- kmeans(featureData, centers = best_k, nstart = 20)
data$Cluster <- as.factor(final_kmeans$cluster)

# Plot clusters on Income and spending score
ggplot(featureData) + geom_point(aes(x=data$Annual.Income..k.., y=data$Spending.Score..1.100., color=Cluster), size=3) + theme_minimal() + labs(x = "Income", y = "Spending Score")

# Plot clusters on Age and Spending Score
ggplot(featureData) + geom_point(aes(x=data$Age, y=data$Spending.Score..1.100., color=Cluster), size=3) + theme_minimal() + labs(x = "Age", y = "Spending Score")

# Plot clusters on Age and Income
ggplot(featureData) + geom_point(aes(x=data$Age, y=data$Annual.Income..k.., color=Cluster), size=3) + theme_minimal() + labs(x = "Age", y = "Income")



#pam code (K medoid)
trials <- list()
sil <- numeric(9)
wss <- numeric(9)
for(i in 2:10) {
  trials[[i-1]] <- pam(featureData, i, metric = 'euclidean')
  wss[i-1] <- trials[[i-1]]$clusinfo[i] * trials[[i-1]]$clusinfo[i*3]
  sil[i-1] <- trials[[i-1]]$silinfo$avg.width
}


plot(2:10, sil)
plot(2:10, wss)

# Cross-validation over number of clusters for k Medoid
set.seed(546)
folds <- createFolds(featureDataForClustering[, "Annual.Income..k.."], k = 5)
silhouette_scores <- matrix(nrow = 9, ncol = 5)

for(k in 2:10) {
  for(i in seq_along(folds)) {
    train_indices <- unlist(folds[-i])
    test_indices <- unlist(folds[i])
    train_data <- featureDataForClustering[train_indices, ]
    pam_result <- pam(train_data, k, metric = 'euclidean')
    test_data <- featureDataForClustering[test_indices, ]
    
    test_clusters <- sapply(1:nrow(test_data), function(idx) {
      row <- test_data[idx, ]
      # Compute distances from the current row to each cluster center
      distances <- apply(pam_result$medoids, 1, function(center) sum((row - center)^2))
      which.min(distances)
    })
    
    dists <- dist(test_data)
    sil_scores <- silhouette(test_clusters, dists)
    silhouette_scores[k - 1, i] <- mean(sil_scores[, "sil_width"])
  }
}


# Average silhouette scores across folds for each k
average_silhouette <- rowMeans(silhouette_scores)
plot(2:10, average_silhouette, type = 'b', xlab = 'Number of Clusters', ylab = 'Average Silhouette Score', main = 'K-Means Clustering Validation')

# Determine the best number of clusters
best_k <- which.max(average_silhouette) + 1
final_pam <- pam(featureDataForClustering, best_k, nstart = 20)
silhouette_avg <- silhouette(final_pam$clustering, dist(featureDataForClustering))


data$Cluster <- as.factor(final_pam$clustering)

# Plot clusters on Income and spending score
ggplot(featureData) + geom_point(aes(x=data$Annual.Income..k.., y=data$Spending.Score..1.100., color=Cluster), size=3) + theme_minimal() + labs(x = "Income", y = "Spending Score")

# Plot clusters on Age and Spending Score
ggplot(featureData) + geom_point(aes(x=data$Age, y=data$Spending.Score..1.100., color=Cluster), size=3) + theme_minimal() + labs(x = "Age", y = "Spending Score")

# Plot clusters on Age and Income
ggplot(featureData) + geom_point(aes(x=data$Age, y=data$Annual.Income..k.., color=Cluster), size=3) + theme_minimal() + labs(x = "Age", y = "Income")


