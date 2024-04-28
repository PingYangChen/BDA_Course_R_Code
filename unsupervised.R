library(cluster)
library(mclust)
library(isotree)
library(e1071)




demo_data_1 <- read.csv('unsup_data/demo_data_1.csv')
test_data_1 <- read.csv('unsup_data/test_data_1.csv')
plot(demo_data_1, pch = 16, col = '#666666AA', xlim = c(-1, 1), ylim = c(-1, 1), main = "Dataset 1")
points(test_data_1, pch = 17, col = '#4444FF')
legend("topleft", legend = c("Training Data", "Testing Data"), 
       pch = c(16, 17), col = c('#666666', '#4444FF'))

demo_data_2 <- read.csv('unsup_data/demo_data_2.csv')
test_data_2 <- read.csv('unsup_data/test_data_2.csv')
plot(demo_data_2, pch = 16, col = '#666666AA', 
     xlim = c(-1, 3), ylim = c(-1, 3), main = "Dataset 2")
points(test_data_2, pch = 17, col = '#4444FF')
legend("topleft", legend = c("Training Data", "Testing Data"), 
       pch = c(16, 17), col = c('#666666', '#4444FF'))



x <- matrix(rnorm(100), 20, 5)

dist(x, method = "euclidean")
dist(x, method = "manhattan")
dist(x, method = "manhattan")
mahalanobis(
  x, center = colMeans(x), cov = cov(x)
)

data(iris)
x <- iris[,1:4]


euc_dist <- dist(x, method = "euclidean")
image(1:nrow(x), 1:nrow(x), as.matrix(euc_dist),
      xlab = "", ylab = "")



# H
# Import the cluster package
library(cluster)
# Compute pairwise distance between samples in the training data
dist_2 <- dist(demo_data_2, method = "euclidean")
# Build hierarchical clusters
m_hc <- hclust(dist_2, method = "ward.D2")
# Draw Dendrogram
plot(m_hc)
# Cut the Dendrogram
cut_m_hc <- cutree(m_hc, k = 3)

# Build a classification model to predict for new data
# For example, logistic regression model
cla_data <- data.frame(demo_data_2, Y = cut_m_hc)
library(glmnet)
cla_m <- cv.glmnet(
  x = as.matrix(cla_data[,1:2]), y = as.factor(cla_data$Y), 
  family = "multinomial"
)
# Predict groups for testing data using logistic regression model
cla_pred_prob <- predict(
  cla_m, as.matrix(test_data_2[,1:2]), s = cla_m$lambda.min, type = "response"
)
cla_pred <- max.col(cla_pred_prob[,,1])
# Draw clustering results
plot(demo_data_2, pch = 16, col = '#888888AA', xlim = c(-1, 3), ylim = c(-1, 3), 
     main = "Dataset 2: HClust + GLM Prediction")
clust_color <- c('#2d39bd', '#f29129', '#0a8c2d')
for (i in 1:max(cla_pred)) {
  points(test_data_2[which(cla_pred == i),], pch = 17, col = clust_color[i])
}
legend("topleft", cex = 0.65, bty = "n",
       legend = c("Training Data", paste0("Testing Data: Group ", 1:max(cla_pred))), 
       pch = c(16, rep(17, max(cla_pred))), col = c('#666666', clust_color))






# K-means

predict_km <- function(km_object, new_data) {
  cen <- km_object$centers
  new_data <- as.matrix(new_data)
  if (is.null(dim(new_data))) { new_data <- matrix(new_data, 1, ncol(cen)) }
  dist2cen <- t(sapply(1:nrow(new_data), function(i) {
    sqrt(rowSums((cen - matrix(new_data[i,], nrow(cen), ncol(cen), byrow = TRUE))^2))
  }))
  colnames(dist2cen) <- rownames(km_object$centers)
  list(
    # Assign points in new_data to its nearest group
    "cluster" = max.col((-1)*dist2cen), 
    "distance" = dist2cen
  )
}


# Build K-means clusters
m_km <- kmeans(demo_data_2, centers = 3, nstart = 5)
# Predict groups for testing data using self-developed function
p_km <- predict_km(m_km, test_data_2)
# Draw clustering results
plot(demo_data_2, pch = 16, col = '#888888AA', xlim = c(-1, 3), ylim = c(-1, 3), 
     main = "Dataset 2: K-means Prediction")
clust_color <- c('#2d39bd', '#f29129', '#0a8c2d')
points(m_km$centers, pch = 20, col = clust_color)
for (i in 1:max(p_km$cluster)) {
  points(test_data_2[which(p_km$cluster == i),], pch = 17, col = clust_color[i])
}
legend("topleft", cex = 0.65, bty = "n",
       legend = c("Training Data", paste0("Testing Data: Group ", 1:max(p_km$cluster))), 
       pch = c(16, rep(17, max(p_km$cluster))), col = c('#666666', clust_color))




sil = silhouette(m_km$cluster, dist(demo_data_2)) 
plot(sil)
abline(v = mean(sil[,3]), col = "red")



max_clust <- 8
silh_list <- vector("list", length = max_clust - 1)
mean_silh_vec <- numeric(max_clust - 1)
for (i in 2:max_clust) {
  tmp_km <- kmeans(demo_data_2, centers = i, nstart = 5)
  silh = silhouette(tmp_km$cluster, dist(demo_data_2)) 
  silh_list[[i-1]] <- silh
  mean_silh_vec[i-1] <- mean(silh[,3])
}
plot(2:max_clust, mean_silh_vec, type = "l", main = "k-Means Clusters", 
     xlab = "Number of Clusters", ylab = "Average Silhouette Score")
points(2:max_clust, mean_silh_vec, pch = 16)

par(mfrow = c(2, 4))
for (i in 2:max_clust) {
  plot(silh_list[[i - 1]])
  abline(v = mean_silh_vec[i-1], col = "red")
}
par(mfrow = c(1, 1))


# GMM

# Import the mclust package
library(mclust)
# Build GMM model
m_gmm <- Mclust(demo_data_2, G = 2:4, modelNames = c("EII", "VEI", "VVE", "VVV"))
m_gmm_cen <- t(m_gmm$parameters$mean) # get cluster centers
# Predict groups for testing data
p_gmm <- predict(m_gmm, test_data_2)
p_gmm_gp <- p_gmm$classification # get predicted cluster ID
# Draw the clustering results
plot(demo_data_2, pch = 16, col = '#888888AA', xlim = c(-1, 3), ylim = c(-1, 3), 
     main = "Dataset 2: GMM Prediction")
clust_color <- c('#2d39bd', '#f29129', '#0a8c2d')
points(m_gmm_cen, pch = 20, col = clust_color)
for (i in 1:max(p_gmm_gp)) {
  points(test_data_2[which(p_gmm_gp == i),], pch = 17, col = clust_color[i])
}
legend("topleft", cex = 0.65, bty = "n",
       legend = c("Training Data", paste0("Testing Data: Group ", 1:max(p_gmm_gp))), 
       pch = c(16, rep(17, max(p_gmm_gp))), col = c('#666666', clust_color))

