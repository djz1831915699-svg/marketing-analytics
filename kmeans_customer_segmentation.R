# MARK5400 - Customer Analytics
# Practice Assignment: K-Means Customer Segmentation
# Based on the Geico insurance example in the lecture
# This is a learning exercise with simulated data (not real customers).
# Only base R is needed.

# Question 1: Create a dataset of 12 customers.
# Scores range from 1 (not important) to 10 (very important).
customers <- data.frame(
  Customer = LETTERS[1:12],
  Premium_Savings = c(9, 8, 9, 8, 2, 3, 1, 2, 7, 8, 6, 7),
  Neighborhood_Agent = c(2, 3, 3, 1, 9, 8, 9, 7, 8, 9, 8, 9)
)
print(customers)

# Question 2: Compute Euclidean distance between Customer A and E.
d_AE <- sqrt(
  (customers$Premium_Savings[1] - customers$Premium_Savings[5])^2 +
  (customers$Neighborhood_Agent[1] - customers$Neighborhood_Agent[5])^2
)
cat("Distance between A and E:", round(d_AE, 2), "\n\n")

# Question 3: Use K-Means to divide customers into K = 3 groups.
# We cluster on the two numeric attributes, not the customer names.
x <- customers[, c("Premium_Savings", "Neighborhood_Agent")]
set.seed(123)
K <- 3
fit <- kmeans(x, centers = K, nstart = 25)
customers$Cluster <- fit$cluster

cat("Customer assignments:\n")
print(customers[order(customers$Cluster), ], row.names = FALSE)
cat("\nCluster centroids (average scores):\n")
print(round(fit$centers, 2))
cat("\nNumber of customers in each cluster:\n")
print(fit$size)

# Question 4: Plot the clusters.
colors <- c("#2C7FB8", "#E08C33", "#33A58E")
plot(
  x$Premium_Savings, x$Neighborhood_Agent,
  col = colors[fit$cluster], pch = 19, cex = 1.6,
  xlim = c(0, 11), ylim = c(0, 11),
  xlab = "Importance of Premium Savings",
  ylab = "Importance of Neighborhood Agent",
  main = "Geico: K-Means Customer Segmentation"
)
text(x$Premium_Savings, x$Neighborhood_Agent,
     labels = customers$Customer, pos = 3, cex = 0.8)
points(fit$centers[, 1], fit$centers[, 2],
       col = colors[1:K], pch = 8, cex = 2.7, lwd = 2)
legend("bottomleft", legend = paste("Cluster", 1:K),
       col = colors[1:K], pch = 19, bty = "n", cex = 0.9)

# Question 5: Use the Elbow Method to consider the number of clusters.
set.seed(123)
k_values <- 1:6
wss <- sapply(k_values, function(k) {
  kmeans(x, centers = k, nstart = 25)$tot.withinss
})
plot(k_values, wss, type = "b", pch = 19,
     xlab = "Number of Clusters (K)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method")

# Question 6 (written answers):
# 6a. Describe each cluster using its centroid.
# 6b. Which cluster values a neighborhood agent most?
# 6c. What insurance product or marketing message suits each cluster?
# 6d. Explain why K-Means repeats assignment and centroid updates.
# Note: Cluster numbers (1/2/3) are arbitrary; interpret using centroid scores.
