# Imports
library(readxl)
library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
install.packages("fmsb")
library(fmsb)

df <- read_excel("project cluster.xlsx")
set.seed(0)
df_original <- df
head(df)

df$average.price <- as.numeric(df$average.price)
df$persons <- df$number.of.adults + df$number.of.children
df$nights <- df$number.of.weekend.nights + df$number.of.week.nights

mapping_market <- c(
  "Online" = 0,
  "Offline" = 1,
  "Corporate" = 2,
  "Complementary" = 3,
  "Aviation" = 4
)

# Drop columns
df_cluster <- df %>% select(-c(
  number.of.adults,
  number.of.children,
  number.of.weekend.nights,
  number.of.week.nights,
  type.of.meal,
  P.C,
  P.not.C,
  Booking_ID,
  room.type,
  date.of.reservation,
  special.requests,
  repeated,
  booking.status
))

df_cluster$market.segment.type <- as.numeric(recode(df_cluster$market.segment.type, !!!mapping_market))

head(df_cluster)

standard_scale <- function(x) {
  (x - mean(x)) / sqrt(mean((x - mean(x))^2))  
}

# Select ONLY numeric columns
numeric_cols <- sapply(df, is.numeric)

df_numeric <- df[, numeric_cols]

# Apply Python-style scaling
df_scaled <- as.data.frame(apply(df_numeric, 2, standard_scale))

elbow_silhouette_analysis <- function(df_scaled, max_k = 10) {
  
  elbow <- c()
  silhouette_scores <- c()
  K <- 2:max_k
  for (k in K) {
    kmeans_model <- kmeans(df_scaled, centers = k, nstart = 25)
    
    # Inertia = total within-cluster sum of squares
    elbow <- c(elbow, kmeans_model$tot.withinss)
    
    # Silhouette score
    sil <- silhouette(kmeans_model$cluster, dist(df_scaled))
    silhouette_scores <- c(silhouette_scores, mean(sil[, 3]))
  }
  
  # Plot Elbow
  df_elbow <- data.frame(K = K, Elbow = elbow)
  p1 <- ggplot(df_elbow, aes(x = K, y = Elbow)) +
    geom_line() + geom_point() +
    ggtitle("Elbow Method") +
    xlab("k") + ylab("Elbow")
  
  # Plot Silhouette
  df_sil <- data.frame(K = K, Silhouette = silhouette_scores)
  p2 <- ggplot(df_sil, aes(x = K, y = Silhouette)) +
    geom_line(color = "green") + geom_point(color = "green") +
    ggtitle("Silhouette Scores") +
    xlab("k") + ylab("Silhouette")
  
  # Show plots side by side
  gridExtra::grid.arrange(p1, p2, ncol = 2)
  
  # Best k based on max silhouette
  best_k <- K[which.max(silhouette_scores)]
  print(paste("Best k:", best_k))
  
  return(best_k)
}

# Run
best_k <- elbow_silhouette_analysis(df_scaled)

# KMeans Clustering
kmeans_res <- kmeans(df_scaled, centers = best_k, nstart = 25)
kmeans_labels <- kmeans_res$cluster

interpret_clusters <- function(df_vars_only, labels, verbose = TRUE) {
  df_temp <- df_vars_only
  df_temp$cluster <- labels
  
  # Compute cluster centers
  centers <- df_temp %>%
    group_by(cluster) %>%
    summarise(across(everything(), mean)) %>%
    mutate(across(where(is.numeric), \(x) round(x, 3)))
  
  if (verbose) {
    cat("\n===== ORIGINAL-SCALE CLUSTER CENTERS =====\n")
    print(centers)
    
    cat("\n===== INTERPRETATION =====\n")
  }
  
  for (i in 1:nrow(centers)) {
    row <- centers[i, ]
    c <- row$cluster
    
    cat(sprintf("\n--- Cluster %s ---\n", c))
    cat(sprintf("Lead time: %.2f\n", row$lead.time))
    cat(sprintf("Price: %.2f\n", row$average.price))
    cat(sprintf("Persons: %.2f\n", row$persons))
    cat(sprintf("Nights: %.2f\n", row$nights))
    cat(sprintf("Parking: %.2f\n", row$car.parking.space))
    cat(sprintf("Market: %.2f\n", row$market.segment.type))
  }
  return(centers)
}

cancellation_percentage <- function(df_original, labels, cluster_name) {
  df_temp <- df_original
  df_temp[[cluster_name]] <- labels
  
  summary <- df_temp %>%
    group_by(!!sym(cluster_name), booking.status) %>%
    tally() %>%
    group_by(!!sym(cluster_name)) %>%
    mutate(prop = n / sum(n) * 100) %>%
    select(!!sym(cluster_name), booking.status, prop) %>%
    mutate(prop = round(prop, 2))
  
  cat(sprintf("\n===== Cancellation Percentages for %s =====\n\n", cluster_name))
  print(summary)
  return(summary)
}

cancel_kmeans <- cancellation_percentage(df_original, kmeans_labels, "cluster_kmeans")
centers_kmeans <- interpret_clusters(df_cluster, kmeans_labels)

# Hierarchical Clustering
hc <- hclust(dist(df_scaled), method = "average")
hc_labels <- cutree(hc, k = best_k)

# Dendrogram
plot(hc, hang = -1, main = "Hierarchical Clustering Dendrogram (Average)")

centers_hc <- interpret_clusters(df_cluster, hc_labels)
cancel_hc <- cancellation_percentage(df_original, hc_labels, "cluster_hc")


df_cluster$cluster_kmeans <- kmeans_labels
df_summary <- df_cluster %>%
   group_by(cluster_kmeans) %>%
   summarise(
     car_parking = mean(car.parking.space),
    lead_time = mean(lead.time),
     market_segment = mean(market.segment.type),
     avg_price = mean(average.price),
     persons = mean(persons),
     nights = mean(nights)
   ) %>%
  pivot_longer(cols = -cluster_kmeans, names_to = "Variable", values_to = "Value")
 
ggplot(df_summary, aes(x = factor(cluster_kmeans), y = Value, fill = factor(cluster_kmeans))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Cluster Profile Comparison (Faceted View)",
       x = "Cluster",
       y = "Mean Value (Original Scale)",
       fill = "Cluster") +
  theme_minimal()


radar_data <- df_summary %>%
  pivot_wider(names_from = Variable, values_from = Value)

radar_values <- radar_data[,-1]


radarplot <- as.data.frame(rbind(
  apply(radar_values, 2, max),   # Max values
  apply(radar_values, 2, min),   # Min values
  radar_values                  # Cluster rows
))

rownames(radarplot) <- c("Max", "Min", paste0("Cluster ", radar_data$cluster_kmeans))

radarchart(
  radarplot,
  axistype = 1,
  pcol = c("red", "blue"),
  plwd = 3,
  plty = 1,
  title = "Cluster Profile Radar Chart"
)
