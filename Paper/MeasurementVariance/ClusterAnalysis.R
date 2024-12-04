extract_best_runs <- function(res) {
  states <- res$states
  params <- res$params
  errors <- res$metrices
  errors <- Reduce(rbind, errors)
  states <- Reduce(rbind, states)
  params <- Reduce(rbind, params)
  params <- lapply(unique(errors$dataset), function(x) {
    params_subset <- params[params$dataset == x, ]
    errors_subset <- errors[errors$dataset == x, ]
    errors_subset <- errors_subset[order(errors_subset$MeanSquareError), ][1:50, ]
    res <- params_subset[params_subset$repetition %in% errors_subset$repetition, ]
    res <- res[, 1:4]
    res$error <- errors_subset$MeanSquareError
    return(res)
  })
  params <- Reduce(rbind, params)
  return(params)
}

load_params <- function(path) {
  load(path)
  params <- extract_best_runs(res[[1]])
}

p_dba <- load_params("dba_100Runs.RData")
p_ida <- load_params("ida_100.RData")
p_gda <- load_params("gda_100.RData")

# Correlation
library(ggcorrplot)
cor_fct <- function(df) {
  corr <- cor(df[, 1:4])
  ggcorrplot(corr,
    hc.order = TRUE,
    type = "lower",
    ggtheme = ggplot2::theme_gray,
    colors = c("#6D9EC1", "white", "#E46726"),
    outline.col = "white",
    lab = TRUE
  )
}

cor_fct(p_dba)
cor_fct(p_ida)
cor_fct(p_gda)


# K-means clustering
library(ggplot2)
library(cowplot)
cluster_kmeans <- function(df, centers = 3) {
  kmeans_result <- kmeans(df[, 1:4], centers = centers)
  df$cluster <- as.factor(kmeans_result$cluster)
  return(df)
}

p_dba <- cluster_kmeans(p_dba, centers = 3)
p_ida <- cluster_kmeans(p_ida, centers = 3)
p_gda <- cluster_kmeans(p_gda, centers = 3)

plot_clusters <- function(df, title) {
  grid <- expand.grid(1:4, 1:4)
  grid <- grid[grid$Var1 != grid$Var2, ]
  plot_clusters_helper <- function(df, idx1, idx2) {
    ggplot(df, aes(x = df[, idx1], y = df[, idx2], color = cluster)) +
      geom_point(size = 3, alpha = 0.7) +
      labs(title = title, x = names(df)[idx1], y = names(df)[idx2]) +
      theme_minimal() +
      scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A"))
  }
  l <- list()
  for (i in seq_len(nrow(grid))) {
    idx1 <- grid[i, 1]
    idx2 <- grid[i, 2]
    l[[i]] <- plot_clusters_helper(df, idx1, idx2)
  }
  plot_grid(plotlist = l, nrow = 3)
}

plot_clusters(p_dba, "Clusters in p_dba")
plot_clusters(p_ida, "Clusters in p_ida")
plot_clusters(p_gda, "Clusters in p_gda")

# Perform PCA and add clusters
perform_pca <- function(df) {
  pca_result <- prcomp(df[, 1:4], scale. = TRUE) # Perform PCA
  df$pca1 <- pca_result$x[, 1] # First principal component
  df$pca2 <- pca_result$x[, 2] # Second principal component
  return(df)
}

p_dba <- perform_pca(p_dba)
p_ida <- perform_pca(p_ida)
p_gda <- perform_pca(p_gda)

# Function to plot clusters with ellipses on PCA results
plot_clusters_pca <- function(df, title) {
  ggplot(df, aes(x = pca1, y = pca2, color = cluster)) +
    geom_point(size = 3, alpha = 0.7) + # Scatter plot
    stat_ellipse(aes(color = cluster), level = 0.95) + # Add ellipses
    labs(title = title, x = "PC1", y = "PC2") +
    theme_minimal() +
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A"))
}

# Plot PCA with clusters and ellipses
plot_clusters_pca(p_dba, "Clusters in p_dba (PCA)")
plot_clusters_pca(p_ida, "Clusters in p_ida (PCA)")
plot_clusters_pca(p_gda, "Clusters in p_gda (PCA)")







# Perform PCA and add clusters
perform_pca <- function(df) {
  pca_result <- prcomp(df[, 1:4], scale. = TRUE)
  df$pca1 <- pca_result$x[, 1]
  df$pca2 <- pca_result$x[, 2]
  pca_result
}

pca_dba <- perform_pca(p_dba)
pca_ida <- perform_pca(p_ida)
pca_gda <- perform_pca(p_gda)

plot_clusters_pca_with_arrows <- function(df, pca_result, title) {
  loadings <- pca_result$rotation[, 1:2]
  p <- ggplot(df, aes(x = pca1, y = pca2)) +
    geom_point(aes(color = cluster), size = 3, alpha = 0.7) + # Scatter plot
    stat_ellipse(aes(color = cluster), level = 0.95) + # Add ellipses
    labs(title = title, x = "PC1", y = "PC2") +
    theme_minimal() +
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A"))

  arrows <- data.frame(
    xend = loadings[, 1], yend = loadings[, 2],
    variable = rownames(loadings)
  )

  pa <- p +
    geom_segment(
      data = arrows, aes(x = 0, y = 0, xend = xend, yend = yend),
      arrow = arrow(length = unit(0.2, "inches")), color = "black"
    ) +
    geom_text(
      data = arrows, aes(x = xend, y = yend, label = variable),
      size = 3, hjust = -0.2, vjust = -0.2
    )
  return(pa)
}

plot_clusters_pca_with_arrows(p_dba, pca_dba, "Clusters in p_dba (PCA)")
plot_clusters_pca_with_arrows(p_ida, pca_ida, "Clusters in p_ida (PCA)")
plot_clusters_pca_with_arrows(p_gda, pca_gda, "Clusters in p_gda (PCA)")
