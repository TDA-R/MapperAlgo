#' Perform clustering within a level set
#'
#' @param points_in_this_level Points in the current level set.
#' @param filter_values The filter values.
#' @param methods Specify the clustering method to be used, e.g., "hclust" or "kmeans".
#' @param method_params A list of parameters for the clustering method.
#' @return A list containing the number of vertices, external indices, and internal indices.
#' @importFrom stats as.dist hclust cutree dist kmeans
#' @export
perform_clustering <- function(
    points_in_this_level,
    filter_values,
    methods,
    method_params = list()
) {
  num_points_in_this_level <- length(points_in_this_level)
  
  if (num_points_in_this_level == 0) {
    return(list(num_vertices = 0, external_indices = NULL, internal_indices = NULL))
  }
  
  if (num_points_in_this_level == 1) {
    return(list(num_vertices = 1, external_indices = points_in_this_level, internal_indices = c(1)))
  }

  clustering_methods <- list(
    hierarchical = function() {

      sub <- filter_values[points_in_this_level, , drop = FALSE]
      level_dist_object <- dist(sub)

      level_max_dist <- max(level_dist_object)
      level_hclust <- hclust(level_dist_object, method = method_params$method)
      level_heights <- level_hclust$height
      # find the best cutoff
      level_cutoff <- cluster_cutoff_at_first_empty_bin(level_heights, level_max_dist, method_params$num_bins_when_clustering)
      level_external_indices <- points_in_this_level[level_hclust$order]
      level_internal_indices <- as.vector(cutree(list(
        merge = level_hclust$merge,
        height = level_hclust$height,
        labels = level_external_indices), h = level_cutoff))
      num_vertices_in_this_level <- max(level_internal_indices)
      list(level_external_indices, level_internal_indices, num_vertices_in_this_level)
    },
    kmeans = function() {
      max_clusters <- min(method_params$max_kmeans_clusters, num_points_in_this_level)
      level_filter_values <- filter_values[points_in_this_level, , drop = FALSE]
      if (max_clusters < nrow(level_filter_values)) {
        level_kmean <- kmeans(level_filter_values, centers = max_clusters)
        list(
          points_in_this_level[order(level_kmean$cluster)], 
          as.vector(level_kmean$cluster), 
          max(level_kmean$cluster)
        )
      } else {
        list(points_in_this_level, rep(1, num_points_in_this_level), 1)
      }
    },
    dbscan = function() {
      level_filter_values <- filter_values[points_in_this_level, , drop = FALSE]
      dbscan_result <- dbscan::dbscan(
        level_filter_values, 
        eps = method_params$eps, 
        minPts = method_params$minPts
      )
      if (max(dbscan_result$cluster) > 0) {
        list(
          points_in_this_level[order(dbscan_result$cluster)], 
          as.vector(dbscan_result$cluster), 
          max(dbscan_result$cluster)
        )
      } else {
        list(points_in_this_level, rep(1, num_points_in_this_level), 1)
      }
    },
    pam = function() {
      level_filter_values <- filter_values[points_in_this_level, , drop = FALSE]
      if (nrow(level_filter_values) >= 2) {
        num_clusters <- min(method_params$num_clusters, nrow(level_filter_values) - 1)
        pam_result <- cluster::pam(level_filter_values, k = num_clusters)
        if (max(pam_result$clustering) > 0) {
          list(
            points_in_this_level[order(pam_result$clustering)], 
            as.vector(pam_result$clustering), 
            max(pam_result$clustering)
          )
        } else {
          list(points_in_this_level, rep(1, num_points_in_this_level), 1)
        }
      } else {
        list(points_in_this_level, rep(1, num_points_in_this_level), 1)
      }
    }
  )
  
  if (!methods %in% names(clustering_methods)) {
    stop("Invalid method provided")
  }
  clustering_result <- clustering_methods[[methods]]()
  
  return(list(
    num_vertices = clustering_result[[3]],
    external_indices = clustering_result[[1]],
    internal_indices = clustering_result[[2]]
  ))
}

#' Cut the hierarchical clustering tree to define clusters
#'
#' @param heights Heights of the clusters.
#' @param diam Diameter of the clusters.
#' @param num_bins_when_clustering Number of bins when clustering.
#' @return The cutoff height for the clusters.
#' @importFrom graphics hist
#' @export
cluster_cutoff_at_first_empty_bin <- function(heights, diam, num_bins_when_clustering) {
  if (length(heights) == 1) {
    if (heights == diam) {
      return(Inf)
    }
  }
  # keep bin_breaks cover in the range
  min_height <- min(heights)
  max_height <- max(c(heights, diam))
  # if bins is too small, we need to add a small number to max_height to make sure the last bin is not empty
  if (min_height == max_height) {
    bin_breaks <- seq(from = min_height, to = max_height + 1e-6, length.out = num_bins_when_clustering + 1)
  } else {
    bin_breaks <- seq(from = min_height, to = max_height, length.out = num_bins_when_clustering + 1)
  }
  
  myhist <- hist(c(heights, diam), breaks = bin_breaks, plot = FALSE)
  z <- (myhist$counts == 0)
  
  if (sum(z) == 0) {
    return(Inf)
  } else {
    cutoff <- myhist$mids[min(which(z == TRUE))]
    return(cutoff)
  }
}

#' Find the optimal number of clusters for k-means
#'
#' This function calculates the total within-cluster sum of squares (WSS) for a range
#' of cluster numbers and identifies the best number of clusters (k) based on the 
#' elbow method.
#'
#' @param dist_object A distance matrix or data frame containing the data to be clustered.
#' @param max_clusters The maximum number of clusters to test for k-means. Default is 10.
#' @return The optimal number of clusters (k) based on the elbow method.
#' @importFrom stats kmeans
#' @export
find_best_k_for_kmeans <- function(dist_object, max_clusters = 10) {
  # elbow method
  wss_values <- numeric(max_clusters)
  
  for (k in 1:max_clusters) {
    kmean_result <- kmeans(dist_object, centers = k, nstart = 25)  # nstart for more stable results
    wss_values[k] <- kmean_result$tot.withinss  # Total within-cluster sum of squares
  }
  
  differences <- diff(wss_values)
  second_differences <- diff(differences)
  
  best_k <- which(second_differences == min(second_differences)) + 1
  
  return(best_k)
}