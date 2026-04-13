#' G-Mapper Algorithm
#'
#' Implements a Mapper algorithm using Anderson-Darling tests
#' and Gaussian Mixture Models (GMM) to automatically learn the cover.
#'
#' @param original_data Original dataframe, not the filter values.
#' @param filter_values A data frame or matrix of the data to be analysed (1-D).
#' @param AD_threshold Critical value for the Anderson-Darling test
#' @param g_overlap The geometric overlap percentage when splitting an interval
#' @param methods Specify the clustering method to be used, e.g., "hclust" or "kmeans".
#' @param method_params A list of parameters for the clustering method.
#' @param num_cores Number of cores to use for parallel computing.
#' @return A MapperAlgo object same as MapperAlgo output
#' @importFrom mclust Mclust
#' @importFrom nortest ad.test
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel makeCluster stopCluster
#' @importFrom stats var
#' @export
GMapperAlgo <- function(
    original_data,
    filter_values,
    AD_threshold = 10,
    g_overlap = 0.1,
    methods,
    method_params = list(),
    num_cores = 1
) {

  filter_values <- as.numeric(unlist(filter_values)) # Force to 1D vector for AD test
  original_data <- as.data.frame(original_data)

  # Start the split with the min and max values of the 1D filter
  init_a <- min(filter_values)
  init_b <- max(filter_values)

  learned_intervals <- recursive_gaussian_split(init_a, init_b, filter_values,
                                                AD_threshold, g_overlap, depth = 1)
  num_levelsets <- length(learned_intervals)

  # Convert the learned geometric intervals back to point indices (Pull-back)
  level_sets_indices <- list()
  for (i in 1:num_levelsets) {
    intv <- learned_intervals[[i]]
    level_sets_indices[[i]] <- which(filter_values >= intv[1] & filter_values <= intv[2])
  }

  cat(sprintf("Total intervals: %d\n", num_levelsets))

  vertex_index <- 0
  level_of_vertex <- c()
  points_in_vertex <- list()
  points_in_level_set <- vector("list", num_levelsets)
  vertices_in_level_set <- vector("list", num_levelsets)

  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  results <- foreach(lsfi = 1:num_levelsets,
                     .packages = c("cluster"),
                     .export = c("perform_clustering",
                                 "cluster_cutoff_at_first_empty_bin",
                                 "find_best_k_for_kmeans")) %dopar% {

                                   points_in_level_set <- level_sets_indices[[lsfi]]

                                   if (length(points_in_level_set) == 0) {
                                     return(list(clustering_result = list(num_vertices=0), points_in_level_set = integer(0)))
                                   }

                                   clustering_result <- perform_clustering(
                                     original_data,
                                     data.frame(filter_values),
                                     points_in_level_set,
                                     methods,
                                     method_params
                                   )

                                   list(
                                     clustering_result = clustering_result,
                                     points_in_level_set = points_in_level_set
                                   )
                                 }
  stopCluster(cl)

  for (lsfi in 1:num_levelsets) {
    clustering_result <- results[[lsfi]]$clustering_result
    points_in_level_set[[lsfi]] <- results[[lsfi]]$points_in_level_set

    num_vertices_in_this_level <- clustering_result$num_vertices
    level_external_indices <- clustering_result$external_indices
    level_internal_indices <- clustering_result$internal_indices

    if (num_vertices_in_this_level > 0) {
      vertices_in_level_set[[lsfi]] <- vertex_index + (1:num_vertices_in_this_level)

      for (j in 1:num_vertices_in_this_level) {
        vertex_index <- vertex_index + 1
        level_of_vertex[vertex_index] <- lsfi
        points_in_vertex[[vertex_index]] <- level_external_indices[level_internal_indices == j]
      }
    }
  }

  num_vertices <- vertex_index
  adja <- matrix(0, nrow = num_vertices, ncol = num_vertices)

  if (num_vertices > 1) {
    for (i in 1:(num_vertices - 1)) {
      pts_i <- points_in_vertex[[i]]
      level_i <- level_of_vertex[i]

      for (j in (i + 1):num_vertices) {
        if (level_i != level_of_vertex[j]) {
          pts_j <- points_in_vertex[[j]]
          if (length(intersect(pts_i, pts_j)) > 0) {
            adja[i, j] <- 1
            adja[j, i] <- 1
          }
        }
      }
    }
  }

  if (num_vertices > 0 && sum(adja) == 0) {
    warning("No edges were created in the Mapper graph")
  }

  mapperoutput <- list(adjacency = adja,
                       num_vertices = num_vertices,
                       level_of_vertex = level_of_vertex,
                       points_in_vertex = points_in_vertex,
                       points_in_level_set = points_in_level_set,
                       vertices_in_level_set = vertices_in_level_set,
                       input_params = list(
                         AD_threshold = AD_threshold,
                         g_overlap = g_overlap,
                         methods = methods,
                         method_params = method_params
                       ))

  class(mapperoutput) <- "G-Mapper"
  return(mapperoutput)
}
