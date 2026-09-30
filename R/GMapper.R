#' G-Mapper Algorithm
#'
#' Implements a Mapper algorithm using Anderson-Darling tests
#' and Gaussian Mixture Models (GMM) to automatically learn the cover.
#'
#' @param original_data Original dataframe, not the filter values.
#' @param filter_values A data frame or matrix of the data to be analysed (1-D).
#' @param AD_threshold Critical value for the Anderson-Darling test
#' @param g_overlap The geometric overlap percentage when splitting an interval
#' @param methods Specify the clustering method to be used, e.g., "hclust" or "kmeans". Mutually exclusive with `method_mlr`.
#' @param method_params A list of parameters for the clustering method.
#' @param method_mlr An mlr3cluster Learner, e.g. mlr3cluster::lrn("clust.kmeans", centers = 3). Mutually exclusive with `methods`.
#' @param num_cores Number of cores to use for parallel computing.
#' @return A MapperAlgo object same as MapperAlgo output
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel makeCluster stopCluster
#' @importFrom stats var
#' @export
GMapperAlgo <- function(
    original_data,
    filter_values,
    AD_threshold = 10,
    g_overlap = 0.1,
    methods = NULL,
    method_params = list(), # params in each clustering method for 'methods'
    method_mlr = NULL,
    num_cores = 1
) {

  using_new_method <- param_condition(methods, method_params, method_mlr, has_method_params = !missing(method_params))

  filter_values <- as.numeric(unlist(filter_values)) # Force to 1D vector for AD test
  original_data <- as.data.frame(original_data)

  # Start the split with the min and max values of the 1D filter
  init_a <- min(filter_values)
  init_b <- max(filter_values)

  learned_intervals <- recursive_gaussian_split(init_a, init_b, filter_values, AD_threshold, g_overlap, depth = 1)
  num_levelsets <- length(learned_intervals)

  # Convert the learned geometric intervals back to point indices (Pull-back)
  level_sets_indices <- list()
  for (i in 1:num_levelsets) {
    intv <- learned_intervals[[i]]
    level_sets_indices[[i]] <- which(filter_values >= intv[1] & filter_values <= intv[2])
  }

  cat(sprintf("Total intervals: %d\n", num_levelsets))

  clustering_results <- cluster_level_sets(
    level_sets_indices, original_data, data.frame(filter_values),
    methods, method_params, method_mlr, using_new_method, num_cores
  )
  vertices <- build_vertices(clustering_results)
  adja <- overlap_adjacency(vertices$points_in_vertex, vertices$level_of_vertex)

  new_mapper_output(
    adja, vertices, level_sets_indices,
    input_params = list(
      AD_threshold = AD_threshold,
      g_overlap = g_overlap,
      methods = methods,
      method_params = method_params,
      method_mlr = method_mlr
    ),
    class_name = "G-Mapper"
  )
}
