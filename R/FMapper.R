#' Fuzzy Mapper Algorithm (Fixed Adjacency Calculation)
#'
#' Implements a variant of the Mapper algorithm using Fuzzy C-Means (FCM) clustering for the level sets.
#'
#' @param original_data Original dataframe, not the filter values.
#' @param filter_values A data frame or matrix of the data to be analyzed.
#' @param cluster_n Number of fuzzy clusters (c in FCM). Default is 5.
#' @param fcm_threshold Membership threshold (tau). Points with u > tau are included in the interval.
#' @param fuzzifier Fuzzifier m of fuzzy c-means (must be > 1). Controls the amount of overlap
#'   between cover elements: larger values make memberships more even, so more points pass
#'   `fcm_threshold` in several clusters (more overlap); values close to 1 approach hard
#'   k-means (little overlap). Default is 2.
#' @param methods Specify the clustering method to be used, e.g., "hclust" or "kmeans". Mutually exclusive with `method_mlr`.
#' @param method_params A list of parameters for the clustering method.
#' @param method_mlr An mlr3cluster Learner, e.g. mlr3cluster::lrn("clust.kmeans", centers = 3). Mutually exclusive with `methods`.
#' @param num_cores Number of cores to use for parallel computing.
#' @return A MapperAlgo object same as MapperAlgo output
#' @importFrom ppclust fcm
#' @importFrom inaparc kmpp
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @export
FuzzyMapperAlgo <- function(
    original_data,
    filter_values,
    cluster_n = 5,
    fcm_threshold = NULL,
    fuzzifier = 2,
    methods = NULL,
    method_params = list(), # params in each clustering method for 'methods'
    method_mlr = NULL,
    num_cores = 1
) {

  using_new_method <- param_condition(methods, method_params, method_mlr, has_method_params = !missing(method_params))

  if (!is.numeric(fuzzifier) || length(fuzzifier) != 1 || fuzzifier <= 1) {
    stop("`fuzzifier` must be a single number greater than 1.")
  }

  original_data <- as.data.frame(original_data)

  if (is.null(fcm_threshold)) {
    fcm_threshold <- min(0.1, 1 / cluster_n)
    message(paste("Auto-setting fcm_threshold to:", round(fcm_threshold, 4)))
  }

  v0 <- inaparc::kmpp(as.matrix(filter_values), k = cluster_n)$v

  res.fcm <- ppclust::fcm(as.matrix(filter_values), centers = v0, m = fuzzifier)
  U <- res.fcm$u
  num_levelsets <- ncol(U)
  level_sets_indices <- list()
  for (j in 1:num_levelsets) {
    level_sets_indices[[j]] <- which(U[, j] > fcm_threshold)
  }

  clustering_results <- cluster_level_sets(
    level_sets_indices, original_data, filter_values,
    methods, method_params, method_mlr, using_new_method, num_cores
  )
  vertices <- build_vertices(clustering_results)
  adja <- overlap_adjacency(vertices$points_in_vertex, vertices$level_of_vertex)

  new_mapper_output(
    adja, vertices, level_sets_indices,
    input_params = list(
      cluster_n = cluster_n,
      fcm_threshold = fcm_threshold,
      fuzzifier = fuzzifier,
      methods = methods,
      method_params = method_params,
      method_mlr = method_mlr
    ),
    class_name = "FMapper"
  )
}
