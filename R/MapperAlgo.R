#' Mapper Algorithm
#'
#' Implements the Mapper algorithm for Topological Data Analysis (TDA).
#' It divides data into intervals, applies clustering within each interval, and constructs a
#' simplicial complex representing the structure of the data.
#'
#' @param original_data Original dataframe, not the filter values.
#' @param filter_values A data frame or matrix of the data to be analysed.
#' @param intervals An integer specifying the number of intervals.
#' @param interval_width The width of each interval.
#' @param percent_overlap Percentage of overlap between consecutive intervals.
#' @param methods Specify the clustering method to be used, e.g., "hclust" or "kmeans". Mutually exclusive with `method_mlr`.
#' @param method_params A list of parameters for the clustering method.
#' @param method_mlr An mlr3cluster Learner, e.g. mlr3cluster::lrn("clust.kmeans", centers = 3). Mutually exclusive with `methods`.
#' @param cover_type Type of interval, either 'stride' or 'extension'.
#' @param num_cores Number of cores to use for parallel computing.
#' @return A list containing the Mapper graph components:
#' \describe{
#'   \item{adjacency}{The adjacency matrix of the Mapper graph.}
#'   \item{num_vertices}{The number of vertices in the Mapper graph.}
#'   \item{level_of_vertex}{A vector specifying the level of each vertex.}
#'   \item{points_in_vertex}{A list of the indices of the points in each vertex.}
#'   \item{points_in_level_set}{A list of the indices of the points in each level set.}
#'   \item{vertices_in_level_set}{A list of the indices of the vertices in each level set.}
#' }
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @import foreach
#' @export
MapperAlgo <- function(
    original_data,
    filter_values, # dist_df[,1:col]
    percent_overlap, # 50
    methods = NULL,
    method_params = list(), # params in each clustering method for 'methods'
    method_mlr = NULL,
    cover_type = 'extension',
    intervals = NULL,
    interval_width = NULL,
    num_cores = 1
) {

  using_new_method <- param_condition(methods, method_params, method_mlr, has_method_params = !missing(method_params))

  filter_values <- data.frame(filter_values)
  original_data <- as.data.frame(original_data)

  num_points <- dim(filter_values)[1] # row

  # define some vectors of length k = number of columns
  filter_min <- as.vector(sapply(filter_values, min))
  filter_max <- as.vector(sapply(filter_values, max))
  L <- (filter_max - filter_min)

  # four conditions:
  # 1. No intervals, with width
  # 2. No intervals, no width : This couldn't be computed
  # 3. Intervals, with width
  # 4. Intervals, no width
  if (is.null(intervals) & !is.null(interval_width)) {
    # if only width is specified, calculate the number of intervals
    if (cover_type == 'stride') {
      # stride: n = ceil((L - w) / (w*(1 - p))) + 1, L<=w → n=1
      stride <- interval_width * (1 - percent_overlap/100)
      num_intervals <- ifelse(
        L <= interval_width,
        1L,
        as.integer(ceiling((L - interval_width) / pmax(stride, .Machine$double.eps)) + 1L)
      )
    } else if (cover_type == 'extension') {
      # extension: n = ceil(L / w - p/100)
      num_intervals <- pmax(1L, as.integer(ceiling(L / interval_width - percent_overlap/100)))
    } else {
      stop("cover_type must be 'stride' or 'extension'")
    }

  } else if (!is.null(intervals) & is.null(interval_width)) {
    # if only intervals is specified, calculate the widths
    num_intervals <- rep(intervals, ncol(filter_values)) # rep(2,4) = (2,2,2,2)
    interval_width <- (filter_max - filter_min) / num_intervals
  } else {
     stop("Invalid combination of intervals and interval_width.")
  }

  num_levelsets <- prod(num_intervals)

  points_in_level_set <- lapply(seq_len(num_levelsets), function(lsfi) {
    cover_points(lsfi, filter_min, interval_width, percent_overlap, filter_values, num_intervals, cover_type)
  })

  clustering_results <- cluster_level_sets(
    points_in_level_set, original_data, filter_values,
    methods, method_params, method_mlr, using_new_method, num_cores
  )
  vertices <- build_vertices(clustering_results)

  adja <- mapper_adjacency(filter_values, vertices$num_vertices, num_levelsets, num_intervals,
                            vertices$vertices_in_level_set, vertices$points_in_vertex)

  new_mapper_output(
    adja, vertices, points_in_level_set,
    input_params = list(
      percent_overlap = percent_overlap,
      methods = methods,
      method_params = method_params,
      method_mlr = method_mlr,
      cover_type = cover_type,
      intervals = intervals,
      interval_width = interval_width
    ),
    class_name = "Mapper"
  )
}
