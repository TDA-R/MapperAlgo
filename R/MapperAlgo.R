#' Mapper Algorithm
#'
#' Implements the Mapper algorithm for Topological Data Analysis (TDA).
#' It divides data into intervals, applies clustering within each interval, and constructs a
#' simplicial complex representing the structure of the data.
#'
#' @param filter_values A data frame or matrix of the data to be analyzed.
#' @param intervals An integer specifying the number of intervals.
#' @param interval_width The width of each interval.
#' @param percent_overlap Percentage of overlap between consecutive intervals.
#' @param methods Specify the clustering method to be used, e.g., "hclust" or "kmeans".
#' @param method_params A list of parameters for the clustering method.
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
#'
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @import foreach
#' @export
MapperAlgo <- function(
    filter_values, # dist_df[,1:col]
    percent_overlap, # 50
    methods,
    method_params = list(), # params in each clustering method
    cover_type = 'extension',
    intervals = NULL,
    interval_width = NULL,
    num_cores = 1
) {

  filter_values <- data.frame(filter_values)

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

  # initialize variables
  vertex_index <- 0
  level_of_vertex <- c()
  points_in_vertex <- list()
  points_in_level_set <- vector("list", num_levelsets)
  # store the data points owned by each individual interval
  vertices_in_level_set <- vector("list", num_levelsets)

  # Set up parallel computing
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  results <- foreach(lsfi = 1:num_levelsets,
                     .packages = c("cluster"),
                     .export = c("cover_points", "to_lsmi", "perform_clustering",
                                 "cluster_cutoff_at_first_empty_bin")) %dopar% {

                       points_in_level_set <- cover_points(
                         lsfi, filter_min, interval_width, percent_overlap,
                         filter_values, num_intervals, cover_type
                       )

                       clustering_result <- perform_clustering(
                         points_in_level_set,
                         filter_values,
                         methods,
                         method_params
                       )

                       list(
                         clustering_result = clustering_result,
                         points_in_level_set = points_in_level_set
                       )
                     }

  stopCluster(cl)

  # begin loop through all level sets
  for (lsfi in 1:num_levelsets) {

    clustering_result <- results[[lsfi]]$clustering_result
    points_in_level_set[[lsfi]] <- results[[lsfi]]$points_in_level_set

    num_vertices_in_this_level <- clustering_result$num_vertices
    level_external_indices <- clustering_result$external_indices
    level_internal_indices <- clustering_result$internal_indices

    # Begin vertex construction
    if (num_vertices_in_this_level > 0) { # check admissibility condition
      # add the number of vertices in the current level set to the vertex index
      vertices_in_level_set[[lsfi]] <- vertex_index + (1:num_vertices_in_this_level)
      for (j in 1:num_vertices_in_this_level) {
        vertex_index <- vertex_index + 1
        level_of_vertex[vertex_index] <- lsfi # put the current loop count into the corresponding index vertex
        # let all points that satisfy the condition "the number of internal clusters of the current lsfi ==
        # the maximum value of the current vertices" be put into points_in_vertex
        points_in_vertex[[vertex_index]] <- level_external_indices[level_internal_indices == j]
      }
    }
    # note : compute the number of points in each cluster of a single interval,
    # and then loop over the number of intervals
  }

  # Begin simplicial complex
  adja <- simplcial_complex(filter_values, vertex_index, num_levelsets, num_intervals,
                            vertices_in_level_set, points_in_vertex)

  mapperoutput <- list(adjacency = adja,
                       num_vertices = vertex_index,
                       level_of_vertex = level_of_vertex,
                       points_in_vertex = points_in_vertex,
                       points_in_level_set = points_in_level_set,
                       vertices_in_level_set = vertices_in_level_set)

  class(mapperoutput) <- "TDAmapper"
  return(mapperoutput)
}
