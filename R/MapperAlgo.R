#' Mapper Algorithm
#'
#' Implements the Mapper algorithm for Topological Data Analysis (TDA). 
#' It divides data into intervals, applies clustering within each interval, and constructs a 
#' simplicial complex representing the structure of the data.
#'
#' @param filter_values A data frame or matrix of the data to be analyzed.
#' @param intervals An integer specifying the number of intervals.
#' @param percent_overlap Percentage of overlap between consecutive intervals.
#' @param num_bins_when_clustering Number of bins to use when clustering.
#' @param methods Specify the clustering method to be used, e.g., "hclust" or "kmeans".
#' @param method_params A list of parameters for the clustering method
#' @return A list containing the Mapper graph components:
#' \item{adjacency}{The adjacency matrix of the Mapper graph.}
#' \item{num_vertices}{The number of vertices in the Mapper graph.}
#' \item{level_of_vertex}{A vector specifying the level of each vertex.}
#' \item{points_in_vertex}{A list of the indices of the points in each vertex.}
#' \item{points_in_level_set}{A list of the indices of the points in each level set.}
#' \item{vertices_in_level_set}{A list of the indices of the vertices in each level set.}
#' @export
MapperAlgo <- function(
    filter_values, # dist_df[,1:col]
    intervals, # rep(2, col)
    percent_overlap, # 50
    num_bins_when_clustering, # 10
    methods,
    method_params = list() # params in each clustering method
) {

  filter_values <- data.frame(filter_values)
  num_intervals <- rep(intervals, ncol(filter_values)) # rep(2,4) = (2,2,2,2)
  
  num_points <- dim(filter_values)[1] # row
  num_levelsets <- prod(num_intervals) 
  
  # define some vectors of length k = number of columns
  filter_min <- as.vector(sapply(filter_values, min))
  filter_max <- as.vector(sapply(filter_values, max))
  interval_width <- (filter_max - filter_min) / num_intervals
  
  # initialize variables    
  vertex_index <- 0
  level_of_vertex <- c()
  points_in_vertex <- list()
  points_in_level_set <- vector("list", num_levelsets) 
  # store the data points owned by each individual interval
  vertices_in_level_set <- vector("list", num_levelsets)

  # begin loop through all level sets
  for (lsfi in 1:num_levelsets) {
    if (lsfi %% 100 == 0) {
      message(paste(lsfi, "/", num_levelsets, " processed"))
    }
    # Cover step
    points_in_level_set[[lsfi]] <- cover_points(
      lsfi, filter_min, interval_width, percent_overlap, filter_values, num_intervals
      )
    # Clustering step
    clustering_result <- perform_clustering(
        points_in_level_set[[lsfi]], 
        filter_values, 
        num_bins_when_clustering, 
        methods,
        method_params
      )
    
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