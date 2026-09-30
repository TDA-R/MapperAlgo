param_condition <- function(methods, method_params, method_mlr, has_method_params) {

  has_methods <- !is.null(methods)
  has_method <- !is.null(method_mlr)

  if (has_method && (has_methods || has_method_params)) {
    stop("When `method_mlr` is supplied, `methods` and `method_params` must not be used.")
  }
  if (has_methods && !has_method_params) {
    stop("`methods` must be used together with `method_params`.")
  }
  if (has_method_params && !has_methods) {
    stop("`method_params` must be used together with `methods`.")
  }
  if (!has_method && !has_methods) {
    stop("You must specify a clustering method via `methods` + `method_params`, or via `method_mlr`.")
  }
  if (has_methods && !is.list(method_params)) {
    stop("`method_params` must be a list.")
  }
  if (has_method && !inherits(method_mlr, "Learner")) {
    stop("`method_mlr` must be an mlr3cluster Learner, e.g. mlr3cluster::lrn(\"clust.kmeans\", centers = 3).")
  }

  return(has_method)
}

#' Cluster every level set (in parallel)
#'
#' Shared by MapperAlgo, GMapperAlgo and FuzzyMapperAlgo.
#'
#' @param level_sets A list; each element is the vector of point indices in one level set.
#' @param original_data Original dataframe.
#' @param filter_values The filter values.
#' @param methods,method_params Clustering method and its parameters (old interface).
#' @param method_mlr An mlr3cluster Learner (new interface).
#' @param using_new_method Logical, TRUE to use `method_mlr`.
#' @param num_cores Number of cores.
#' @return A list of clustering results, one per level set.
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @noRd
cluster_level_sets <- function(
    level_sets, original_data, filter_values, methods, method_params, method_mlr, using_new_method, num_cores = 1
    ) {

  cl <- makeCluster(num_cores)
  on.exit(stopCluster(cl), add = TRUE)
  registerDoParallel(cl)

  lsfi <- NULL

  foreach(lsfi = seq_along(level_sets),
          .packages = if (using_new_method) c("mlr3", "mlr3cluster") else c("cluster"),
          .export = if (using_new_method) {
            c("perform_clustering_mlr3")
          } else {
            c("perform_clustering", "cluster_cutoff_at_first_empty_bin", "find_best_k_for_kmeans")
          }) %dopar% {

            points_in_level_set <- level_sets[[lsfi]]

            if (length(points_in_level_set) == 0) {
              list(num_vertices = 0, external_indices = NULL, internal_indices = NULL)
            } else if (using_new_method) {
              perform_clustering_mlr3(original_data, filter_values, points_in_level_set, method_mlr)
            } else {
              perform_clustering(original_data, filter_values, points_in_level_set, methods, method_params)
            }
          }
}

#' Turn per-level-set clustering results into Mapper vertices
#'
#' @param clustering_results Output of `cluster_level_sets()`.
#' @return A list with `num_vertices`, `level_of_vertex`, `points_in_vertex` and `vertices_in_level_set`.
#' @noRd
build_vertices <- function(clustering_results) {
  num_levelsets <- length(clustering_results)
  vertex_index <- 0
  level_of_vertex <- c()
  points_in_vertex <- list()
  vertices_in_level_set <- vector("list", num_levelsets)

  for (lsfi in seq_len(num_levelsets)) {
    res <- clustering_results[[lsfi]]
    n_v <- res$num_vertices

    # Begin vertex construction
    if (n_v > 0) { # admissibility condition
      # add the number of vertices in the current level set to the vertex index
      vertices_in_level_set[[lsfi]] <- vertex_index + (1:n_v)
      for (j in 1:n_v) {
        vertex_index <- vertex_index + 1
        level_of_vertex[vertex_index] <- lsfi # put the current loop count into the corresponding index vertex
        # let all points that satisfy the condition "the number of internal clusters of the current lsfi equal to
        # the maximum value of the current vertices" be put into points_in_vertex
        points_in_vertex[[vertex_index]] <- res$external_indices[res$internal_indices == j]
      }
    }
    # note : compute the number of points in each cluster of a single interval and then loop over the number of intervals
  }

  list(num_vertices = vertex_index,
       level_of_vertex = level_of_vertex,
       points_in_vertex = points_in_vertex,
       vertices_in_level_set = vertices_in_level_set)
}

#' Construct adjacency matrix
#'
#' @param filter_values A matrix of filter values.
#' @param vertex_index The number of vertices.
#' @param num_levelsets The total number of level sets.
#' @param num_intervals A vector representing the number of intervals for each filter.
#' @param vertices_in_level_set A list where each element contains the vertices corresponding to each level set.
#' @param points_in_vertex A list where each element contains the points corresponding to each vertex.
#' @return An adjacency matrix
#' @export
mapper_adjacency <- function(
    filter_values, vertex_index, num_levelsets, num_intervals, vertices_in_level_set, points_in_vertex
) {
  filter_output_dim <- dim(filter_values)[2] # columns
  # create empty adjacency matrix to store the connections between vertices
  adja <- mat.or.vec(vertex_index, vertex_index)
  for (lsfi in 1:num_levelsets) {

    lsmi <- to_lsmi(lsfi, num_intervals)
    # Find adjacent level sets +1 of each entry in lsmi (within bounds of num_intervals)
    # Need to_lsfi to do this easily.
    for (k in 1:filter_output_dim) {
      # check admissibility condition
      if (lsmi[k] >= num_intervals[k]) { next }
      lsmi_adjacent <- lsmi + diag(filter_output_dim)[, k]
      lsfi_adjacent <- to_lsfi(lsmi_adjacent, num_intervals)

      v1_set <- vertices_in_level_set[[lsfi]]
      v2_set <- vertices_in_level_set[[lsfi_adjacent]]

      if (length(v1_set) < 1 | length(v2_set) < 1) { next }
      # construct adjacency matrix
      for (v1 in v1_set) {
        for (v2 in v2_set) {
          adja[v1, v2] <- (length(intersect(
            points_in_vertex[[v1]], points_in_vertex[[v2]])) > 0)

          adja[v2, v1] <- adja[v1,v2]
        }
      }
    }
  }
  return(adja)
}

#' Adjacency matrix from shared points between vertices of different level sets
#'
#' Used by GMapperAlgo and FuzzyMapperAlgo (MapperAlgo uses `simplcial_complex()`).
#'
#' @param points_in_vertex List of point indices per vertex.
#' @param level_of_vertex Level set index of each vertex.
#' @return A symmetric 0/1 adjacency matrix.
#' @noRd
overlap_adjacency <- function(points_in_vertex, level_of_vertex) {
  num_vertices <- length(points_in_vertex)
  adja <- matrix(0, nrow = num_vertices, ncol = num_vertices)

  if (num_vertices > 1) {
    for (i in 1:(num_vertices - 1)) {
      pts_i <- points_in_vertex[[i]]
      level_i <- level_of_vertex[i]
      for (j in (i + 1):num_vertices) {
        if (level_i != level_of_vertex[j] &&
            length(intersect(pts_i, points_in_vertex[[j]])) > 0) {
          adja[i, j] <- 1
          adja[j, i] <- 1
        }
      }
    }
    if (sum(adja) == 0) {
      warning("No edges were created in the Mapper graph. Consider adjusting the clustering parameters or filter function.")
    }
  }
  adja
}

#' Assemble the Mapper output object
#'
#' @param adjacency Adjacency matrix.
#' @param vertices Output of `build_vertices()`.
#' @param points_in_level_set List of point indices per level set.
#' @param input_params List of the input parameters to store.
#' @param class_name Class of the returned object.
#' @noRd
new_mapper_output <- function(adjacency, vertices, points_in_level_set, input_params, class_name) {

  out <- list(adjacency = adjacency,
              num_vertices = vertices$num_vertices,
              level_of_vertex = vertices$level_of_vertex,
              points_in_vertex = vertices$points_in_vertex,
              points_in_level_set = points_in_level_set,
              vertices_in_level_set = vertices$vertices_in_level_set,
              input_params = input_params)

  class(out) <- class_name
  out
}
