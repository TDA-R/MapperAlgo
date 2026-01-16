<<<<<<< HEAD
#' Fuzzy Mapper Algorithm (Fixed Adjacency Calculation)
#'
#' @param original_data Original dataframe, not the filter values.
#' @param filter_values A data frame or matrix of the data to be analyzed.
#' @param cluster_n Number of fuzzy clusters (c in FCM). Default is 5.
#' @param fcm_threshold Membership threshold (tau). Points with u > tau are included in the interval.
#' @param methods Specify the clustering method to be used, e.g., "hclust" or "kmeans".
#' @param method_params A list of parameters for the clustering method.
#' @param num_cores Number of cores to use for parallel computing.
#' @return A MapperAlgo object same as MapperAlgo output
#' @importFrom ppclust fcm
#' @importFrom inaparc kmpp
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel stopCluster
#' @export
FuzzyMapperAlgo <- function(
    original_data,
    filter_values,
    cluster_n = 5,
    fcm_threshold = NULL,
    methods,
    method_params = list(),
    num_cores = 1
) {
  original_data <- as.data.frame(original_data)

  if (is.null(fcm_threshold)) {
    fcm_threshold <- min(0.1, 1 / cluster_n)
    message(paste("Auto-setting fcm_threshold to:", round(fcm_threshold, 4)))
  }

  data_matrix <- as.matrix(filter_values)

  v0 <- inaparc::kmpp(as.matrix(filter_values), k = cluster_n)$v

  res.fcm <- ppclust::fcm(as.matrix(filter_values), centers = v0)
  U <- res.fcm$u
  num_levelsets <- ncol(U)
  level_sets_indices <- list()
  for (j in 1:num_levelsets) {
    level_sets_indices[[j]] <- which(U[, j] > fcm_threshold)
  }
  print(level_sets_indices)

  vertex_index <- 0
  level_of_vertex <- c()
  points_in_vertex <- list()
  points_in_level_set <- vector("list", num_levelsets)
  vertices_in_level_set <- vector("list", num_levelsets)

  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  results <- foreach(lsfi = 1:num_levelsets,
                     .packages = c("cluster"),
                     .export = c("perform_clustering", "cluster_cutoff_at_first_empty_bin"
                     )) %dopar% {

                                   points_in_level_set <- level_sets_indices[[lsfi]]

                                   if (length(points_in_level_set) == 0) {
                                     return(list(clustering_result = list(num_vertices=0), points_in_level_set = integer(0)))
                                   }

                                   clustering_result <- perform_clustering(
                                     original_data,
                                     filter_values,
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

  # Mapper construction here is different from the original MapperAlgo
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

  if (sum(adja) == 0 && num_vertices > 1) {
    warning("No edges were created in the Mapper graph. Consider adjusting the clustering parameters or filter function.")
  }

  mapperoutput <- list(adjacency = adja,
                       num_vertices = num_vertices,
                       level_of_vertex = level_of_vertex,
                       points_in_vertex = points_in_vertex,
                       points_in_level_set = points_in_level_set,
                       vertices_in_level_set = vertices_in_level_set)

  class(mapperoutput) <- "TDAmapper"
  return(mapperoutput)

}


=======
library(ggplot2)
library(igraph)
library(networkD3)
library(parallel)
library(foreach)
library(doParallel)
library(htmlwidgets)
library(webshot)
library(tidygraph)
library(ggraph)

source('R/EdgeVertices.R')
source('R/ConvertLevelsets.R')
source('R/Cover.R')
source('R/Cluster.R')
source('R/SimplicialComplex.R')
source('R/MapperAlgo.R')
source('R/Plotter.R')
source('inst/example/ExampleData.R')

data <- get(data("iris"))
library(ppclust)
library(factoextra)
library(cluster)
library(fclust)

x <- iris[,-5]
res.fcm <- fcm(x, centers=3)
v0 <- inaparc::kmpp(x, k=3)$v
u0 <- inaparc::imembrand(nrow(x), k=3)$u
# res.fcm <- fcm(x, centers=v0)
res.fcm <- fcm(x, centers=3, memberships=u0)

summary(res.fcm)

res.fcm2 <- ppclust2(res.fcm, "kmeans")
factoextra::fviz_cluster(res.fcm2, data = x,
                         ellipse.type = "convex",
                         palette = "jco",
                         repel = TRUE)


res.fcm$u


create_fuzzy_mapper_graph <- function(
    fcm_result, original_data = NULL, threshold = 0.5
    ) {

  U <- fcm_result$u
  num_clusters <- ncol(U)

  adj_mat <- matrix(0, nrow = num_clusters, ncol = num_clusters)

  for (i in 1:(num_clusters - 1)) {
    for (j in (i + 1):num_clusters) {
      print(paste(i,j))
      intersection_strength <- sum(pmin(U[, i], U[, j]))

      adj_mat[i, j] <- intersection_strength
      adj_mat[j, i] <- intersection_strength
    }
  }
  print(adj_mat)

  adj_mat[adj_mat < threshold] <- 0

  g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected", weighted = TRUE)

  cluster_sizes <- colSums(U)
  V(g)$size <- cluster_sizes
  V(g)$name <- paste0("Cluster ", 1:num_clusters)

  return(g)
}
g <- create_fuzzy_mapper_graph(res.fcm, original_data = x, threshold = 5)

ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.4, color = "gray50") +
  geom_node_point(aes(size = size)) +
  geom_node_text(aes(label = name), repel = TRUE, fontface = "bold") +
  scale_size_continuous(range = c(5, 15)) +
  # scale_edge_width(range = c(0.5, 3)) +
  # scale_color_viridis_c(option = "magma", name = "Avg Feature 1") +
  theme_graph()
>>>>>>> a907489bc8f76e5760196d957384c60db6368725
