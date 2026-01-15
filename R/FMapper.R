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
