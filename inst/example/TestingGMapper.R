library(igraph)
library(networkD3)
library(parallel)
library(foreach)
library(doParallel)
library(tidygraph)
library(ggraph)
library(mclust)
library(nortest)

source('R/EdgeVertices.R')
source('R/ConvertLevelsets.R')
source('R/Cover.R')
source('R/Cluster.R')
source('R/SimplicialComplex.R')
source('R/GMapper.R')
source('R/Plotter.R')
source('inst/example/ExampleData.R')

data <- get(data("iris"))

pca_result <- prcomp(data[, 1:4], scale. = TRUE)
filter_pca1 <- pca_result$x[, 1]

time_taken <- system.time({
  GMapper <- GMapperAlgo(
    data[,1:4],
    # filter_values = filter_pca1,
    filter_values = data[,1],
    AD_threshold = 0.8,
    g_overlap = 0.5,
    methods = "kmeans",
    method_params = list(max_kmeans_clusters = 2),
    num_cores = 12
  )
})
time_taken
MapperPlotter(GMapper, label=data$Species, original_data=data, avg=FALSE, use_embedding=FALSE)

# This is an example for using is_node_attribute=TRUE
g <- graph_from_adjacency_matrix(Mapper$adjacency, mode = "undirected")
e_result <- eigen_centrality(g)
MapperPlotter(Mapper, label=e_result$vector, original_data=data, avg=FALSE, use_embedding=TRUE)

## Save mapper
library(jsonlite)

export_data <- list(
  adjacency = Mapper$adjacency,
  num_vertices = Mapper$num_vertices,
  level_of_vertex = Mapper$level_of_vertex,
  points_in_vertex = Mapper$points_in_vertex,
  original_data = data
)

write(toJSON(export_data, auto_unbox = TRUE), "~/desktop/data.json")
