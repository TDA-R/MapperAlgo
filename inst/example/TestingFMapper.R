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
source('R/FMapper.R')
source('inst/example/ExampleData.R')

data <- get(data("iris"))
library(ppclust)
library(factoextra)
library(cluster)
library(fclust)

FMapper <- FuzzyMapperAlgo(
  original_data = data[,1:4],
  filter_values =  data[,1:2],
  cluster_n = 8,
  fcm_threshold = 0.2,
  # methods = "hierarchical",
  # method_params = list(num_bins_when_clustering = 1, method = 'ward.D2'),
  methods = "kmeans",
  method_params = list(max_kmeans_clusters = 2)
)

MapperPlotter(FMapper, label=data$Species, original_data=data, avg=FALSE, use_embedding=FALSE)
