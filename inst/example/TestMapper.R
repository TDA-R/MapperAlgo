library(ggplot2)
library(igraph)
library(networkD3)
library(plotly)
library(parallel)
library(foreach)
library(doParallel)
library(htmlwidgets)
library(webshot)
library(tidygraph)
library(ggraph)
library(mlr3)
library(mlr3cluster)

source('R/EdgeVertices.R')
source('R/ConvertLevelsets.R')
source('R/Cover.R')
source('R/Cluster.R')
source('R/MapperUtils.R')
source('R/MapperAlgo.R')
source('R/Plotter.R')
source('inst/example/ExampleData.R')
source('R/ClusterMlr3.R')

data <- get(data("iris"))
circle_data <- reader(dataset_name = 'circle')
# mnist <- reader(dataset_name = 'mnist')

time_taken <- system.time({
  Mapper <- MapperAlgo(
    data[,1:4],
    filter_values = data[,1:3],
    # filter_values = circle_data[,2:2],
    # filter_values = mnist[,1:2],
    percent_overlap = 40,
    # methods = "dbscan",
    # method_params = list(eps = 1, minPts = 1),
    # methods = "hierarchical",
    # method_params = list(num_bins_when_clustering = 2, method = 'ward.D2'),
    methods = "kmeans",
    method_params = list(max_kmeans_clusters = 2),
    # methods = "pam",
    # method_params = list(num_clusters = 2),
    cover_type = 'stride',
    # intervals = 4,
    interval_width = 1,
    num_cores = 12
  )
})
time_taken

# This is the new version of clustering method, which uses mlr3cluster package.
# It is more flexible but a longer calculation time since it need to package up from mlr3.
time_taken <- system.time({
  Mapper_new <- MapperAlgo(
    data[,1:4],
    filter_values = data[,1:3],
    percent_overlap = 30,
    method_mlr = lrn("clust.kmeans", centers = 2),
    cover_type = 'stride',
    interval_width = 1,
    num_cores = 12
  )
})
time_taken

MapperPlotter(Mapper, label=data$Species, avg=FALSE, use_embedding=FALSE, legend_name="Label")
MapperPlotter(Mapper, label=data$Petal.Length, avg=TRUE, use_embedding=FALSE, legend_name="Label")
MapperPlotter3D(Mapper, label=data$Species, avg=FALSE, use_embedding=FALSE, legend_name="Label")

# use_embedding could be use if the label is a node attribute, instead of the label from the original data. e.g. the eigen centrality value of each node.
g <- graph_from_adjacency_matrix(Mapper$adjacency, mode = "undirected")
e_result <- eigen_centrality(g)
MapperPlotter(Mapper, label=e_result$vector, avg=FALSE, use_embedding=TRUE, legend_name="Eigen Centrality")
MapperPlotter3D(Mapper, label=e_result$vector, avg=FALSE, use_embedding=TRUE, legend_name="Eigen Centrality")


length(Mapper$points_in_level_set)
unique_indexes <- unique(unlist(Mapper$points_in_vertex))
unique_indexes%>%length()
unique_levelset <- unique(unlist(Mapper$points_in_level_set))
unique_levelset%>%length()

source('R/GridSearch.R')
# Without embedding
GridSearch(
  original_data = data[,1:4],
  filter_values = data[,1:2],
  label = data$Species,
  cover_type = "stride",
  width_vec = c(1.0, 1.5),
  overlap_vec = c(10, 20, 30, 40),
  num_cores = 12,
  out_dir = "../mapper_grid_outputs",
)

# With embedding
cpe_params <- list("PW_group", "Species", "wide", "versicolor")
data$PW_group <- ifelse(data$Sepal.Width > 1.5, "wide", "narrow")
labels <- data%>%select(PW_group, Species)
GridSearch(
  filter_values = data[,1:4],
  label = labels,
  column = "Species",
  cover_type = "stride",
  width_vec = c(1),
  overlap_vec = c(30),
  num_cores = 12,
  out_dir = "../mapper_grid_outputs",
  avg = TRUE,
  use_embedding = cpe_params
)

source('R/MapperCorrelation.R')
MapperCorrelation(Mapper, labels = list(data$Sepal.Length, data$Sepal.Width))

# Use this when you're interest in the conditional probability value in each node, use CPEmbedding. e.g. the probability of being a certain species given that the sepal width is wide.
source('R/CPEmbedding.R')
data$PW_group <- ifelse(data$Sepal.Width > 1.5, "wide", "narrow")
embedded <- CPEmbedding(Mapper, data, columns = list("PW_group", "Species"), a_level = "wide", b_level = "versicolor")
# MapperCorrelation(Mapper, labels = list(data$Sepal.Length, embedded), use_embedding = list(FALSE, TRUE))
MapperPlotter(Mapper, label=embedded, avg=FALSE, use_embedding=TRUE, legend_name="CPEmbedding")


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
