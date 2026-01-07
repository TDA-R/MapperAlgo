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
circle_data <- reader(dataset_name = 'circle')
# mnist <- reader(dataset_name = 'mnist')

time_taken <- system.time({
  Mapper <- MapperAlgo(
    data[,1:4],
    filter_values = data[,1:3],
    # filter_values = circle_data[,2:2],
    # filter_values = mnist[,1:2],
    percent_overlap = 30,
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

# MapperPlotter(Mapper, label=mnist$label, original_data=mnist, avg=FALSE, use_embedding=FALSE)

# This is an example for using is_node_attribute=TRUE
g <- graph_from_adjacency_matrix(Mapper$adjacency, mode = "undirected")
e_result <- eigen_centrality(g)
MapperPlotter(Mapper, label=e_result$vector, original_data=data, avg=FALSE, use_embedding=TRUE)


length(Mapper$points_in_level_set)
unique_indexes <- unique(unlist(Mapper$points_in_vertex))
unique_indexes%>%length()
unique_levelset <- unique(unlist(Mapper$points_in_level_set))
unique_levelset%>%length()

setdiff(1:150, unique_levelset)
data[,1:4]%>%nrow()

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
MapperCorrelation(Mapper, original_data = data, labels = list(data$Sepal.Length, data$Sepal.Width))

source('R/CPEmbedding.R')
data$PW_group <- ifelse(data$Sepal.Width > 1.5, "wide", "narrow")
embedded <- CPEmbedding(Mapper, data, columns = list("PW_group", "Species"), a_level = "wide", b_level = "versicolor")
MapperCorrelation(Mapper, original_data = data, labels = list(data$Sepal.Length, embedded), use_embedding = list(FALSE, TRUE))


MapperPlotter(Mapper, label=data$Species, data=data, type="forceNetwork", avg=FALSE, use_embedding=FALSE)
MapperPlotter(Mapper, label=embedded, data=data, type="forceNetwork", avg=TRUE, use_embedding=TRUE)
# MapperPlotter(Mapper, label=data$Species, data=data, type="forceNetwork", avg=FALSE)

## Save mapper
library(jsonlite)

export_data <- list(
  adjacency = Mapper$adjacency,
  num_vertices = Mapper$num_vertices,
  level_of_vertex = Mapper$level_of_vertex,
  points_in_vertex = Mapper$points_in_vertex,
  original_data = as.matrix(mnist$label),
)

write(toJSON(export_data, auto_unbox = TRUE), "~/desktop/mnist.json")
