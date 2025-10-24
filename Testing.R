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

data("iris")

# Toy dataset testing
make_noisy_circle <- function(radius, num_points, noise_sd = 0.1) {
  theta <- runif(num_points, 0, 2 * pi)
  x <- radius * cos(theta) + rnorm(num_points, sd = noise_sd)
  y <- radius * sin(theta) + rnorm(num_points, sd = noise_sd)
  data.frame(x = x, y = y)
}

noisy_inner_circle <- make_noisy_circle(radius = 1, num_points = 1000)
noisy_outer_circle <- make_noisy_circle(radius = 2, num_points = 1000)

circle_data <- rbind(
  data.frame(circle = "inner", noisy_inner_circle),
  data.frame(circle = "outer",noisy_outer_circle)
)

ggplot(circle_data)+geom_point(aes(x = x, y = y, color = circle))


data <- iris
ggplot(data)+geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Species))

time_taken <- system.time({
  Mapper <- MapperAlgo(
    filter_values = data[,1:4],
    # filter_values = circle_data[,2:3],
    percent_overlap = 30,
    methods = "dbscan",
    method_params = list(eps = 1, minPts = 1),
    # methods = "hierarchical",
    # method_params = list(num_bins_when_clustering = 10, method = 'ward.D2'),
    # methods = "kmeans",
    # method_params = list(max_kmeans_clusters = 2),
    # methods = "pam",
    # method_params = list(num_clusters = 2),
    cover_type = 'stride',
    # intervals = 4,
    interval_width = 1,
    num_cores = 12
    )
})
time_taken

source('R/GridSearch.R')
GridSearch(
  filter_values = filter_values,
  label = data$Species,
  column = 'Species',
  cover_type = "stride",
  width_vec = c(1.0, 1.5),
  overlap_vec = c(10, 20, 30, 40),
  num_cores = 12,
  out_dir = "../mapper_grid_outputs",
  use_embedding = TRUE,
)

source('R/MapperCorrelation.R')
MapperCorrelation(Mapper, data = circle_data, labels = list(circle_data$x, circle_data$y))

source('R/CPEmbedding.R')

data$PW_group <- ifelse(data$Sepal.Width > 1.5, "wide", "narrow")
embedded <- CPEmbedding(Mapper, data, columns = list("PW_group", "Species"), a_level = "wide", b_level = "versicolor")
embedded

source('R/Plotter.R')
MapperPlotter(Mapper, label=embedded, data=data, type="forceNetwork", avg=TRUE, use_embedding=TRUE)
# MapperPlotter(Mapper, label=data$Species, data=data, type="forceNetwork", avg=FALSE)

source('R/MapperCorrelation.R')
df <- MapperCorrelation(Mapper, data, labels = list(data$Petal.Width, data$Sepal.Width),
                  avg = list(TRUE, TRUE), use_embedding = list(FALSE, FALSE))

