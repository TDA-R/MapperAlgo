library(networkD3)
library(igraph)
library(ggplot2)
source('R/EdgeVertices.R')
source('R/ConvertLevelsets.R')
source('R/Cover.R')
source('R/Cluster.R')
source('R/SimplicialComplex.R')
source('R/MapperAlgo.R')

# data("iris")

# Toy dataset testing
make_noisy_circle <- function(radius, num_points, noise_sd = 0.05) {
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

time_taken <- system.time({
  Mapper <- MapperAlgo(
    # filter_values = iris[,1:4],
    filter_values = circle_data[,2:3],
    intervals = 4,
    percent_overlap = 50, 
    # methods = "dbscan",
    # method_params = list(eps = 0.5, minPts = 5)
    methods = "hierarchical",
    method_params = list(num_bins_when_clustering = 10, method = 'ward.D2')
    # methods = "kmeans",
    # method_params = list(max_kmeans_clusters = 2)
    # methods = "pam",
    # method_params = list(num_clusters = 2)
    )
})
time_taken

Graph <- graph.adjacency(Mapper$adjacency, mode="undirected")
l = length(V(Graph))
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Distribution of specific variable in each vertex Majority vote
var.maj.vertex <- c()
filter.vertex <- c()
circle_groups <- as.character(circle_data$circle)

for (i in 1:l){
  points.in.vertex <- Mapper$points_in_vertex[[i]]
  # Mode.in.vertex <- Mode(iris$Species[points.in.vertex])
  Mode.in.vertex <- Mode(circle_groups[points.in.vertex])
  var.maj.vertex <- c(var.maj.vertex,as.character(Mode.in.vertex))
}

# Add information to the nodes
MapperNodes <- mapperVertices(Mapper, 1:nrow(circle_data))
MapperNodes$Group <- as.factor(var.maj.vertex)

# Size
vertex.size <- rep(0,l)
for (i in 1:l){
  points.in.vertex <- Mapper$points_in_vertex[[i]]
  vertex.size[i] <- length((Mapper$points_in_vertex[[i]]))
}
MapperNodes <- mapperVertices(Mapper, 1:nrow(iris))
MapperNodes$var.maj.vertex <- as.factor(var.maj.vertex)
MapperNodes$Nodesize <- vertex.size

MapperLinks <- mapperEdges(Mapper)
forceNetwork(
  Nodes = MapperNodes, 
  Links = MapperLinks, 
  Source = "Linksource",
  Target = "Linktarget",
  Value = "Linkvalue",
  NodeID = "Nodename",
  Nodesize = "Nodesize",
  Group = "var.maj.vertex",
  opacity = 1, 
  zoom = TRUE,
  radiusCalculation = JS("Math.sqrt(d.nodesize)"),
  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
  linkDistance = 30, 
  charge = -10, 
  legend = TRUE
)

