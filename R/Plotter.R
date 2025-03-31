#' @param Mapper Mapper object.
#' @param label Label of the data.
#' @param data Data.
#' @return Plot of the Mapper.
#' @importFrom igraph networkD3
#' @export
MapperPlotter <- function(
    Mapper, label, data
) {
  
  Graph <- graph.adjacency(Mapper$adjacency, mode="undirected")
  l = length(V(Graph))
  var.maj.vertex <- c() # vertex majority label
  filter.vertex <- c() # vertex size
  circle_groups <- as.character(label)
  vertex.size <- rep(0,l)
  
  for (i in 1:l){
    # points in each vertex
    points.in.vertex <- Mapper$points_in_vertex[[i]]
    # find the most common label in the vertex
    ux <- unique(circle_groups[points.in.vertex])
    Mode.in.vertex <- ux[which.max(tabulate(match(circle_groups[points.in.vertex], ux)))]
    var.maj.vertex <- c(var.maj.vertex,as.character(Mode.in.vertex))
    # vertex size = number of points in the vertex
    vertex.size[i] <- length((Mapper$points_in_vertex[[i]]))
  }
  
  # Add information to the nodes
  MapperNodes <- mapperVertices(Mapper, 1:nrow(data))
  MapperNodes$Group <- as.factor(var.maj.vertex)
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
}
