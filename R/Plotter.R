#' Plot Mapper Result
#'
#' Visualizes the Mapper output using either networkD3 or ggraph.
#' 
#' @param Mapper Mapper object.
#' @param label Label of the data.
#' @param data Data.
#' @param type Visualization type: "forceNetwork" or "ggraph".
#' @return Plot of the Mapper.
#' @importFrom igraph graph.adjacency V
#' @importFrom networkD3 forceNetwork
#' @importFrom htmlwidgets JS
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text
#' @importFrom tidygraph tbl_graph
#' @importFrom ggplot2 aes labs theme_void
#' @export
MapperPlotter <- function(
    Mapper, label, data, type="forceNetwork"
) {
  
  if (type == "forceNetwork") {
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
  else if (type == "ggraph") {
    
    # create node data frame
    node_df <- data.frame(
      id = 1:Mapper$num_vertices,
      level = Mapper$level_of_vertex,
      size = sapply(Mapper$points_in_vertex, length)
    )
    
    # create edge data frame
    adj <- Mapper$adjacency
    edge_df <- which(adj == 1, arr.ind = TRUE)
    edge_df <- edge_df[edge_df[,1] < edge_df[,2], ]  # avoid self-loops
    edges <- data.frame(
      from = edge_df[,1],
      to = edge_df[,2]
    )
    
    # to tidygraph
    graph <- tbl_graph(nodes = node_df, edges = edges, directed = FALSE)
    
    ggraph(graph, layout = "fr") +  # Fruchterman-Reingold layout
      geom_edge_link(color = "gray") +
      geom_node_point(aes(size = size, color = factor(level))) +
      geom_node_text(aes(label = id), repel = TRUE, size = 3) +
      theme_void() +
      labs(color = "Level", size = "Points in Cluster")
  }
}