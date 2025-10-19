#' Plot Mapper Result
#'
#' Visualizes the Mapper output using either networkD3 or ggraph.
#'
#' @param Mapper Mapper object.
#' @param label Label of the data.
#' @param data Data.
#' @param type Visualization type: "forceNetwork" or "ggraph".
#' @param avg Whether coloring the nodes by average label or majority label.
#' @param use_embedding Whether to use original data for coloring (TRUE or FALSE).
#' @return Plot of the Mapper.
#' @importFrom igraph graph.adjacency V
#' @importFrom networkD3 forceNetwork
#' @importFrom htmlwidgets JS
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text
#' @importFrom tidygraph tbl_graph
#' @importFrom ggplot2 aes labs theme_void
#' @importFrom stats quantile
#' @importFrom rlang .data
#' @export
MapperPlotter <- function(
    Mapper, label, data, type="forceNetwork", avg=FALSE,
    use_embedding=FALSE
) {

  Graph <- graph.adjacency(Mapper$adjacency, mode="undirected")
  l = length(V(Graph))
  piv <- Mapper$points_in_vertex
  nbins <- 5
  vertex.size <- sapply(piv, length)

  if (avg) {
    legend <- FALSE
    avg_label <- vapply(piv, \(idx) mean(label[idx], na.rm = TRUE), numeric(1))
    Group_col <- avg_label
    color_title <- "Avg(label)"
  }else {
    legend <- TRUE
    lab_chr <- as.character(label)
    majority <- character(l)

    for (i in seq_len(l)) {
      pts <- piv[[i]]
      ux <- unique(lab_chr[pts])
      majority[i] <- ux[which.max(tabulate(match(lab_chr[pts], ux)))]
    }
    Group_col <- factor(majority)
    color_title <- "Majority label"
  }
  if (use_embedding) {
    Group_col <- label
    if (!avg) legend <- FALSE
  }

  if (type == "forceNetwork") {

    Graph <- igraph::graph.adjacency(Mapper$adjacency, mode = "undirected")
    MapperNodes <- mapperVertices(Mapper, 1:nrow(data))
    MapperNodes$Group <- Group_col
    MapperNodes$Nodesize <- vertex.size * 5
    if (avg) MapperNodes$AvgLabel <- Group_col
    if (!avg && !use_embedding) MapperNodes$majority <- Group_col

    MapperLinks <- mapperEdges(Mapper)

    if (is.numeric(MapperNodes$Group)) {
      rng <- range(MapperNodes$Group, na.rm = TRUE)
      colourScale <- htmlwidgets::JS(sprintf(
        "d3.scaleSequential(d3.interpolateViridis).domain([%f, %f])",
        rng[1], rng[2]
      ))
    } else {
      colourScale <- htmlwidgets::JS("d3.scaleOrdinal(d3.schemeCategory10)")
    }

    p <- forceNetwork(
      Nodes = MapperNodes,
      Links = MapperLinks,
      Source = "Linksource",
      Target = "Linktarget",
      Value  = "Linkvalue",
      NodeID = "Nodename",
      Nodesize = "Nodesize",
      Group = "Group",
      opacity = 1,
      zoom = TRUE,
      radiusCalculation = JS("Math.sqrt(d.nodesize)"),
      colourScale = colourScale,
      linkDistance = 30,
      charge = -10,
      legend = legend
    )

  }
  else if (type == "ggraph") {

    # create node data frame
    node_df <- data.frame(
      id = seq_len(l),
      level = Mapper$level_of_vertex,
      size = vertex.size,
      Group = Group_col,
      stringsAsFactors = FALSE
    )

    if (use_embedding) {
      node_df$Group <- label
    }

    if (avg) node_df$AvgLabel <- avg_label

    adj <- Mapper$adjacency
    edge_df <- which(adj == 1, arr.ind = TRUE)
    edge_df <- edge_df[edge_df[, 1] < edge_df[, 2], , drop = FALSE]
    edges <- data.frame(from = edge_df[, 1], to = edge_df[, 2])

    graph <- tbl_graph(nodes = node_df, edges = edges, directed = FALSE)

    p <- ggraph(graph, layout = "fr") +  # Fruchterman-Reingold layout
      geom_edge_link(color = "gray") +
      geom_node_point(aes(size = size, color = Group)) +
      # geom_node_text(aes(label = id), repel = TRUE, size = 3) +
      theme_void() +
      labs(color = 'Group', size = "Points in Cluster")
  }

  return(p)
}
