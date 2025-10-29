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
    # if (!avg) legend <- FALSE
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
      is_continuous <- TRUE
    } else {
      colourScale <- htmlwidgets::JS("d3.scaleOrdinal(d3.schemeCategory10)")
      is_continuous <- FALSE
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
      linkDistance = JS("function(d){ return (d.value ? 40 + 8*Math.sqrt(d.value) : 60); }"),
      charge = JS("function(d){ return - (60 + 2*Math.sqrt(d.nodesize)); }"),
      legend = legend
    )
    if (avg && is_continuous) {
      pal <- viridisLite::viridis(100)
      pal_json <- jsonlite::toJSON(pal, auto_unbox = TRUE)
      p <- htmlwidgets::onRender(p, htmlwidgets::JS(sprintf(
        "function(el, x) {
           var colors = %s;
           var minv = %f, maxv = %f;
           var root = d3.select(el);
           var container = root.append('div')
             .attr('class','rd3-colorbar')
             .style('position','absolute')
             .style('right','10px')
             .style('top','10px')
             .style('padding','6px')
             .style('background','rgba(255,255,255,0.95)')
             .style('border','1px solid #ddd')
             .style('border-radius','3px')
             .style('font-family','sans-serif')
             .style('font-size','11px')
             .style('pointer-events','none');

           container.append('div').text('%s').style('margin-bottom','4px').style('font-weight','500');

           // gradient bar
           var grad = container.append('div')
             .style('width','140px')
             .style('height','12px')
             .style('border','1px solid #ccc')
             .style('background', 'linear-gradient(to right,' + colors.join(',') + ')');

           // min / max labels
           var labels = container.append('div').style('display','flex').style('justify-content','space-between').style('margin-top','4px');
           labels.append('div').text(minv);
           labels.append('div').text(maxv);
         }",
        pal_json, rng[1], rng[2], color_title
      )))
    }

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

    set.seed(123)
    p <- ggraph(graph, layout = "fr") +  # Fruchterman-Reingold layout
      geom_edge_link(color = "gray") +
      geom_node_point(aes(size = size, color = .data$Group)) +
      # geom_node_text(aes(label = id), repel = TRUE, size = 3) +
      theme_void() +
      labs(color = 'Group', size = "Points in Cluster")
  }

  return(p)
}
