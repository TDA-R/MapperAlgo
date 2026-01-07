#' Plot Mapper Result
#'
#' Visualizes the Mapper output using either networkD3.
#'
#' @param Mapper Mapper object.
#' @param original_data Original dataframe, not the filter values.
#' @param label Label of the data.
#' @param avg Whether coloring the nodes by average label or majority label.
#' @param use_embedding Whether to use original data for coloring (TRUE or FALSE).
#' @return Plot of the Mapper.
#' @importFrom igraph graph.adjacency V
#' @importFrom networkD3 forceNetwork
#' @importFrom htmlwidgets JS
#' @importFrom rlang .data
#' @export

MapperPlotter <- function(
    Mapper, original_data, label, avg=FALSE, use_embedding=FALSE
) {

  piv <- Mapper$points_in_vertex
  num_vertices <- Mapper$num_vertices
  vertex.size <- sapply(piv, length)

  adj_indices <- which(Mapper$adjacency == 1, arr.ind = TRUE)
  adj_indices <- adj_indices[adj_indices[, 1] < adj_indices[, 2], , drop = FALSE]

  if (nrow(adj_indices) > 0) {
    edge_weights <- apply(adj_indices, 1, function(idx) {
      length(intersect(piv[[idx[1]]], piv[[idx[2]]]))
    })
  } else {
    edge_weights <- numeric(0)
  }

  legend <- TRUE
  color_title <- "Label"

  if (use_embedding) {
    Group_col <- label

  } else if (avg) {
    legend <- FALSE
    avg_label <- vapply(piv, function(idx) mean(label[idx], na.rm = TRUE), numeric(1))
    Group_col <- avg_label
    color_title <- "Avg(label)"

  } else {
    lab_chr <- as.character(label)
    majority <- character(num_vertices)
    for (i in seq_len(num_vertices)) {
      pts <- piv[[i]]
      if (length(pts) > 0) {
        ux <- unique(lab_chr[pts])
        majority[i] <- ux[which.max(tabulate(match(lab_chr[pts], ux)))]
      } else {
        majority[i] <- "NA"
      }
    }
    Group_col <- factor(majority)
    color_title <- "Majority label"
  }

  MapperNodes <- data.frame(
    Nodename = 1:num_vertices,
    Nodesize = vertex.size * 5,
    Group = Group_col
  )

  if (nrow(adj_indices) > 0) {
    MapperLinks <- data.frame(
      Linksource = adj_indices[, 1] - 1,
      Linktarget = adj_indices[, 2] - 1,
      Linkvalue = edge_weights
    )
  } else {
    MapperLinks <- data.frame(Linksource=numeric(0), Linktarget=numeric(0), Linkvalue=numeric(0))
  }

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
    linkDistance = JS("function(d){ return 150 / Math.sqrt(d.value + 1); }"),
    charge = JS("function(d){ return - (60 + 2*Math.sqrt(d.nodesize)); }"),
    legend = legend
  )

  if (is_continuous) {
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

           var grad = container.append('div')
             .style('width','140px')
             .style('height','12px')
             .style('border','1px solid #ccc')
             .style('background', 'linear-gradient(to right,' + colors.join(',') + ')');

           var labels = container.append('div').style('display','flex').style('justify-content','space-between').style('margin-top','4px');
           labels.append('div').text(minv.toFixed(2));
           labels.append('div').text(maxv.toFixed(2));
         }",
      pal_json, rng[1], rng[2], color_title
    )))
  }

  return(p)
}
