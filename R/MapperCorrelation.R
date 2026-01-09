#' Visualizes the correlation between two Mapper colorings.
#'
#' @param mapper A Mapper object created by the `MapperAlgo` function.
#' @param original_data Original dataframe, not the filter values.
#' @param labels List of two Mapper color.
#' @param use_embedding List of two booleans indicating whether to use original data or embedding data.
#' @return Plot of the correlation between two Mapper.
#' @importFrom stats cor
#' @importFrom ggplot2 ggplot geom_point geom_smooth theme_minimal labs aes
#' @export
MapperCorrelation <- function(
    mapper, original_data, labels = list(), use_embedding = list(FALSE, FALSE)
) {

  get_node_values <- function(m, lbl, embed) {
    if (embed) {
      return(lbl)
    }
    else {
      piv <- m$points_in_vertex
      return(vapply(piv, function(idx) mean(lbl[idx], na.rm = TRUE), numeric(1)))
    }
  }

  x <- get_node_values(mapper, labels[[1]], use_embedding[[1]])
  y <- get_node_values(mapper, labels[[2]], use_embedding[[2]])

  cc <- cor(x, y, method = "pearson", use = "complete.obs")

  df <- data.frame(x=x, y=y)
  plt <- ggplot(data = df, aes(x, y)) +
    geom_point(color='#447356') +
    geom_smooth(method = "lm", se = FALSE, color = "#58ad90") +
    labs(
      title = paste("Correlation between two Mapper", round(cc, 3)),
      x = "Avg label 1",
      y = "Avg label 2"
    ) +
    theme_minimal()

  return(plt)
}
