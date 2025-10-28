#' Visualizes the correlation between two Mapper colorings.
#'
#' @param Mapper Mapper object.
#' @param data Data.
#' @param labels List of two Mapper color.
#' @param use_embedding List of two booleans indicating whether to use original data or embedding data.
#' @return Plot of the correlation between two Mapper.
#' @export
MapperCorrelation <- function(
    Mapper, data, labels = list(), use_embedding = list(FALSE, FALSE)
) {
  graph1 <- MapperPlotter(Mapper, label=labels[[1]], data=data, type="ggraph", avg=TRUE, use_embedding=use_embedding[[1]])
  graph2 <- MapperPlotter(Mapper, label=labels[[2]], data=data, type="ggraph", avg=TRUE, use_embedding=use_embedding[[2]])

  x <- graph1$data$AvgLabel
  y <- graph2$data$AvgLabel

  cc <- cor(x, y, method = "pearson", use = "complete.obs")

  df <- data.frame(x=x, y=y)
  # plot
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
