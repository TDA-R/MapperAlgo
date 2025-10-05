#' Visualizes the correlation between two Mapper colorings.
#'
#' @param Mapper Mapper object.
#' @param data Data.
#' @param labels List of two Mapper color.
#' @return Plot of the correlation between two Mapper.
#' @export
MapperCorrelation <- function(
    Mapper, data, labels = list()
) {
  graph1 <- MapperPlotter(Mapper, label = labels[[1]], data = data, type = "ggraph", avg = TRUE)
  graph2 <- MapperPlotter(Mapper, label = labels[[2]], data = data, type = "ggraph", avg = TRUE)

  x <- graph1$data$AvgLabel
  y <- graph2$data$AvgLabel

  cc <- cor(x, y, method = "pearson", use = "complete.obs")

  df <- data.frame(x=x, y=y)
  # plot
  ggplot(data = df, aes(x, y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "#58ad90") +
    labs(
      title = paste("Correlation between two Mapper", round(cc, 3)),
      x = "Avg label 1",
      y = "Avg label 2"
    ) +
    theme_minimal()
}
