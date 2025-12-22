library(tidyverse)
library(reticulate)
library(keras)

reader <- function(dataset_name='iris') {

  if(dataset_name == 'mnist') {

    data <- NULL
    mnist <- dataset_mnist()

    train_image <- mnist$train$x
    train_label <- mnist$train$y

    train_flat <- array_reshape(train_image, c(nrow(train_image), 784))
    train_df <- data.frame(train_flat) %>% select_if(~ sd(.) > 0)

    pca_result <- prcomp(train_df, center = TRUE, scale. = TRUE)
    data <- data.frame(pca_result$x)
    data$label <- train_label


  } else if (dataset_name == 'circle') {
    # circle
    make_noisy_circle <- function(radius, num_points, noise_sd = 0.1) {
      theta <- runif(num_points, 0, 2 * pi)
      x <- radius * cos(theta) + rnorm(num_points, sd = noise_sd)
      y <- radius * sin(theta) + rnorm(num_points, sd = noise_sd)
      data.frame(x = x, y = y)
    }

    noisy_inner_circle <- make_noisy_circle(radius = 1, num_points = 1000)
    noisy_outer_circle <- make_noisy_circle(radius = 2, num_points = 1000)

    data <- rbind(
      data.frame(circle = "inner", noisy_inner_circle),
      data.frame(circle = "outer",noisy_outer_circle)
    )
  }
  return(data)
}
