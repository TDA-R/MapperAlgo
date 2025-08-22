library(ggplot2)
source('R/ConvertLevelsets.R')

# This file shows how 'extension' and 'stride' cover works and it's performance, 
# calc_n_stride and calc_n_extension are helper functions to calculate the number of intervals

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
  data.frame(circle = "outer", noisy_outer_circle)
)
library(tidyverse)
max(circle_data$y%>%max(), circle_data$x%>%max())
domain_min <- min(circle_data$y%>%min(), circle_data$x%>%min())
domain_max <- max(circle_data$y%>%max(), circle_data$x%>%max())

r <- 2
interval_width <- rep(1, r)
percent_overlap <- rep(30, r)
filter_min <- rep(domain_min, r)
L <- rep(domain_max - domain_min, r)

calc_n_stride <- function(L, w, p) {
  stride <- w * (1 - p/100)
  ifelse(L <= w, 1L,
         as.integer(ceiling((L - w) / pmax(stride, .Machine$double.eps)) + 1L))
}

calc_n_extension <- function(L, w, p) {
  as.integer(pmax(1, ceiling(L / w - p/100)))
}

num_intervals_stride <- calc_n_stride(L, interval_width, percent_overlap)
num_intervals_ext <- calc_n_extension(L, interval_width, percent_overlap)

# same as cover_points but with different return
get_cell_bounds <- function(
  lsfi, filter_min, interval_width, percent_overlap, 
  num_intervals, type=c("stride","extension")
  ){
  type <- match.arg(type)
  lsmi <- to_lsmi(lsfi, num_intervals)
  if (type == "stride") {
    stride <- interval_width * (1 - percent_overlap / 100)
    anchor <- filter_min + (lsmi - 1) * stride
    lsfmin <- anchor
    lsfmax <- anchor + interval_width
  } else {
    anchor <- filter_min + (lsmi - 1) * interval_width
    ext    <- 0.5 * interval_width * percent_overlap / 100
    lsfmin <- anchor - ext
    lsfmax <- anchor + interval_width + ext
  }
  list(min=lsfmin, max=lsfmax)
}

plot_cover_with_grid <- function(data, type, num_intervals, 
                                 filter_min, interval_width, percent_overlap,
                                 domain_min, domain_max){
  total  <- prod(num_intervals)
  bounds <- lapply(1:total, function(i){
    get_cell_bounds(i, filter_min, interval_width, percent_overlap, num_intervals, type)
  })
  xv <- sort(unique(unlist(lapply(bounds, function(b) c(b$min[1], b$max[1])))))
  yh <- sort(unique(unlist(lapply(bounds, function(b) c(b$min[2], b$max[2])))))

  p <- ggplot(data, aes(x=x, y=y, color=circle)) +
    geom_point(alpha=0.4, size=0.7) +
    coord_equal(xlim = c(domain_min, domain_max), ylim = c(domain_min, domain_max)) +
    theme_minimal() +
    labs(
      title = sprintf("%s cover (w=%g, overlap=%g%%, n=c(%d,%d))",
                      type, interval_width[1], percent_overlap[1], 
                      num_intervals[1], num_intervals[2]),
      x = "x", y = "y"
    ) +
    theme(legend.position="top")
  for (v in xv) p <- p + geom_vline(xintercept=v, color="black", linewidth=0.3)
  for (h in yh) p <- p + geom_hline(yintercept=h, color="black", linewidth=0.3)
  p
}

p_stride <- plot_cover_with_grid(circle_data, "stride", num_intervals_stride, 
                                   filter_min, interval_width, percent_overlap,
                                   domain_min, domain_max)
p_ext <- plot_cover_with_grid(circle_data, "extension", num_intervals_ext, 
                               filter_min, interval_width, percent_overlap,
                               domain_min, domain_max)

mrf <- gridExtra::grid.arrange(p_stride, p_ext, ncol = 2)

# compute coverage for sample points
sample_pts <- rbind(
  data.frame(label="A", x= 0,   y= 0),
  data.frame(label="B", x= 1.0, y= 1.0),
  data.frame(label="C", x= 2.0, y= 0.0),
  data.frame(label="D", x= 0.0, y= 2.0),
  data.frame(label="E", x= 1.99, y= 2.01),
  data.frame(label="F", x=-1.5, y= 1.5)
)

count_cells <- function(type){
  total <- prod(num_intervals)
  apply(sample_pts[,c("x","y")], 1, function(pt){
    sum(sapply(1:total, function(i){
      idx <- cover_points(i, filter_min, interval_width, percent_overlap, 
                          rbind(pt), num_intervals, type=type)
      length(idx) > 0
    }))
  })
}
source('R/Cover.R')
sample_pts$stride_cells <- count_cells("stride")
sample_pts$ext_cells <- count_cells("extension")
sample_pts
