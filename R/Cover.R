#' Cover points based on intervals and overlap
#'
#' @param lsfi Level set flat index.
#' @param filter_min Minimum filter value.
#' @param interval_width Width of the interval.
#' @param percent_overlap Percentage overlap between intervals.
#' @param filter_values The filter values to be analyzed.
#' @param num_intervals Number of intervals.
#' @param type Type of interval, either 'stride' or 'extension'.
#' @return Indices of points in the range.
#' @export
cover_points <- function(
    lsfi, filter_min, interval_width, percent_overlap,
    filter_values, num_intervals, type='stride'
    ) {
  # level set flat index (lsfi), which is a number, has a corresponding
  # level set multi index (lsmi), which is a vector
  lsmi <- to_lsmi(lsfi, num_intervals)

  # set the range of the interval
  if (type == 'stride') {
    # This is the original code in paper, but not performing well
    stride <- interval_width * (1 - percent_overlap / 100)
    anchor <- filter_min + (lsmi - 1) * stride
    lsfmin <- anchor
    lsfmax <- anchor + interval_width
  } else if (type == 'extension') {
    # the anchor is the leftmost point of the interval, center point is anchor + 0.5 * interval_width
    anchor <- filter_min + (lsmi - 1) * interval_width
    extension <- 0.5 * interval_width * percent_overlap / 100
    lsfmin <- anchor - extension
    lsfmax <- anchor + interval_width + extension
  }

  # compute whether each point is in the range
  in_range <- apply(filter_values, 1, function(x) all(lsfmin <= x & x <= lsfmax))
  # return the indices of the points that are in the range
  return(which(in_range))
}

#' Helper function to recursively split data until it is Gaussian
#' The function now takes geometric boundaries (a, b) instead of indices
#'
#' @param a Left boundary of the interval
#' @param b Right boundary of the interval
#' @param vals The original filter values (1D vector)
#' @param AD_threshold The threshold for the Anderson-Darling test to determine Gaussian
#' @param g_overlap The geometric overlap percentage when splitting an interval
#' @param depth Current depth of recursion to prevent infinite loops
#' @return A list of geometric intervals that are Gaussian
#' @export
recursive_gaussian_split <- function(
    a, b, vals, AD_threshold, g_overlap, depth = 1
    ) {

  # Find data points that fall into the current interval [a, b]
  indices <- which(vals >= a & vals <= b)

  # Too few points for an AD test, zero variance, or max depth reached
  if (length(indices) < 10 || (b - a) < 1e-6) {
    return(list(c(a, b)))
  }

  data_subset <- vals[indices]

  if (var(data_subset) < 1e-10) {
    return(list(c(a, b)))
  }

  # Anderson-Darling Test
  # Safely get the 'statistic', defaulting to 0 on error
  ad_result <- tryCatch({
    nortest::ad.test(data_subset)
  }, error = function(e) list(statistic = 0))

  # Check AD Statistic against the threshold
  # Checks if the statistic is smaller than the threshold
  if (ad_result$statistic < AD_threshold) {
    # Stop splitting if it's Gaussian
    return(list(c(a, b)))
  } else {

    c_mean <- mean(data_subset)
    lambda <- var(data_subset)
    init_centers <- c(c_mean - sqrt(2*lambda/pi), c_mean + sqrt(2*lambda/pi))

    assignments <- ifelse(abs(data_subset - init_centers[1]) <
                            abs(data_subset - init_centers[2]), 1, 2)

    init_params <- list(
      mean = init_centers,
      variance = list(
        modelName = "V",
        sigmasq = c(var(data_subset[assignments == 1]),
                    var(data_subset[assignments == 2]))
      ),
      pro = c(mean(assignments == 1), mean(assignments == 2))
    )

    # Split into 2 using GMM if it's not Gaussian
    gmm_model <- suppressWarnings(
      mclust::Mclust(
        data_subset,
        G = 2,
        verbose = FALSE,
        initialization = list(parameters = init_params)
      )
    )

    # Stop splitting if GMM fails to find 2 distinct components
    if (is.null(gmm_model) || gmm_model$G < 2) {
      return(list(c(a, b)))
    }

    # Extract means and variances to apply the paper's formula
    m1 <- gmm_model$parameters$mean[1]
    m2 <- gmm_model$parameters$mean[2]

    if (gmm_model$modelName == "E") {  # Equal variance
      s1 <- s2 <- sqrt(gmm_model$parameters$variance$sigmasq)
    } else {  # Variable variance (V model)
      vars <- gmm_model$parameters$variance$sigmasq
      s1 <- sqrt(vars[1])
      s2 <- sqrt(vars[2])
    }

    # Ensure m1 is the smaller mean for the geometric calculations
    if (m1 > m2) {
      tmp_m <- m1; m1 <- m2; m2 <- tmp_m
      tmp_s <- s1; s1 <- s2; s2 <- tmp_s
    }

    # Exact geometric boundary formulas from the paper
    dist_m <- m2 - m1
    ratio_1 <- s1 / (s1 + s2)
    ratio_2 <- s2 / (s1 + s2)

    # Left interval right boundary: min{m1 + (1 + g_overlap)*ratio_1*(m2 - m1), m2}
    right_bound <- min(m1 + (1 + g_overlap) * ratio_1 * dist_m, m2)

    # Right interval left boundary: max{m2 - (1 + g_overlap)*ratio_2*(m2 - m1), m1}
    left_bound <- max(m2 - (1 + g_overlap) * ratio_2 * dist_m, m1)

    # Stop splitting if calculated boundaries are invalid (prevents infinite loops)
    if (right_bound >= b || left_bound <= a) {
      return(list(c(a, b)))
    }

    # Recursively test the two new geometric subsets
    cover_1 <- recursive_gaussian_split(a, right_bound, vals, AD_threshold, g_overlap, depth + 1)
    cover_2 <- recursive_gaussian_split(left_bound, b, vals, AD_threshold, g_overlap, depth + 1)

    return(c(cover_1, cover_2))
  }
}
