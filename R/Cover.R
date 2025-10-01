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

