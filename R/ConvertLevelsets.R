#' Convert level set flat index (lsfi) to multi-index (lsmi)
#'
#' @param lsfi Level set flat index.
#' @param num_intervals Number of intervals.
#' @return A multi-index corresponding to the flat index.
#' @export
to_lsmi <- function(lsfi, num_intervals) {
  csizes <- cumprod(c(1, num_intervals))[seq_along(num_intervals)]
  ((lsfi - 1) %/% csizes) %% num_intervals + 1
}

#' Convert level set multi-index (lsmi) to flat index (lsfi)
#'
#' @param lsmi Level set multi-index.
#' @param num_intervals Number of intervals.
#' @return A flat index corresponding to the multi-index.
#' @export
to_lsfi <- function(lsmi, num_intervals) {
  csizes <- cumprod(c(1, num_intervals))[seq_along(lsmi)]
  sum((lsmi - 1) * csizes) + 1
}
