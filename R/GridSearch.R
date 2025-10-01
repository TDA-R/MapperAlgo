#' GridSearch searched over a list of interval width and overlap,
#' useful for visualizing the convergence of the Mapper.
#'
#' @param filter_values A numeric matrix or data frame of filter values (rows are samples, columns are filter dimensions).
#' @param label A vector of labels for coloring the Mapper nodes.
#' @param cover_type The type of cover to use "stride" or "extension".
#' @param width_vec A vector of interval widths.
#' @param overlap_vec A vector of percent overlaps.
#' @param num_cores Number of cores to use for parallel computing.
#' @param out_dir Directory to save the output.
#' @param avg Whether coloring the nodes by average label or majority label.
#' @return A folder containing the PNG files of the Mapper visualizations.
#' @export
GridSearch <- function(
    filter_values,
    label,
    cover_type = "stride",
    width_vec = c(0.5, 1.0, 1.5),
    overlap_vec = c(10, 20, 30, 40),
    num_cores = 12,
    out_dir = "mapper_grid_outputs",
    avg = FALSE
) {

  dir.create(out_dir, showWarnings = FALSE)

  for (w in width_vec) {

    for (ov in overlap_vec) {

      cat(sprintf("Cover=%s, Width=%.2f, Overlap=%d%%\n", cover_type, w, ov))

      time_taken <- system.time({
        Mapper <- MapperAlgo(
          filter_values = filter_values,
          percent_overlap = ov,
          methods  = "dbscan",
          method_params = list(eps = 0.3, minPts = 1),
          cover_type = cover_type,
          interval_width = w,
          num_cores = num_cores
        )
      })

      wdg <- MapperPlotter(
        Mapper,
        label,
        filter_values,
        type = "forceNetwork",
        avg = avg
      )

      png_file <- file.path(out_dir, sprintf("mapper_%s_w%.2f_ov%02d.png", cover_type, w, ov))
      save_mapper_png(wdg, png_file, vwidth = 1400, vheight = 1000, zoom = 2, delay = 0.7)

      cat("Saved:", png_file, ", Elapsed:", time_taken["elapsed"], "sec\n")
      gc()
    }
  }
}

#' GridSearch searched over a list of interval width and overlap,
#' useful for visualizing the convergence of the Mapper.
#'
#' @param widget The htmlwidget object to be saved as PNG.
#' @param png_path The file path to save the PNG image.
#' @param vwidth The viewport width for the webshot.
#' @param vheight The viewport height for the webshot.
#' @param zoom The zoom factor for the webshot.
#' @param delay The delay in seconds before taking the snapshot. Useful for allowing time for the widget to fully render.
#' @return The snapshot is saved to the specified path.
#' @import htmlwidgets
#' @import webshot2
#' @export
save_mapper_png <- function(
    widget, png_path, vwidth = 1200, vheight = 900, zoom = 2, delay = 0.5
    ) {
  tmp_html <- tempfile(fileext = ".html")
  on.exit(try(unlink(tmp_html), silent = TRUE), add = TRUE)
  saveWidget(widget, tmp_html, selfcontained = TRUE)
  webshot(tmp_html, file = png_path, vwidth = vwidth, vheight = vheight, zoom = zoom, delay = delay)
}
