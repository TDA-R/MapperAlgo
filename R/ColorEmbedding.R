#' Color Embedding
#'
#' Sometimes we use concatenated data to construct MapperAlgo, which filter values doesn't have
#' the original data information. Therefore, this function is useful to connect original data
#' for color labeling
#'
#' @param original_data Original dataframe, not the filter values.
#' @param column The original column name you are interested.
#' @param type 'mean' or 'most_common'
#' @return A dataframe containing the node number, original indexes, and target value for coloring.
#'
#' @export
ColorEmbedding <- function(
    Mapper, original_data, column, type="mean"
) {

  rows <- length(Mapper$level_of_vertex)
  df_for_search <- data.frame()
  target_lst <- list()

  for (i in 1:rows) {

    # Get original index
    original_row_lst <- Mapper$points_in_vertex[[i]]
    df_for_search <- rbind(
      df_for_search,
      data.frame(
        node = i,
        original_indexes = I(list(original_row_lst))
      )
    )

    indexes <- df_for_search[i,]$original_indexes[[1]]

    if (type == "mean") {

      if (!(is.numeric(original_data[[column]]) & !is.integer(original_data[[column]]))) {
        stop("Column must be numeric or integer for 'mean' type")
      }

      col_data <- mean(original_data[indexes,][[column]])
    }

    else if (type == "most_common") {

      if (!(is.factor(original_data[[column]]) & !is.character(original_data[[column]]))) {
        stop("Column must be factor or character for 'most_common' type")
      }

      vals <- original_data[indexes, ][[column]]
      col_data <- names(which.max(table(vals)))
    }

    else {
      stop("type must be 'mean' or 'most_common'")
    }

    target_lst <- append(target_lst, col_data)
  }

  return(unlist(target_lst))
}
