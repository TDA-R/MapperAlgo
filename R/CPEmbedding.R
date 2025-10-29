#' Conditional Probability Embedding for Mapper Nodes
#'
#' The origin Mapper includes mean and majority label embeddings.
#' And this function provides another way to color the Mapper nodes.
#' The function is useful to connect original data for color labeling, especially if you're interested in characteristic attributes.
#'
#' @param mapper A Mapper object created by the `MapperAlgo` function.
#' @param original_data Original dataframe, not the filter values.
#' @param columns Two columns in original_data to compute conditional probability.
#' @param a_level The level (attribute) of column A to condition on. If NULL, the first level is used.
#' @param b_level The level (attribute) of column B for which the conditional probability is computed. If NULL, the first level is used.
#' @return A list of conditional probabilities value for each Mapper node.
#'
#' @export
CPEmbedding <- function(
    mapper, original_data, columns=list(), a_level = NULL, b_level = NULL
) {

  rows <- length(mapper$level_of_vertex)
  df_for_search <- data.frame()
  target_lst <- list()

  for (i in 1:rows) {
    original_row_lst <- mapper$points_in_vertex[[i]]
    df_for_search <- rbind(
      df_for_search,
      data.frame(
        node = i,
        original_indexes = I(list(original_row_lst))
      )
    )

    indexes <- df_for_search[i, ]$original_indexes[[1]]

    if (length(columns) != 2) {
      stop("Columns must be a list of length 2: list(A, B)")
    }

    colA <- columns[[1]]
    colB <- columns[[2]]

    if (!(colA %in% names(original_data)) || !(colB %in% names(original_data))) {
      stop("Specified columns not found in original_data.")
    }

    sub <- original_data[indexes, c(colA, colB), drop = FALSE]
    A <- as.character(sub[[colA]])
    B <- as.character(sub[[colB]])
    if (is.logical(A) || is.character(A)) A <- factor(A)
    if (is.logical(B) || is.character(B)) B <- factor(B)

    A <- droplevels(A)
    B <- droplevels(B)

    # Default levels: if not specified, take the first level of each factor
    a_lv <- if (is.null(a_level)) levels(A)[1] else a_level
    b_lv <- if (is.null(b_level)) levels(B)[1] else b_level

    # Formula: P(B=b_lv | A=a_lv) = count(A=a_lv & B=b_lv) / count(A=a_lv)
    denom <- sum(A == a_lv, na.rm = TRUE)
    if (denom == 0) {
      col_data <- NA_real_
    } else {
      num <- sum(A == a_lv & B == b_lv, na.rm = TRUE)
      col_data <- num / denom
    }

    target_lst[[i]] <- col_data
  }

  ret <- unlist(target_lst)
  ret[is.na(ret)] <- 0
  return(ret)
}
