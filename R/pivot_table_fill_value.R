

#' Title
#'
#' @param pt
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
fill_value <- function(pt) {
  UseMethod("fill_value", pt)
}

#' Title
#'
#' @param pt
#'
#' @return
#'
#'
#' @rdname fill_value
#' @export fill_value.pivot_table
#' @method fill_value pivot_table
#' @export
#'
#' @examples
fill_value.pivot_table <- function(pt) {
  rows <- (attr(pt, "n_row_labels") + 1):nrow(pt)
  cols <- (attr(pt, "n_col_labels") + 1):ncol(pt)
  pt[rows, cols] <-
    apply(pt[rows, cols, drop = FALSE], 2, function(x)
      dplyr::na_if(stringr::str_trim(x), ""))
  pt
}

