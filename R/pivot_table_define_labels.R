
#' Title
#'
#' @param pt
#' @param n_col
#' @param n_row
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
define_labels <- function(pt,
                          n_col,
                          n_row) {
  UseMethod("define_labels", pt)
}

#' Title
#'
#' @param pt
#' @param n_col
#' @param n_row
#'
#' @return
#'
#'
#' @rdname define_labels
#' @export define_labels.pivot_table
#' @method define_labels pivot_table
#' @export
#'
#' @examples
define_labels.pivot_table <- function(pt,
                                      n_col,
                                      n_row) {
  attr(pt, "n_col_labels") <- n_col
  attr(pt, "n_row_labels") <- n_row
  pt
}
