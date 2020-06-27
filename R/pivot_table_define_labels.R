#' Define the quantity of rows and columns that contain labels
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data. This function defines the quantity of rows and
#' columns that contain labels.
#'
#' @param pt A pivot_table object.
#' @param n_col A number, quantity of columns containing pivot table labels.
#' @param n_row A number, quantity of rows containing pivot table labels.
#'
#' @return A pivot_table object.
#' @export
#' @keywords internal
#'
#' @examples
#' library(tidyr)
#'
#' pt <- pt_m4 %>% define_labels(n_col = 2, n_row = 2)
#'
define_labels <- function(pt,
                          n_col,
                          n_row) {
  UseMethod("define_labels", pt)
}

#' @rdname define_labels
#' @export define_labels.pivot_table
#' @method define_labels pivot_table
#' @export
define_labels.pivot_table <- function(pt,
                                      n_col,
                                      n_row) {
  attr(pt, "n_col_labels") <- n_col
  attr(pt, "n_row_labels") <- n_row
  pt
}
