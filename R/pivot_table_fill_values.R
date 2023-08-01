#' Fill in missing values
#'
#' Fills with NA missing values in a pivot table value array.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' To correctly carry out this operation, the number of rows and columns that
#' contain labels must be defined, and the table must only contain the pivot
#' table rows and columns.
#'
#' @param pt A `pivot_table` object.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table transformation functions
#' @seealso
#'
#' @examples
#'
#' pt <-
#'   pt_m4 |>
#'   remove_top(1) |>
#'   define_labels(n_col = 2, n_row = 2) |>
#'   fill_values()
#'
#' pt <-
#'   pt_ine2871 |>
#'   remove_top(6) |>
#'   remove_bottom(9) |>
#'   define_labels(n_col = 1, n_row = 2) |>
#'   fill_values()
#'
#' @export
fill_values <- function(pt) {
  UseMethod("fill_values")
}

#' @rdname fill_values
#' @export
fill_values.pivot_table <- function(pt) {
  rows <- (attr(pt, "n_row_labels") + 1):nrow(pt)
  cols <- (attr(pt, "n_col_labels") + 1):ncol(pt)
  pt[rows, cols] <-
    apply(pt[rows, cols, drop = FALSE], 2, function(x)
      dplyr::na_if(stringr::str_trim(x), ""))
  pt
}

