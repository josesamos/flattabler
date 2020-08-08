#' Remove rows and columns with aggregated data
#'
#' Removes pivot table rows and columns that contain aggregated data.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' Aggregated data is recognized because the label of the row or column closest
#' to the array of values is empty or has a special value as an indicator.
#'
#' To correctly carry out this operation, the number of rows and columns that
#' contain labels must be defined, and the table must only contain the pivot
#' table rows and columns.

#'
#' @param pt A `pivot_table` object.
#' @param indicator A string, row or column label for aggregates.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table transformation functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' pt <-
#'   pt_m4 %>%
#'   remove_top(1) %>%
#'   define_labels(n_col = 2, n_row = 2) %>%
#'   remove_agg()
#'
#' pt <-
#'   pt_pivottabler %>%
#'   define_labels(n_col = 2, n_row = 2) %>%
#'   remove_agg("Total") %>%
#'   remove_agg()
#'
#' @export
remove_agg <- function(pt,
                       indicator) {
  UseMethod("remove_agg")
}

#' @rdname remove_agg
#' @export
remove_agg.pivot_table <- function(pt,
                                   indicator = "") {
  page <- attr(pt, "page")
  n_col <- attr(pt, "n_col_labels")
  n_row <- attr(pt, "n_row_labels")
  n_extract <- attr(pt, "n_extract")

  rows <- c((n_row + 1):nrow(pt))
  cols <- c((n_col + 1):ncol(pt))

  if (n_col > 0) {
    pt[rows, n_col] <-
      dplyr::na_if(stringr::str_trim(pt[rows, n_col]), indicator)
    pt <- pt[c(rep(TRUE, n_row), !is.na(pt[rows, n_col])), ]
  }
  if (n_row > 0) {
    pt[n_row, cols] <-
      dplyr::na_if(stringr::str_trim(pt[n_row, cols]), indicator)
    pt <- pt[, c(rep(TRUE, n_col), !is.na(pt[n_row, cols]))]
  }
  new_pivot_table(
    pt,
    page = page,
    n_col_labels = n_col,
    n_row_labels = n_row,
    n_extract = n_extract
  )
}
