#' Fill in missing values in a vector
#'
#' Fills missing values in a vector with previous value.
#'
#' @param v A vector.
#'
#' @return A vector.
#'
#' @keywords internal
fill_vector <- function(v) {
  v <- dplyr::na_if(stringr::str_trim(v), "")
  last <- ""
  for (i in  seq_along(v)) {
    if (is.na(v[i])) {
      v[i] <- last
    } else {
      last <- v[i]
    }
  }
  v
}


#' Fill in missing labels
#'
#' Fills missing values in row and column labels for a pivot table. In columns
#' they are filled down; in rows to the right.
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
#' library(tidyr)
#'
#' pt <-
#'   pt_m4 %>%
#'   remove_top(1) %>%
#'   define_labels(n_col = 2, n_row = 2) %>%
#'   fill_labels()
#'
#' pt <-
#'   pt_ine2871 %>%
#'   remove_top(6) %>%
#'   remove_bottom(9) %>%
#'   define_labels(n_col = 1, n_row = 2) %>%
#'   fill_labels()
#'
#' @export
fill_labels <- function(pt) {
  UseMethod("fill_labels")
}

#' @rdname fill_labels
#' @export
fill_labels.pivot_table <- function(pt) {
  if (attr(pt, "n_col_labels") > 1) {
    cols <- c(1:(attr(pt, "n_col_labels") - 1))
  } else {
    cols <- c()
  }
  if (attr(pt, "n_row_labels") > 1) {
    rows <- c(1:(attr(pt, "n_row_labels") - 1))
  } else {
    rows <- c()
  }
  for (c in cols) {
    pt[, c] <- fill_vector(pt[, c])
  }
  for (r in rows) {
    pt[r,] <- fill_vector(pt[r,])
  }
  pt
}
