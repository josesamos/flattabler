#' Replace decimal separator
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data. Values, even though they are numbers, are
#' represented as text and sometimes include a decimal separator different from
#' the one needed; it can be replaced using this function.
#'
#' To correctly carry out this operation, the number of rows and columns that
#' contain labels must be defined, and the table must only contain the pivot
#' table rows and columns.
#'
#' The only decimal separators considered are "." and ",".
#'
#' @param pt A `pivot_table` object.
#' @param sep A character, new decimal separator to use.
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
#'   replace_dec()
#'
#' @export
replace_dec <- function(pt, sep = ".") {
  UseMethod("replace_dec")
}

#' @rdname replace_dec
#' @export
replace_dec.pivot_table <-
  function(pt, sep = ".") {
    if (sep == ".") {
      pattern <- ","
    } else {
      pattern <- "\\."
      sep <- ','
    }
    rows <- (attr(pt, "n_row_labels") + 1):nrow(pt)
    cols <- (attr(pt, "n_col_labels") + 1):ncol(pt)
    pt[rows, cols] <-
      apply(pt[rows, cols, drop = FALSE], 2, function(x)
        stringr::str_replace(x, pattern, sep))
    pt
  }
