#' Remove thousands separator
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data. Values, even though they are numbers, are
#' represented as text and sometimes include a thousands separator that can be
#' removed using this function.
#'
#' To correctly carry out this operation, the number of rows and columns that
#' contain labels must be defined, and the table must only contain the pivot
#' table rows and columns.
#'
#' @param pt A `pivot_table` object.
#' @param sep A character, thousands separator to remove.
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
#'   remove_k()
#'
#' pt <-
#'   pt_ine2871 %>%
#'   remove_top(6) %>%
#'   remove_bottom(9) %>%
#'   define_labels(n_col = 1, n_row = 2) %>%
#'   remove_k()
#'
#' @export
remove_k <- function(pt, sep = ".") {
  UseMethod("remove_k")
}

#' @rdname remove_k
#' @export
remove_k.pivot_table <-
  function(pt, sep = ".") {
    if (sep == ".") {
      pattern <- "\\."
    } else {
      pattern <- sep
    }
    rows <- (attr(pt, "n_row_labels") + 1):nrow(pt)
    cols <- (attr(pt, "n_col_labels") + 1):ncol(pt)
    pt[rows, cols] <-
      apply(pt[rows, cols, drop = FALSE], 2, function(x)
        stringr::str_replace_all(x, pattern, ""))
    pt
  }
