#' Unpivot a pivot table
#'
#' Transforms a pivot table into a flat table (implemented by a tibble). An
#' additional column with page information can be included. NA values can be
#' excluded from the array of values.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' To correctly carry out this operation, the number of rows and columns that
#' contain labels must be defined, and the table must only contain the pivot
#' table rows and columns.
#'
#' @param pt A `pivot_table` object.
#' @param include_page A boolean, indicates whether a column with the page
#'   information is included or not.
#' @param na.rm A boolean, indicates whether NA values from the array of values
#'   are removed or not.
#'
#' @return A tibble.
#'
#' @family flat table generation functions
#' @seealso
#'
#' @examples
#' library(tidyr)
#'
#' a_tibble <-
#'   pt_m4 %>%
#'   remove_top(1) %>%
#'   define_labels(n_col = 2, n_row = 2) %>%
#'   unpivot(include_page = FALSE)
#'
#' a_tibble <-
#'   pt_m4 %>%
#'   set_page(1, 1) %>%
#'   remove_top(1) %>%
#'   define_labels(n_col = 2, n_row = 2) %>%
#'   remove_k() %>%
#'   replace_dec() %>%
#'   fill_values() %>%
#'   fill_labels() %>%
#'   remove_agg() %>%
#'   unpivot()
#'
#' a_tibble <-
#'   pt_pivottabler %>%
#'   define_labels(n_col = 2, n_row = 2) %>%
#'   fill_values() %>%
#'   fill_labels() %>%
#'   remove_agg("Total") %>%
#'   remove_agg() %>%
#'   unpivot(include_page = FALSE, na.rm = FALSE)
#'
#' @export
unpivot <- function(pt, include_page = TRUE, na.rm = TRUE) {
  UseMethod("unpivot")
}

#' @rdname unpivot
#' @export
unpivot.pivot_table <- function(pt,
                                include_page = TRUE,
                                na.rm = TRUE)
{
  n_col <- attr(pt, "n_col_labels")
  n_row <- attr(pt, "n_row_labels")
  if (include_page) {
    names <- c("page")
    page_v <- c(attr(pt, "page")[length(attr(pt, "page"))])
    n_pag <- 1
  } else {
    names <- c()
    page_v <- c()
    n_pag <- 0
  }
  df <-
    data.frame(matrix(
      ncol = n_pag + n_col + n_row + 1,
      nrow = (ncol(pt) - n_col) * (nrow(pt) - n_row)
    ), stringsAsFactors = FALSE)
  if (n_col > 0) {
    seq_c <- c(1:n_col)
    for (c in seq_along(seq_c)) {
      names <- c(names, sprintf("col%d", c))
    }
  } else {
    seq_c <- c()
  }
  if (n_row > 0) {
    seq_r <- c(1:n_row)
    for (r in seq_along(seq_r)) {
      names <- c(names, sprintf("row%d", r))
    }
  } else {
    seq_r <- c()
  }
  colnames(df) <- c(names, "value")

  k <- 1
  for (i in (n_row + 1):nrow(pt)) {
    for (j in (n_col + 1):ncol(pt)) {
      df[k, ] <-
        as.vector(unlist(c(page_v, pt[i, seq_c], pt[seq_r, j], pt[i, j])))
      k <- k + 1
    }
  }
  if (na.rm) {
    df <- df[!is.na(df[, "value"]),]
  }
  tibble::tibble(df)
}
