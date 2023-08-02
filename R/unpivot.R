#' Unpivot a pivot table
#'
#' Transforms a pivot table into a flat table (implemented by a `tibble`). An
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
#' @return A `tibble`.
#'
#' @family pivot table transformation functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' a_tibble <-
#'   pt_ex |>
#'   remove_top(1) |>
#'   define_labels(n_col = 2, n_row = 2) |>
#'   unpivot(include_page = FALSE)
#'
#' a_tibble <-
#'   pt_ex |>
#'   set_page(1, 1) |>
#'   remove_top(1) |>
#'   define_labels(n_col = 2, n_row = 2) |>
#'   remove_k() |>
#'   replace_dec() |>
#'   fill_values() |>
#'   fill_labels() |>
#'   remove_agg() |>
#'   unpivot()
#'
#' @export
unpivot <- function(pt, include_page, na.rm) UseMethod("unpivot")

#' @rdname unpivot
#' @export
unpivot.pivot_table <- function(pt,
                                include_page = TRUE,
                                na.rm = TRUE)
{
  n_col <- pt$n_col_labels
  n_row <- pt$n_row_labels
  if (include_page & length(pt$page) > 0) {
    names <- "page"
    page_v <- pt$page
    n_pag <- 1
  } else {
    names <- NULL
    page_v <- NULL
    n_pag <- 0
  }
  df <-
    data.frame(matrix(
      ncol = n_pag + n_col + n_row + 1,
      nrow = (ncol(pt$df) - n_col) * (nrow(pt$df) - n_row)
    ), stringsAsFactors = FALSE)
  if (n_col > 0) {
    seq_c <- 1:n_col
    for (c in seq_c) {
      names <- c(names, sprintf("col%d", c))
    }
  } else {
    seq_c <- NULL
  }
  if (n_row > 0) {
    seq_r <- 1:n_row
    for (r in seq_r) {
      names <- c(names, sprintf("row%d", r))
    }
  } else {
    seq_r <- NULL
  }
  colnames(df) <- c(names, "value")

  k <- 1
  for (i in (n_row + 1):nrow(pt$df)) {
    for (j in (n_col + 1):ncol(pt$df)) {
      df[k, ] <-
        as.vector(unlist(c(page_v, pt$df[i, seq_c], pt$df[seq_r, j], pt$df[i, j])))
      k <- k + 1
    }
  }
  if (na.rm) {
    df <- df[!is.na(df[, "value"]),]
  }
  tibble::tibble(df)
}
