#' Transform a pivot table list into a flat table
#'
#' Given a list of pivot tables and a transformation function that flattens a
#' `pivot_table` object, transforms each table using the function and merges the
#' results into a flat table.
#'
#' @param lpt A list of `pivot_table` objects.
#' @param FUN A function, transformation function that flattens a `pivot_table`
#'   object (it returns a `tibble`).
#'
#' @return A `tibble`, a flat table implemented by a `tibble`.
#'
#' @family flat table generation functions
#' @seealso
#'
#' @examples
#'
#' f <- function(pt) {
#'   pt |>
#'     set_page(1, 1) |>
#'     remove_top(1) |>
#'     define_labels(n_col = 2, n_row = 2) |>
#'     remove_k() |>
#'     replace_dec() |>
#'     fill_values() |>
#'     fill_labels() |>
#'     remove_agg() |>
#'     unpivot()
#' }
#'
#' ft <- flatten_table_list(list_pt, f)
#'
#' @export
flatten_table_list <- function(lpt = list(), FUN) {
  lft <- lapply(lpt, FUN)
  ft <- lft[[1]]
  if (length(lft) > 1) {
    for (i in  2:length(lft)) {
      ft <- dplyr::bind_rows(ft,lft[[i]])
    }
  }
  ft
}


