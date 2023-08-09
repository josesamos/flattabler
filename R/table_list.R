
#' Transform a `pivot_table` object list into a flat table
#'
#' Given a list of `pivot_table` objects and a transformation function that
#' flattens a `pivot_table` object, transforms each object using the function
#' and merges the results into a flat table.
#'
#' @param lpt A list of `pivot_table` objects.
#' @param FUN A function, transformation function that flattens a `pivot_table`
#'   object (it returns a `tibble`).
#'
#' @return A `tibble`, a flat table implemented by a `tibble`.
#'
#' @family flat table list functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' f <- function(pt) {
#'  pt |>
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
#' pt <- pivot_table(df_set_h_v)
#' lpt <- pt |> divide()
#' ft <- flatten_table_list(lpt, f)
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


#' Get column values
#'
#' Gets the values of the indicated column of each table in a list of tables,
#' avoiding the rows at the beginning or the end of each table that are
#' indicated.
#'
#' Sometimes a column includes values of multiple label fields. To facilitate
#' the study of the labels included in the same column of several tables, this
#' function gets the values of the indicated column in a list of tables.
#'
#' @param lpt `pivot_table` object list.
#' @param col A number, column to consider.
#' @param start_row A number, start row in each table.
#' @param rows_left A number, rows to ignore at the end of each table.
#'
#' @return Data frame with two columns: Labels in the column, and the index of
#'   the table in the list of tables from which they come.
#'
#' @family flat table list functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pivot_table(df_set_h_v)
#' lpt <- pt |> divide()
#' df <- get_col_values(lpt, col = 1, start_row = 4)
#' labels <- sort(unique(df$label))
#'
#' @export
get_col_values <-
  function(lpt,
           col = 1,
           start_row = 2,
           rows_left = 0) {
    label <- c()
    table <- c()
    for (i in seq_along(lpt)) {
      nr <- nrow(lpt[[i]]$df)
      label <-
        c(label, lpt[[i]]$df[start_row:(nr - rows_left), col])
      table <- c(table, rep(i, nr - rows_left - start_row + 1))
    }
    data.frame(label, table, stringsAsFactors = FALSE)
  }
