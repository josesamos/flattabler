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
#' @param lpt List of tables.
#' @param col A number, column to consider.
#' @param start_row A number, start row in each table.
#' @param rows_left A number, rows to ignore at the end of each table.
#'
#' @return Data frame with two columns: Labels in the column, and the index of
#'   the table in the list of tables from which they come.
#' @export
#'
#' @examples
#' df <- get_col_values(list_pt_compact, start_row = 4)
#' labels <- sort(unique(df$label))
#'
get_col_values <-
  function(lpt,
           col = 1,
           start_row = 2,
           rows_left = 0) {
    label <- c()
    table <- c()
    for (i in seq_along(lpt)) {
      label <-
        c(label, lpt[[i]][start_row:(nrow(lpt[[i]]) - rows_left), col])
      table <- c(table, rep(i, nrow(lpt[[i]]) - rows_left - start_row + 1))
    }
    data.frame(table, label, stringsAsFactors = FALSE)
  }
