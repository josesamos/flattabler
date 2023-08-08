#' Divide table
#'
#' Divides a table into tables separated by some empty row or column. Returns a
#' `pivot_table` object list.
#'
#' Sometimes multiple pivot tables are placed in a text document, imported as
#' one text table. This operation recursively divides the initial table into
#' tables separated by some empty row or column. Once a division has been made,
#' it tries to divide each part of the result. An object is generated for each
#' indivisible pivot table. Returns a list of `pivot_table` objects.
#'
#' If individual tables have a header or footer, they should not be separated
#' from the table by empty rows. If they were, objects would be generated from
#' them that must later be removed from the list of objects in the result.
#'
#' The operation can be applied to tables located horizontally, vertically or in
#' a grid on the initial table. The only requirement to be able to divide it is
#' that there is some empty row or column between them.
#'
#' @param pt A data frame.
#'
#' @return A `pivot_table` list.
#'
#' @family flat table list functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' lpt <- divide(df_set_h_v)
#'
#' @export
divide <- function(pt) {
  # empty cells with NA
  df <-
    data.frame(lapply(pt, function(x)
      dplyr::na_if(stringr::str_trim(x), "")), stringsAsFactors = FALSE)
  # get spacer rows and columns
  x <- spacer_rows(df)
  y <- spacer_columns(df)
  # get subtables
  lpt <- vector("list")
  for (i in (1:(length(x) - 1))) {
    for (j in (1:(length(y) - 1))) {
      pt2 <- pt[x[i]:x[(i + 1)], y[j]:y[(j + 1)]]
      df2 <- df[x[i]:x[(i + 1)], y[j]:y[(j + 1)]]
      x2 <- spacer_rows(df2)
      y2 <- spacer_columns(df2)
      if (length(x2) > 2 | length(y2) > 2) {
        # recursively divide
        lpt <-
          c(lpt, divide(data.frame(pt2)))
      } else {
        # remove empty rows and columns
        pt2 <-
          pt2[rowSums(is.na(df2)) != ncol(df2), colSums(is.na(df2)) != nrow(df2)]
        lpt <-
          c(lpt, list(pivot_table(pt2)))
      }
    }
  }
  lpt
}


#' Spacer rows
#'
#' Gets the empty row numbers for a table. If there are several consecutive
#' empty rows, only one is considered. The first and last rows in the table are
#' also added to the list even if they are not empty.
#'
#' @param df A data frame.
#'
#' @return A vector of numbers.
#'
#' @keywords internal
spacer_rows <- function(df) {
  # empty rows
  x <- which((rowSums(is.na(df)) == ncol(df)) == TRUE)
  # consider the first and last rows
  x <- unique(c(1, x, nrow(df)))
  # only one row as spacer
  x <- x[c(x[-1], 0) - x != 1]
}

#' Spacer columns
#'
#' Gets the empty column numbers for a table. If there are several consecutive
#' empty columns, only one is considered. The first and last columns in the
#' table are also added to the list even if they are not empty.
#'
#' @param df A data frame.
#'
#' @return A vector of numbers.
#'
#' @keywords internal
spacer_columns <- function(df) {
  # empty columns
  y <- which((colSums(is.na(df)) == nrow(df)) == TRUE)
  # consider the first and last columns
  y <- unique(c(1, y, ncol(df)))
  # only one column as separator
  y <- y[c(y[-1], 0) - y != 1]
}


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
#' lpt <- divide(df_set_h_v)
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
#' lpt <- divide(df_set_h_v)
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
