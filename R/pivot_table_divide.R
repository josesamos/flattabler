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
spacer_columns <- function(df) {
  # empty columns
  y <- which((colSums(is.na(df)) == nrow(df)) == TRUE)
  # consider the first and last columns
  y <- unique(c(1, y, ncol(df)))
  # only one column as separator
  y <- y[c(y[-1], 0) - y != 1]
}


#' Divide table
#'
#' Divides a table into tables separated by some empty row or column. Returns a
#' table list.
#'
#' Sometimes multiple pivot tables are placed in a text document, imported as
#' one text table. This operation recursively divides the initial table into
#' tables separated by some empty row or column. Once a division has been made,
#' it tries to divide each part of the result. An object is generated for each
#' indivisible pivot table. Returns a list of pivot_table objects.
#'
#' If individual tables have a header or footer, they should not be separated
#' from the table by empty rows. If they were, objects would be generated from
#' them that must later be removed from the list of objects in the result.
#'
#' The operation can be applied to tables located horizontally, vertically or in
#' a grid on the initial table. The only requirement to be able to divide it is
#' that there is some empty row or column between them.
#'
#' @param pt A pivot_table object.
#'
#' @return A pivot_table object list.
#' @export
#' @keywords internal
#'
#' @examples
#' library(tidyr)
#'
#' lpt <- pt_set_h %>% divide()
#'
#' lpt <- pt_set_v %>% divide()
#'
#' lpt <- pt_set_h_v %>% divide()
#'
divide <- function(pt) {
  UseMethod("divide", pt)
}

#' @rdname divide
#' @export divide.pivot_table
#' @method divide pivot_table
#' @export
divide.pivot_table <- function(pt) {
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
      if (length(x2) > 2 || length(y2) > 2) {
        # recursively divide
        lpt <-
          c(lpt, divide.pivot_table(new_pivot_table(pt2, attr(pt, "page"))))
      } else {
        # remove empty rows and columns
        pt2 <-
          pt2[rowSums(is.na(df2)) != ncol(df2), colSums(is.na(df2)) != nrow(df2)]
        lpt <-
          c(lpt, list(new_pivot_table(pt2, attr(pt, "page"))))
      }
    }
  }
  lpt
}
