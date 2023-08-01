#' `pivot_table` S3 class
#'
#' Creates a `pivot_table` object from a data frame. Additional information
#' associated with the pivot table can be indicated. The data frame data is
#' converted to character data type.
#'
#' @param df A data frame, contains one or more pivot tables.
#' @param page A string, additional information associated with the pivot table.
#' @param n_col_labels A number, number of columns containing pivot table labels.
#' @param n_row_labels A number, number of rows containing pivot table labels.
#' @param n_extract A number, number of new columns added to the table.
#'
#' @return A `pivot_table` object.
#'
#' @family import data functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' df <- df_ex
#' pt <- pivot_table(df)
#'
#' pt <- pivot_table(df, page = "M4")
#'
#' @export
pivot_table <- function(df,
                        page = vector("character"),
                        n_col_labels = 0,
                        n_row_labels = 0,
                        n_extract = 0) {
  stopifnot(is.data.frame(df))
  stopifnot(is.vector(page))

  df <-
    data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  page <- as.character(page)

  structure(
    list(
      df = df,
      page = page,
      n_col_labels = n_col_labels,
      n_row_labels = n_row_labels,
      n_extract = n_extract
    ),
    class = "pivot_table"
  )
}

#' Get the page information of a pivot table
#'
#' Get the page information associated with the pivot table represented by the
#' object.
#'
#' Each pivot table implements a report. The pivot table page represents the
#' context of that report. It is useful when we work with several pivot tables
#' with the same structure: for example, the page can allow us to differentiate
#' their origin, date or author. This information is often included in the file
#' name, sheet name, or cells attached to the pivot table.
#'
#' @param pt A `pivot_table` object.
#'
#' @return A vector of strings.
#'
#' @family pivot table definition functions
#' @seealso
#'
#' @examples
#' page <- get_page(pt_ex)
#'
#' @export
get_page <- function(pt)
  UseMethod("get_page")


#' @rdname get_page
#' @export
get_page.pivot_table <- function(pt) {
  pt$page
}


#' Set page information to a pivot table
#'
#' Define the page information associated with a pivot table. Previously
#' existing information is replaced by new information.
#'
#' Each pivot table implements a report. The pivot table page represents the
#' context of that report. It is useful when we work with several pivot tables
#' with the same structure: for example, the page can allow us to differentiate
#' their origin, date or author. This information is often included in the file
#' name, sheet name, or a cell attached to the pivot table.
#'
#' @param pt A `pivot_table` object.
#' @param row,col A cell (row and column number), page information included in
#'   the table.
#' @param page A string, page information.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table definition functions
#' @seealso
#'
#' @examples
#'
#' pt <- pt_ex |> set_page(1, 1)
#'
#' pt <- pt_ex |> set_page(page = "M4")
#'
#' @export
set_page <- function(pt, row, col, page)
  UseMethod("set_page")


#' @rdname set_page
#' @export
set_page.pivot_table <- function(pt,
                                 row = 0,
                                 col = 0,
                                 page = "") {
  if (row > 0 & col > 0) {
    pt$page <- pt$df[row, col]
  } else {
    pt$page <- page
  }
  pt
}

#' Define the quantity of rows and columns that contain labels
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data. This function defines the quantity of rows and
#' columns that contain labels.
#'
#' @param pt A `pivot_table` object.
#' @param n_col A number, quantity of columns containing pivot table labels.
#' @param n_row A number, quantity of rows containing pivot table labels.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table definition functions
#' @seealso
#'
#' @examples
#'
#' pt <- pt_ex |> define_labels(n_col = 2, n_row = 2)
#'
#' @export
define_labels <-
  function(pt, n_col, n_row)
    UseMethod("define_labels")


#' @rdname define_labels
#' @export
define_labels.pivot_table <- function(pt,
                                      n_col,
                                      n_row) {
  pt$n_col_labels <- n_col
  pt$n_row_labels <- n_row
  pt
}


#' Remove rows from a pivot table
#'
#' Remove the rows whose numbers are indicated from the pivot table represented
#' by the object.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' All rows not belonging to the pivot table must be removed. It is common to
#' find rows with header or footer information, which must be removed.
#'
#' @param pt A `pivot_table` object.
#' @param r A vector of numbers, row numbers.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table definition functions
#' @seealso
#'
#' @examples
#'
#' pt <- pt_ex |> remove_rows(1)
#'
#' pt <- pt_ex |> remove_rows(c(1, 8, 14, 19, 25, 26))
#'
#' @export
remove_rows <- function(pt, r) UseMethod("remove_rows")

#' @rdname remove_rows
#' @export
remove_rows.pivot_table <- function(pt, r) {
  pt$df <- pt$df[-r,]
  pt
}

