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
#' df <- data.frame(unclass(pt_m4)[c(1:7)])
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
#' page <- get_page(pt_m4)
#'
#' @export
get_page <- function(pt) UseMethod("get_page")


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
#' pt <- pt_m4 |> set_page(1, 1)
#'
#' pt <- pt_m4 |> set_page(page = "M4")
#'
#' @export
set_page <- function(pt, row, col, page) UseMethod("set_page")


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
