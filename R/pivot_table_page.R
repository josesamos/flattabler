#' Get the page information of a pivot_table object
#'
#' Get the page information associated with the pivot table represented by the
#' object.
#'
#' Each pivot table implements a report. The pivot table page represents the
#' context of that report. It is useful when we work with several pivot tables
#' with the same structure: for example, the page can allow us to differentiate
#' their origin, date, author, etc. This information is often included in the
#' file name, sheet name, or cells attached to the pivot table.
#'
#' @param pt A pivot_table object.
#'
#' @return A vector of strings.
#' @export
#' @keywords internal
#'
#' @examples
#' page <- get_page(pt_m4)
#'
get_page <- function(pt) {
  UseMethod("get_page", pt)
}

#' @rdname get_page
#' @export get_page.pivot_table
#' @method get_page pivot_table
#' @export
get_page.pivot_table <- function(pt) {
  attr(pt, "page")
}


#' Set page information to a pivot_table object
#'
#' Define the page information associated with a pivot table. Previously
#' existing information is replaced by new information.
#'
#' Each pivot table implements a report. The pivot table page represents the
#' context of that report. It is useful when we work with several pivot tables
#' with the same structure: for example, the page can allow us to differentiate
#' their origin, date, author, etc. This information is often included in the
#' file name, sheet name, or a cell attached to the pivot table.
#'
#' @param pt A pivot_table object.
#' @param row,col A cell (row and column number), page information included in
#'   the table.
#' @param page A string, page information.
#'
#' @return A pivot_table object.
#' @export
#' @keywords internal
#'
#' @examples
#' library(tidyr)
#'
#' pt <- pt_m4 %>% set_page(1, 1)
#'
#' pt <- pt_m4 %>% set_page(page = "M4")
#'
set_page <- function(pt, row = 0, col = 0, page = "") {
  UseMethod("set_page", pt)
}

#' @rdname set_page
#' @export set_page.pivot_table
#' @method set_page pivot_table
#' @export
set_page.pivot_table <- function(pt,
                                 row = 0,
                                 col = 0,
                                 page = "") {
  if (row > 0 && col > 0) {
    attr(pt, "page") <- pt[row, col]
  } else {
    attr(pt, "page") <- page
  }
  pt
}
