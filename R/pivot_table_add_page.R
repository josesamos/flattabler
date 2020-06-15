

#' Title
#'
#' @param pt
#' @param row
#' @param col
#' @param page
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
add_page <- function(pt, row = 0, col = 0, page = "") {
  UseMethod("add_page", pt)
}

#' Title
#'
#' @param pt
#' @param row
#' @param col
#' @param page
#'
#' @return
#'
#'
#' @rdname add_page
#' @export add_page.pivot_table
#' @method add_page pivot_table
#' @export
#'
#' @examples
add_page.pivot_table <- function(pt,
                                 row = 0,
                                 col = 0,
                                 page = "") {
  if (row > 0 && col > 0) {
    attr(pt, "page") <- c(attr(pt, "page"), pt[row, col])
  } else if (page != "") {
    attr(pt, "page") <- c(attr(pt, "page"), page)
  }
  pt
}
