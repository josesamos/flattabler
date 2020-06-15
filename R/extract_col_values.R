#' Title
#'
#' @param lpt
#' @param col
#' @param start_row
#' @param rows_left
#'
#' @return
#' @export
#'
#' @examples
extract_col_values <- function(lpt, col = 1, start_row = 2, rows_left = 0) {
  value <- c()
  table <- c()
  for (i in seq_along(lpt)) {
    value <- c(value, lpt[[i]][start_row:(nrow(lpt[[i]])-rows_left),col])
    table <- c(table, rep(i, nrow(lpt[[i]])-rows_left-start_row+1))
  }
  data.frame(table, value)
}
