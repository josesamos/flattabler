

#' Title
#'
#' @param pt
#' @param indicator
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
remove_agg <- function(pt,
                       indicator) {
  UseMethod("remove_agg", pt)
}

#' Title
#'
#' @param pt
#' @param indicator
#'
#' @return
#'
#'
#' @rdname remove_agg
#' @export remove_agg.pivot_table
#' @method remove_agg pivot_table
#' @export
#'
#' @examples
remove_agg.pivot_table <- function(pt,
                                   indicator = "") {
  page <- attr(pt, "page")
  n_col <- attr(pt, "n_col_labels")
  n_row <- attr(pt, "n_row_labels")

  rows <- c((n_row + 1):nrow(pt))
  cols <- c((n_col + 1):ncol(pt))

  if (n_col > 0) {
    pt[rows, n_col] <-
      dplyr::na_if(stringr::str_trim(pt[rows, n_col]), indicator)
    pt <- pt[c(rep(TRUE, n_row),!is.na(pt[rows, n_col])),]
  }
  if (n_row > 0) {
    pt[n_row, cols] <-
      dplyr::na_if(stringr::str_trim(pt[n_row, cols]), indicator)
    pt <- pt[, c(rep(TRUE, n_col),!is.na(pt[n_row, cols]))]
  }
  new_pivot_table(pt,
                  page = page,
                  n_col_labels = n_col,
                  n_row_labels = n_row)
}

