

#' Title
#'
#' @param pt
#' @param sep
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
remove_k <- function(pt, sep = ".") {
  UseMethod("remove_k", pt)
}

#' Title
#'
#' @param pt
#' @param sep
#'
#' @return
#'
#'
#' @rdname remove_k
#' @export remove_k.pivot_table
#' @method remove_k pivot_table
#' @export
#'
#' @examples
remove_k.pivot_table <-
  function(pt, sep = ".") {
    if (sep == ".") {
      pattern <- "\\."
    } else {
      pattern <- sep
    }
    rows <- (attr(pt, "n_row_labels") + 1):nrow(pt)
    cols <- (attr(pt, "n_col_labels") + 1):ncol(pt)
    pt[rows, cols] <-
      apply(pt[rows, cols, drop = FALSE], 2, function(x)
        stringr::str_replace(x, pattern, ""))
    pt
  }
