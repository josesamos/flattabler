

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
change_dec <- function(pt, sep = ".") {
  UseMethod("change_dec", pt)
}

#' Title
#'
#' @param pt
#' @param sep
#'
#' @return
#'
#'
#' @rdname change_dec
#' @export change_dec.pivot_table
#' @method change_dec pivot_table
#' @export
#'
#' @examples
change_dec.pivot_table <-
  function(pt, sep = ".") {
    if (sep == ".") {
      pattern <- ","
    } else {
      pattern <- "\\."
      sep <- ','
    }
    rows <- (attr(pt, "n_row_labels") + 1):nrow(pt)
    cols <- (attr(pt, "n_col_labels") + 1):ncol(pt)
    pt[rows, cols] <-
      apply(pt[rows, cols, drop = FALSE], 2, function(x)
        stringr::str_replace(x, pattern, sep))
    pt
  }
