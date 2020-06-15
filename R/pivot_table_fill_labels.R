fill <- function(v) {
  v <- dplyr::na_if(stringr::str_trim(v), "")
  last <- ""
  for (i in  seq_along(v)) {
    if (is.na(v[i])) {
      v[i] <- last
    } else {
      last <- v[i]
    }
  }
  v
}


#' Title
#'
#' @param pt
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
fill_labels <- function(pt) {
  UseMethod("fill_labels", pt)
}

#' Title
#'
#' @param pt
#'
#' @return
#'
#'
#' @rdname fill_labels
#' @export fill_labels.pivot_table
#' @method fill_labels pivot_table
#' @export
#'
#' @examples
fill_labels.pivot_table <- function(pt) {
  if (attr(pt, "n_col_labels") > 1) {
    cols <- c(1:(attr(pt, "n_col_labels") - 1))
  } else {
    cols <- c()
  }
  if (attr(pt, "n_row_labels") > 1) {
    rows <- c(1:(attr(pt, "n_row_labels") - 1))
  } else {
    rows <- c()
  }
  for (c in cols) {
    pt[, c] <- fill(pt[, c])
  }
  for (r in rows) {
    pt[r,] <- fill(pt[r,])
  }
  pt
}
