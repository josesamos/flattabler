#' Title
#'
#' @param pt
#' @param col
#' @param values
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
extract_label <- function(pt, col = 1, values = c()) {
  UseMethod("extract_label", pt)
}

#' Title
#'
#' @param pt
#' @param col
#' @param values
#'
#' @return
#'
#'
#' @rdname extract_label
#' @export extract_label.pivot_table
#' @method extract_label pivot_table
#' @export
#'
#' @examples
extract_label.pivot_table <- function(pt, col = 1, values = c()) {
  if (col > 0 && length(values) > 0) {
    page <- attr(pt, "page")
    n_col <- attr(pt, "n_col_labels")
    n_row <- attr(pt, "n_row_labels")
    df <- data.frame(new = rep("", nrow(pt)))
    for (v in values) {
      df[pt[, col] == v, 1] <- v
      pt[pt[, col] == v, col] <- ""
    }

    if (col == 1) {
      pt <- cbind(df, pt)
    } else {
      pt <- cbind(pt[, 1:(col - 1), drop = FALSE], df, pt[, col:ncol(pt)])
    }
    new_pivot_table(
      pt,
      page = page,
      n_col_labels = n_col + 1,
      n_row_labels = n_row
    )
  } else {
    pt
  }
}
