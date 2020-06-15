

#' Title
#'
#' @param pt
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
view <- function(pt) {
  UseMethod("view", pt)
}

#' Title
#'
#' @param pt
#'
#' @return
#'
#'
#' @rdname view
#' @export view.pivot_table
#' @method view pivot_table
#' @export
#'
#' @examples
view.pivot_table <- function(pt) {
  if (ncol(pt) > 0) {
    utils::View(pt)
  } else {
    utils::View("")
  }
  if (length(attr(pt, "page")) > 0) {
    df <- data.frame(page = attr(pt, "page"), n_col_labels = "", n_row_labels = "")
    df[1, "n_col_labels"] <- attr(pt, "n_col_labels")
    df[1, "n_row_labels"] <- attr(pt, "n_row_labels")
    utils::View(df)
  } else {
    utils::View("")
  }
  invisible(pt)
}
