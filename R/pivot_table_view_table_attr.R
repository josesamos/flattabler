#' View table and attributes
#'
#' Displays the table and attributes of the object.
#'
#' @param pt A pivot_table object.
#'
#' @return A pivot_table object.
#' @export
#' @keywords internal
#'
view_table_attr <- function(pt) {
  UseMethod("view_table_attr", pt)
}

#' @rdname view_table_attr
#' @export view_table_attr.pivot_table
#' @method view_table_attr pivot_table
#' @export
view_table_attr.pivot_table <- function(pt) {
  if (ncol(pt) > 0) {
    utils::View(pt)
  } else {
    utils::View("")
  }
  if (length(attr(pt, "page")) > 0) {
    df <- data.frame(page = attr(pt, "page"), n_col_labels = "", n_row_labels = "", stringsAsFactors = FALSE)
    df[1, "n_col_labels"] <- attr(pt, "n_col_labels")
    df[1, "n_row_labels"] <- attr(pt, "n_row_labels")
    utils::View(df)
  } else {
    utils::View("")
  }
  invisible(pt)
}
