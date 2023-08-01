#' Extract labels
#'
#' Extract the given set of labels from a table column in compact format to
#' generate a new column in the table.
#'
#' Sometimes a table column includes values of multiple label fields, this is
#' generally known as compact table format. Given a column number and a set of
#' labels, it generates a new column with the labels located at the positions
#' they occupied in the original column and removes them from it.
#'
#' @param pt A `pivot_table` object.
#' @param col A number, column from which labels are extracted.
#' @param labels A vector of strings, set of labels to extract.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table transformation functions
#' @seealso
#'
#' @examples
#'
#' pt <- pt_m4_compact |>
#'   extract_labels(col = 1, labels = c("b1", "b2", "b3", "b4", "Total general"))
#'
#' pt <- pt_ine2871 |>
#'   extract_labels(col = 1, labels = c("18 Granada"))
#'
#' @export
extract_labels <- function(pt, col = 1, labels = c()) {
  UseMethod("extract_labels")
}

#' @rdname extract_labels
#' @export
extract_labels.pivot_table <- function(pt, col = 1, labels = c()) {
  if (col > 0 && length(labels) > 0) {
    page <- attr(pt, "page")
    n_col <- attr(pt, "n_col_labels")
    n_row <- attr(pt, "n_row_labels")
    n_extract <- attr(pt, "n_extract") + 1
    attr(pt, "n_extract") <- n_extract
    df <- data.frame(new = rep("", nrow(pt)), stringsAsFactors = FALSE)
    names(df) <- sprintf("%s_%d", names(pt)[col], n_extract)
    for (label in labels) {
      df[pt[, col] == label, 1] <- label
      pt[pt[, col] == label, col] <- ""
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
      n_row_labels = n_row,
      n_extract = n_extract
    )
  } else {
    pt
  }
}
