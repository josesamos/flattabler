spacer_rows <- function(df) {
  # empty rows
  x <- which((rowSums(is.na(df)) == ncol(df)) == TRUE)
  # consider the first and last rows
  x <- unique(c(1, x, nrow(df)))
  # only one row as spacer
  x <- x[c(x[-1], 0) - x != 1]
}

spacer_columns <- function(df) {
  # empty columns
  y <- which((colSums(is.na(df)) == nrow(df)) == TRUE)
  # consider the first and last columns
  y <- unique(c(1, y, ncol(df)))
  # only one column as separator
  y <- y[c(y[-1], 0) - y != 1]
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
divide <- function(pt) {
  UseMethod("divide", pt)
}

#' Title
#'
#' @param pt
#'
#' @return
#'
#'
#' @rdname divide
#' @export divide.pivot_table
#' @method divide pivot_table
#' @export
#'
#' @examples
divide.pivot_table <- function(pt) {
  # empty cells with NA
  df <-
    data.frame(lapply(pt, function(x)
      dplyr::na_if(stringr::str_trim(x), "")))
  # get spacer rows and columns
  x <- spacer_rows(df)
  y <- spacer_columns(df)
  # get subtables
  lpt <- vector("list")
  for (i in (1:(length(x) - 1))) {
    for (j in (1:(length(y) - 1))) {
      pt2 <- pt[x[i]:x[(i + 1)], y[j]:y[(j + 1)]]
      df2 <- df[x[i]:x[(i + 1)], y[j]:y[(j + 1)]]
      x2 <- spacer_rows(df2)
      y2 <- spacer_columns(df2)
      if (length(x2) > 2 || length(y2) > 2) {
        # recursively divide
        lpt <-
          c(lpt, divide.pivot_table(new_pivot_table(pt2, attr(pt, "page"))))
      } else {
        # remove empty rows and columns
        pt2 <-
          pt2[rowSums(is.na(df2)) != ncol(df2), colSums(is.na(df2)) != nrow(df2)]
        lpt <-
          c(lpt, list(new_pivot_table(pt2, attr(pt, "page"))))
      }
    }
  }
  lpt
}
