
# rows --------------------------------------------------------------------

#' Remove rows from a pivot table
#'
#' Remove the rows whose numbers are indicated from the pivot table represented
#' by the object.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' All rows not belonging to the pivot table must be removed. It is common to
#' find rows with header or footer information, which must be removed.
#'
#' @param pt A `pivot_table` object.
#' @param r A vector of numbers, row numbers.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table definition functions
#' @seealso
#'
#' @examples
#'
#' pt <- pt_m4 |> remove_rows(1)
#'
#' pt <- pt_m4 |> remove_rows(c(1, 8, 14, 19, 25, 26))
#'
#' @export
remove_rows <- function(pt, r) {
  UseMethod("remove_rows")
}

#' @rdname remove_rows
#' @export
remove_rows.pivot_table <- function(pt, r) {
  pt[-r,]
}


# cols -----------------------------------------------------------------

#' Remove columns from a pivot table
#'
#' Remove the columns whose numbers are indicated from the pivot table
#' represented by the object.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' All columns not belonging to the pivot table must be removed.
#'
#' @param pt A `pivot_table` object.
#' @param c A vector of numbers, column numbers.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table definition functions
#' @seealso
#'
#' @examples
#'
#' pt <- pt_m4 |> remove_cols(7)
#'
#' pt <- pt_m4 |> remove_cols(c(6,7))
#'
#' @export
remove_cols <- function(pt, c) {
  UseMethod("remove_cols")
}

#' @rdname remove_cols
#' @export
remove_cols.pivot_table <- function(pt, c) {
  page <- attr(pt, "page")
  n_col <- attr(pt, "n_col_labels")
  n_row <- attr(pt, "n_row_labels")
  n_extract <- attr(pt, "n_extract")
  pt <- as.data.frame(pt[,-c], stringsAsFactors = FALSE)
  new_pivot_table(
    pt,
    page = page,
    n_col_labels = n_col,
    n_row_labels = n_row,
    n_extract = n_extract
  )
}


# empty --------------------------------------------------------------

#' Remove empty rows and columns from a pivot table
#'
#' Remove rows and columns without data from the pivot table represented by the
#' object.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' All rows and columns not belonging to the pivot table must be removed,
#' including those without data.
#'
#' @param pt A `pivot_table` object.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table definition functions
#' @seealso
#'
#' @examples
#'
#' pt <- pt_m4 |> remove_empty()
#'
#' pt <- pt_ine2871 |> remove_empty()
#'
#' @export
remove_empty <- function(pt) {
  UseMethod("remove_empty")
}

#' @rdname remove_empty
#' @export
remove_empty.pivot_table <- function(pt) {
  page <- attr(pt, "page")
  n_col <- attr(pt, "n_col_labels")
  n_row <- attr(pt, "n_row_labels")
  n_extract <- attr(pt, "n_extract")
  # empty cells with NA
  df <-
    data.frame(lapply(pt, function(x)
      dplyr::na_if(stringr::str_trim(x), "")), stringsAsFactors = FALSE)
  pt <-
    as.data.frame(pt[rowSums(is.na(df)) != ncol(df), colSums(is.na(df)) != nrow(df)], stringsAsFactors = FALSE)
  new_pivot_table(
    pt,
    page = page,
    n_col_labels = n_col,
    n_row_labels = n_row,
    n_extract = n_extract
  )
}


# top ----------------------------------------------------------------

#' Remove top rows from a pivot table
#'
#' Remove top rows from the pivot table represented by the object.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' All rows not belonging to the pivot table must be removed. It is common to
#' find rows with header information, which must be removed.
#'
#' @param pt A `pivot_table` object.
#' @param n A number, number of rows to remove.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table definition functions
#' @seealso
#'
#' @examples
#'
#' pt <- pt_m4 |> remove_top(1)
#'
#' pt <- pt_ine2871 |> remove_top(6)
#'
#' @export
remove_top <- function(pt, n) {
  UseMethod("remove_top")
}

#' @rdname remove_top
#' @export
remove_top.pivot_table <- function(pt, n) {
  if (n > 0) {
    pt[c(-1:-n), ]
  } else {
    pt
  }
}

# bottom -------------------------------------------------------------

#' Remove bottom rows from a pivot table
#'
#' Remove bottom rows from the pivot table represented by the object.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' All rows not belonging to the pivot table must be removed. It is common to
#' find rows with footer information, which must be removed.
#'
#' This function is very useful because it is not necessary to know the number
#' of rows in the table.
#'
#' @param pt A `pivot_table` object.
#' @param n A number, number of rows to remove.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table definition functions
#' @seealso
#'
#' @examples
#'
#' pt <- pt_m4 |> remove_bottom(1)
#'
#' pt <- pt_ine2871 |> remove_bottom(9)
#'
#' @export
remove_bottom <- function(pt, n) {
  UseMethod("remove_bottom")
}

#' @rdname remove_bottom
#' @export
remove_bottom.pivot_table <- function(pt, n) {
  if (n > 0) {
    n_rows <- nrow(pt)
    first_row <- nrow(pt) - n + 1
    pt[c(-first_row:-n_rows),]
  } else {
    pt
  }
}


# left ------------------------------------------------------------

#' Remove left columns from a pivot table
#'
#' Remove left columns from the pivot table represented by the object.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' All columns not belonging to the pivot table must be removed.
#'
#' @param pt A `pivot_table` object.
#' @param n A number, number of columns to remove.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table definition functions
#' @seealso
#'
#' @examples
#'
#' pt <- pt_m4 |> remove_left(2)
#'
#' @export
remove_left <- function(pt, n) {
  UseMethod("remove_left")
}

#' @rdname remove_left
#' @export
remove_left.pivot_table <- function(pt, n) {
  if (n > 0) {
    page <- attr(pt, "page")
    n_col <- attr(pt, "n_col_labels")
    n_row <- attr(pt, "n_row_labels")
    n_extract <- attr(pt, "n_extract")
    pt <- as.data.frame(pt[, c(-1:-n)], stringsAsFactors = FALSE)
    new_pivot_table(
      pt,
      page = page,
      n_col_labels = n_col,
      n_row_labels = n_row,
      n_extract = n_extract
    )
  } else {
    pt
  }
}


# right -----------------------------------------------------------

#' Remove right columns from a pivot table
#'
#' Remove right columns from the pivot table represented by the object.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' All columns not belonging to the pivot table must be removed.
#'
#' This function is very useful because it is not necessary to know the number
#' of columns in the table.
#'
#' @param pt A `pivot_table` object.
#' @param n A number, number of columns to remove.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table definition functions
#' @seealso
#'
#' @examples
#'
#' pt <- pt_m4 |> remove_right(2)
#'
#' @export
remove_right <- function(pt, n) {
  UseMethod("remove_right")
}

#' @rdname remove_right
#' @export
remove_right.pivot_table <- function(pt, n) {
  if (n > 0) {
    page <- attr(pt, "page")
    n_col <- attr(pt, "n_col_labels")
    n_row <- attr(pt, "n_row_labels")
    n_extract <- attr(pt, "n_extract")
    n_cols <- ncol(pt)
    first_col <- ncol(pt) - n + 1
    pt <- as.data.frame(pt[, c(-first_col:-n_cols)], stringsAsFactors = FALSE)
    new_pivot_table(
      pt,
      page = page,
      n_col_labels = n_col,
      n_row_labels = n_row,
      n_extract = n_extract
    )
  } else {
    pt
  }
}
