#' `pivot_table` S3 class
#'
#' Creates a `pivot_table` object from a data frame. Additional information
#' associated with the pivot table can be indicated. The data frame data is
#' converted to character data type.
#'
#' @param df A data frame, contains one or more pivot tables.
#' @param page A string, additional information associated with the pivot table.
#' @param n_col_labels A number, number of columns containing pivot table labels.
#' @param n_row_labels A number, number of rows containing pivot table labels.
#' @param n_extract A number, number of new columns added to the table.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table definition functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' df <- df_ex
#' pt <- pivot_table(df)
#'
#' pt <- pivot_table(df, page = "M4")
#'
#' @export
pivot_table <- function(df,
                        page = vector("character"),
                        n_col_labels = 0,
                        n_row_labels = 0,
                        n_extract = 0) {
  stopifnot(is.data.frame(df))
  stopifnot(is.vector(page))

  df <-
    data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  df <- assign_names(df)
  page <- as.character(page)

  structure(
    list(
      df = df,
      page = page,
      n_col_labels = n_col_labels,
      n_row_labels = n_row_labels,
      n_extract = n_extract
    ),
    class = "pivot_table"
  )
}


#' data frame col and row names
#'
#' Assign names to the rows and columns of the data frame.
#'
#' @param df A data frame.
#'
#' @return  A data frame.
#'
#' @keywords internal
assign_names <- function(df) {
  if (nrow(df) > 0) row.names(df) <- 1:nrow(df)
  if (length(df) > 0) colnames(df) <- sprintf("V%d", 1:length(df))
  df
}


#' Get the page information of a pivot table
#'
#' Get the page information associated with the pivot table represented by the
#' object.
#'
#' Each pivot table implements a report. The pivot table page represents the
#' context of that report. It is useful when we work with several pivot tables
#' with the same structure: for example, the page can allow us to differentiate
#' their origin, date or author. This information is often included in the file
#' name, sheet name, or cells attached to the pivot table.
#'
#' @param pt A `pivot_table` object.
#'
#' @return A vector of strings.
#'
#' @family pivot table definition functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#' page <- get_page(pt_ex)
#'
#' @export
get_page <- function(pt)
  UseMethod("get_page")


#' @rdname get_page
#' @export
get_page.pivot_table <- function(pt) {
  pt$page
}


#' Set page information to a pivot table
#'
#' Define the page information associated with a pivot table. Previously
#' existing information is replaced by new information.
#'
#' Each pivot table implements a report. The pivot table page represents the
#' context of that report. It is useful when we work with several pivot tables
#' with the same structure: for example, the page can allow us to differentiate
#' their origin, date or author. This information is often included in the file
#' name, sheet name, or a cell attached to the pivot table.
#'
#' @param pt A `pivot_table` object.
#' @param row,col A cell (row and column number), page information included in
#'   the table.
#' @param page A string, page information.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table definition functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pt_ex |> set_page(1, 1)
#'
#' pt <- pt_ex |> set_page(page = "M4")
#'
#' @export
set_page <- function(pt, row, col, page)
  UseMethod("set_page")


#' @rdname set_page
#' @export
set_page.pivot_table <- function(pt,
                                 row = 0,
                                 col = 0,
                                 page = "") {
  if (row > 0 && col > 0) {
    pt$page <- pt$df[row, col]
  } else {
    pt$page <- page
  }
  pt
}

#' Define the quantity of rows and columns that contain labels
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data. This function defines the quantity of rows and
#' columns that contain labels.
#'
#' @param pt A `pivot_table` object.
#' @param n_col A number, quantity of columns containing pivot table labels.
#' @param n_row A number, quantity of rows containing pivot table labels.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table definition functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pt_ex |> define_labels(n_col = 2, n_row = 2)
#'
#' @export
define_labels <-
  function(pt, n_col, n_row)
    UseMethod("define_labels")


#' @rdname define_labels
#' @export
define_labels.pivot_table <- function(pt,
                                      n_col,
                                      n_row) {
  pt$n_col_labels <- n_col
  pt$n_row_labels <- n_row
  pt
}


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
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pt_ex |> remove_rows(1)
#'
#' pt <- pt_ex |> remove_rows(c(1, 8, 14, 19, 25, 26))
#'
#' @export
remove_rows <- function(pt, r)
  UseMethod("remove_rows")

#' @rdname remove_rows
#' @export
remove_rows.pivot_table <- function(pt, r) {
  pt$df <- as.data.frame(pt$df[-r, ], stringsAsFactors = FALSE)
  pt$df <- assign_names(pt$df)
  pt
}

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
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pt_ex |> remove_cols(7)
#'
#' pt <- pt_ex |> remove_cols(c(6,7))
#'
#' @export
remove_cols <- function(pt, c)
  UseMethod("remove_cols")

#' @rdname remove_cols
#' @export
remove_cols.pivot_table <- function(pt, c) {
  pt$df <- as.data.frame(pt$df[, -c], stringsAsFactors = FALSE)
  pt$df <- assign_names(pt$df)
  pt
}


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
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' df <- df_ex
#' df[seq(from = 1, to = 25, by = 2), ] <- " "
#' df[, seq(from = 1, to = 7, by = 2)] <- " "
#' pt <- pivot_table(df)
#' pt <- pt |> remove_empty()
#'
#' @export
remove_empty <- function(pt)
  UseMethod("remove_empty")

#' @rdname remove_empty
#' @export
remove_empty.pivot_table <- function(pt) {
  # empty cells with NA
  pt$df <-
    data.frame(lapply(pt$df, function(x)
      dplyr::na_if(stringr::str_trim(x), "")), stringsAsFactors = FALSE)
  pt$df <-
    as.data.frame(pt$df[rowSums(is.na(pt$df)) != ncol(pt$df), colSums(is.na(pt$df)) != nrow(pt$df)], stringsAsFactors = FALSE)
  pt$df <- assign_names(pt$df)
  pt
}

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
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pt_ex |> remove_top(3)
#'
#' @export
remove_top <- function(pt, n) UseMethod("remove_top")

#' @rdname remove_top
#' @export
remove_top.pivot_table <- function(pt, n) {
  if (n > 0) {
    pt$df <- pt$df[c(-1:-n), ]
    pt$df <- assign_names(pt$df)
  }
  pt
}

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
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pt_ex |> remove_bottom(3)
#'
#' @export
remove_bottom <- function(pt, n) UseMethod("remove_bottom")

#' @rdname remove_bottom
#' @export
remove_bottom.pivot_table <- function(pt, n) {
  if (n > 0) {
    n_rows <- nrow(pt$df)
    if (n >= n_rows) {
      first_row <- 1
    } else {
      first_row <- nrow(pt$df) - n + 1
    }
    pt$df <- pt$df[c(-first_row:-n_rows),]
    pt$df <- assign_names(pt$df)
  }
  pt
}


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
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pt_ex |> remove_left(3)
#'
#' @export
remove_left <- function(pt, n) UseMethod("remove_left")

#' @rdname remove_left
#' @export
remove_left.pivot_table <- function(pt, n) {
  if (n > 0) {
    pt$df <- as.data.frame(pt$df[, c(-1:-n)], stringsAsFactors = FALSE)
    pt$df <- assign_names(pt$df)
  }
  pt
}


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
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pt_ex |> remove_right(3)
#'
#' @export
remove_right <- function(pt, n) UseMethod("remove_right")

#' @rdname remove_right
#' @export
remove_right.pivot_table <- function(pt, n) {
  if (n > 0) {
    n_cols <- ncol(pt$df)
    if (n >= n_cols) {
      first_col <- 1
    } else {
      first_col <- ncol(pt$df) - n + 1
    }
    pt$df <- as.data.frame(pt$df[, c(-first_col:-n_cols)], stringsAsFactors = FALSE)
    pt$df <- assign_names(pt$df)
  }
  pt
}


#' Fill in missing labels
#'
#' Fills missing values in row and column labels for a pivot table. By default,
#' columns are filled down and rows are filled right.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data. The row and column closest to the data array
#' are not filled (they must have data defined for each cell).
#'
#' To correctly carry out this operation, the number of rows and columns that
#' contain labels must be defined, and the table must only contain the pivot
#' table rows and columns.
#'
#' @param pt A `pivot_table` object.
#' @param down A boolean, fill down.
#' @param right A boolean, fill right.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table transformation functions
#' @seealso
#'
#' @examples
#'
#' pt <-
#'   pt_ex |>
#'   remove_top(1) |>
#'   define_labels(n_col = 2, n_row = 2) |>
#'   fill_labels(down = TRUE, right = TRUE)
#'
#' @export
fill_labels <- function(pt, down, right) UseMethod("fill_labels")

#' @rdname fill_labels
#' @export
fill_labels.pivot_table <- function(pt, down = TRUE, right = TRUE) {
  if (pt$n_col_labels > 1) {
    cols <- c(1:(pt$n_col_labels - 1))
  } else {
    cols <- c()
  }
  if (pt$n_row_labels > 1) {
    rows <- c(1:(pt$n_row_labels - 1))
  } else {
    rows <- c()
  }
  for (c in cols) {
    pt$df[, c] <- fill_vector(pt$df[, c], contrary = !down)
  }
  for (r in rows) {
    pt$df[r,] <- fill_vector(pt$df[r,], contrary = !right)
  }
  pt
}


#' Fill in missing values in a vector
#'
#' Fills missing values in a vector with previous value.
#'
#' @param v A vector.
#' @param contrary A boolean, fill in contrary sense.
#'
#' @return A vector.
#'
#' @keywords internal
fill_vector <- function(v, contrary) {
  v <- dplyr::na_if(stringr::str_trim(v), "")
  last <- ""
  if (contrary) {
    lv <- length(v):1
  } else {
    lv <- 1:length(v)
  }
  for (i in  lv) {
    if (is.na(v[i])) {
      v[i] <- last
    } else {
      last <- v[i]
    }
  }
  v
}
