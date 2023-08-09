#' `pivot_table` S3 class
#'
#' Creates a `pivot_table` object from a data frame. Additional information
#' associated with the pivot table can be indicated. The data frame data is
#' converted to character data type.
#'
#' @param df A data frame, contains one or more pivot tables.
#' @param page A string, additional information associated with the pivot table.
#' @param page_row,page_col A cell (row and column number), page information
#' included in the table.
#' @param n_col_labels A number, number of columns containing pivot table labels.
#' @param n_row_labels A number, number of rows containing pivot table labels.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table definition functions
#' @seealso \code{\link{divide}}
#'
#' @examples
#'
#' pt <- pivot_table(df_ex)
#'
#' pt <- pivot_table(df_ex, page = "M4")
#'
#' pt <- pivot_table(df_ex, page_row = 1, page_col = 1)
#'
#' pt <- pivot_table(df_ex, page_row = 1, page_col = 1, n_col_labels = 2, n_row_labels = 2)
#'
#' @export
pivot_table <- function(df,
                        page = "",
                        page_row = 0,
                        page_col = 0,
                        n_col_labels = 0,
                        n_row_labels = 0) {
  stopifnot(is.data.frame(df))
  df <-
    data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  df <- assign_names(df)

  if (page_row > 0 & page_col > 0) {
    page <- df[page_row, page_col]
  }
  else if (page == "") {
    page <- character(0)
  }

  structure(
    list(
      df = df,
      page = page,
      n_col_labels = n_col_labels,
      n_row_labels = n_row_labels
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
  if (nrow(df) > 0)
    row.names(df) <- 1:nrow(df)
  if (length(df) > 0)
    colnames(df) <- sprintf("V%d", 1:length(df))
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
#' page <- pt_ex |> get_page()
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
#' @family pivot table transformation functions
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
  pt$df <- as.data.frame(pt$df[-r,], stringsAsFactors = FALSE)
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
#' @family pivot table transformation functions
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
  pt$df <- as.data.frame(pt$df[,-c], stringsAsFactors = FALSE)
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
#' @family pivot table transformation functions
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
#' @family pivot table transformation functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pt_ex |> remove_top(3)
#'
#' @export
remove_top <- function(pt, n)
  UseMethod("remove_top")

#' @rdname remove_top
#' @export
remove_top.pivot_table <- function(pt, n) {
  if (n > 0) {
    pt$df <- pt$df[c(-1:-n),]
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
#' @family pivot table transformation functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pt_ex |> remove_bottom(3)
#'
#' @export
remove_bottom <- function(pt, n)
  UseMethod("remove_bottom")

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
    pt$df <- pt$df[c(-first_row:-n_rows), ]
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
#' @family pivot table transformation functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pt_ex |> remove_left(3)
#'
#' @export
remove_left <- function(pt, n)
  UseMethod("remove_left")

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
#' @family pivot table transformation functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pt_ex |> remove_right(3)
#'
#' @export
remove_right <- function(pt, n)
  UseMethod("remove_right")

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
    pt$df <-
      as.data.frame(pt$df[, c(-first_col:-n_cols)], stringsAsFactors = FALSE)
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
#' @seealso \code{\link{pivot_table}}
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
fill_labels <- function(pt, down, right)
  UseMethod("fill_labels")

#' @rdname fill_labels
#' @export
fill_labels.pivot_table <- function(pt, down = TRUE, right = TRUE) {
  if (pt$n_col_labels > 1) {
    cols <- c(1:(pt$n_col_labels - 1))
  } else {
    cols <- NULL
  }
  if (pt$n_row_labels > 1) {
    rows <- c(1:(pt$n_row_labels - 1))
  } else {
    rows <- NULL
  }
  for (c in cols) {
    pt$df[, c] <- fill_vector(pt$df[, c], contrary = !down)
  }
  for (r in rows) {
    pt$df[r, ] <- fill_vector(pt$df[r, ], contrary = !right)
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

#' Remove rows and columns with aggregated data
#'
#' Aggregated data is recognized because the label of the row or column closest
#' to the array of values is empty.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' Removes pivot table rows and columns that contain aggregated data. It only
#' checks the value in the row or column closest to the array of values.
#'
#' To correctly carry out this operation, the number of rows and columns that
#' contain labels must be defined, and the table must only contain the pivot
#' table rows and columns.

#'
#' @param pt A `pivot_table` object.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table transformation functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <-
#'   pt_ex |>
#'   remove_top(1) |>
#'   define_labels(n_col = 2, n_row = 2) |>
#'   remove_agg()
#'
#' @export
remove_agg <- function(pt)
  UseMethod("remove_agg")

#' @rdname remove_agg
#' @export
remove_agg.pivot_table <- function(pt) {
  indicator <- ""
  n_col <- pt$n_col_labels
  n_row <- pt$n_row_labels
  cols <- (n_col + 1):ncol(pt$df)
  rows <- (n_row + 1):nrow(pt$df)
  if (n_col > 0) {
    pt$df[rows, n_col] <-
      dplyr::na_if(stringr::str_trim(pt$df[rows, n_col]), indicator)
    pt$df <-
      pt$df[c(rep(TRUE, n_row), !is.na(pt$df[rows, n_col])), ]
  }
  if (n_row > 0) {
    pt$df[n_row, cols] <-
      dplyr::na_if(stringr::str_trim(pt$df[n_row, cols]), indicator)
    pt$df <-
      pt$df[, c(rep(TRUE, n_col), !is.na(pt$df[n_row, cols]))]
  }
  pt$df <- assign_names(pt$df)
  pt
}

#' Fill in missing values
#'
#' Fills with NA missing values in a pivot table value array.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' To correctly carry out this operation, the number of rows and columns that
#' contain labels must be defined, and the table must only contain the pivot
#' table rows and columns.
#'
#' @param pt A `pivot_table` object.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table transformation functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <-
#'   pt_ex |>
#'   remove_top(1) |>
#'   define_labels(n_col = 2, n_row = 2) |>
#'   fill_values()
#'
#' @export
fill_values <- function(pt)
  UseMethod("fill_values")

#' @rdname fill_values
#' @export
fill_values.pivot_table <- function(pt) {
  rows <- (pt$n_row_labels + 1):nrow(pt$df)
  cols <- (pt$n_col_labels + 1):ncol(pt$df)
  pt$df[rows, cols] <-
    apply(pt$df[rows, cols, drop = FALSE], 2, function(x)
      dplyr::na_if(stringr::str_trim(x), ""))
  pt
}


#' Remove thousands separator
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data. Values, even though they are numbers, are
#' represented as text and sometimes include a thousands separator that can be
#' removed using this function.
#'
#' To correctly carry out this operation, the number of rows and columns that
#' contain labels must be defined, and the table must only contain the pivot
#' table rows and columns.
#'
#' @param pt A `pivot_table` object.
#' @param sep A character, thousands separator to remove.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table transformation functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <-
#'   pt_ex |>
#'   remove_top(1) |>
#'   define_labels(n_col = 2, n_row = 2) |>
#'   remove_k()
#'
#' @export
remove_k <- function(pt, sep)
  UseMethod("remove_k")

#' @rdname remove_k
#' @export
remove_k.pivot_table <- function(pt, sep = ".") {
  if (sep == ".") {
    pattern <- "\\."
  } else {
    pattern <- sep
  }
  rows <- (pt$n_row_labels + 1):nrow(pt$df)
  cols <- (pt$n_col_labels + 1):ncol(pt$df)
  pt$df[rows, cols] <-
    apply(pt$df[rows, cols, drop = FALSE], 2, function(x)
      stringr::str_replace_all(x, pattern, ""))
  pt
}


#' Replace decimal separator
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data. Values, even though they are numbers, are
#' represented as text and sometimes include a decimal separator different from
#' the one needed; it can be replaced using this function.
#'
#' To correctly carry out this operation, the number of rows and columns that
#' contain labels must be defined, and the table must only contain the pivot
#' table rows and columns.
#'
#' The only decimal separators considered are "." and ",".
#'
#' @param pt A `pivot_table` object.
#' @param sep A character, new decimal separator to use.
#'
#' @return A `pivot_table` object.
#'
#' @family pivot table transformation functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <-
#'   pt_ex |>
#'   remove_top(1) |>
#'   define_labels(n_col = 2, n_row = 2) |>
#'   replace_dec()
#'
#' @export
replace_dec <- function(pt, sep)
  UseMethod("replace_dec")

#' @rdname replace_dec
#' @export
replace_dec.pivot_table <- function(pt, sep = ".") {
  if (sep == ".") {
    pattern <- ","
  } else {
    pattern <- "\\."
    sep <- ','
  }
  rows <- (pt$n_row_labels + 1):nrow(pt$df)
  cols <- (pt$n_col_labels + 1):ncol(pt$df)
  pt$df[rows, cols] <-
    apply(pt$df[rows, cols, drop = FALSE], 2, function(x)
      stringr::str_replace(x, pattern, sep))
  pt
}


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
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pivot_table(df_ex_compact) |>
#'   extract_labels(col = 1, labels = c("b1", "b2", "b3", "b4", "Total general"))
#'
#' @export
extract_labels <-
  function(pt, col, labels)
    UseMethod("extract_labels")

#' @rdname extract_labels
#' @export
extract_labels.pivot_table <- function(pt, col = 1, labels = c()) {
  if (col > 0 && length(labels) > 0) {
    df <-
      data.frame(new = rep("", nrow(pt$df)), stringsAsFactors = FALSE)
    for (label in labels) {
      df[pt$df[, col] == label, 1] <- label
      pt$df[pt$df[, col] == label, col] <- ""
    }
    if (col == 1) {
      pt$df <- cbind(df, pt$df)
    } else {
      pt$df <-
        cbind(pt$df[, 1:(col - 1), drop = FALSE], df, pt$df[, col:ncol(pt$df)])
    }
    if (pt$n_col_labels > 0) {
      pt$n_col_labels <- pt$n_row_labels + 1
    }
    pt$df <- assign_names(pt$df)
  }
  pt
}


#' Divide table
#'
#' Divides a table into tables separated by some empty row or column. Returns a
#' `pivot_table` object list.
#'
#' Sometimes multiple pivot tables are placed in a text document, imported as
#' one text table. This operation recursively divides the initial table into
#' tables separated by some empty row or column. Once a division has been made,
#' it tries to divide each part of the result. An object is generated for each
#' indivisible pivot table. Returns a list of `pivot_table` objects.
#'
#' If individual tables have a header or footer, they should not be separated
#' from the table by empty rows. If they were, objects would be generated from
#' them that must later be removed from the list of objects in the result.
#'
#' The operation can be applied to tables located horizontally, vertically or in
#' a grid on the initial table. The only requirement to be able to divide it is
#' that there is some empty row or column between them.
#'
#' @param pt A `pivot_table` object.
#'
#' @return A `pivot_table` list.
#'
#' @family flat table list functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' pt <- pivot_table(df_set_h_v)
#' lpt <- pt |> divide()
#'
#' @export
divide <- function(pt) UseMethod("divide")

#' @rdname divide
#' @export
divide.pivot_table <- function(pt) {
  # empty cells with NA
  df <-
    data.frame(lapply(pt$df, function(x)
      dplyr::na_if(stringr::str_trim(x), "")), stringsAsFactors = FALSE)
  # get spacer rows and columns
  x <- spacer_rows(df)
  y <- spacer_columns(df)
  # get subtables
  lpt <- vector("list")
  for (i in (1:(length(x) - 1))) {
    for (j in (1:(length(y) - 1))) {
      pt2 <- pt
      pt2$df <- pt$df[x[i]:x[(i + 1)], y[j]:y[(j + 1)]]
      pt2$df <- assign_names(pt2$df)
      df2 <- df[x[i]:x[(i + 1)], y[j]:y[(j + 1)]]
      x2 <- spacer_rows(df2)
      y2 <- spacer_columns(df2)
      if (length(x2) > 2 | length(y2) > 2) {
        # recursively divide
        lpt <-
          c(lpt, divide(pt2))
      } else {
        # remove empty rows and columns
        pt2$df <-
          pt2$df[rowSums(is.na(df2)) != ncol(df2), colSums(is.na(df2)) != nrow(df2)]
        pt2$df <- assign_names(pt2$df)
        lpt <-
          c(lpt, list(pt2))
      }
    }
  }
  lpt
}


#' Spacer rows
#'
#' Gets the empty row numbers for a table. If there are several consecutive
#' empty rows, only one is considered. The first and last rows in the table are
#' also added to the list even if they are not empty.
#'
#' @param df A data frame.
#'
#' @return A vector of numbers.
#'
#' @keywords internal
spacer_rows <- function(df) {
  # empty rows
  x <- which((rowSums(is.na(df)) == ncol(df)) == TRUE)
  # consider the first and last rows
  x <- unique(c(1, x, nrow(df)))
  # only one row as spacer
  x <- x[c(x[-1], 0) - x != 1]
}

#' Spacer columns
#'
#' Gets the empty column numbers for a table. If there are several consecutive
#' empty columns, only one is considered. The first and last columns in the
#' table are also added to the list even if they are not empty.
#'
#' @param df A data frame.
#'
#' @return A vector of numbers.
#'
#' @keywords internal
spacer_columns <- function(df) {
  # empty columns
  y <- which((colSums(is.na(df)) == nrow(df)) == TRUE)
  # consider the first and last columns
  y <- unique(c(1, y, ncol(df)))
  # only one column as separator
  y <- y[c(y[-1], 0) - y != 1]
}


#' Unpivot a pivot table
#'
#' Transforms a pivot table into a flat table (implemented by a `tibble`). An
#' additional column with page information can be included. NA values can be
#' excluded from the array of values.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' To correctly carry out this operation, the number of rows and columns that
#' contain labels must be defined, and the table must only contain the pivot
#' table rows and columns.
#'
#' @param pt A `pivot_table` object.
#' @param include_page A boolean, indicates whether a column with the page
#'   information is included or not.
#' @param na.rm A boolean, indicates whether NA values from the array of values
#'   are removed or not.
#'
#' @return A `tibble`.
#'
#' @family pivot table transformation functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' a_tibble <-
#'   pt_ex |>
#'   remove_top(1) |>
#'   define_labels(n_col = 2, n_row = 2) |>
#'   unpivot(include_page = FALSE)
#'
#' a_tibble <-
#'   pt_ex |>
#'   set_page(1, 1) |>
#'   remove_top(1) |>
#'   define_labels(n_col = 2, n_row = 2) |>
#'   remove_k() |>
#'   replace_dec() |>
#'   fill_values() |>
#'   fill_labels() |>
#'   remove_agg() |>
#'   unpivot()
#'
#' @export
unpivot <- function(pt, include_page, na.rm) UseMethod("unpivot")

#' @rdname unpivot
#' @export
unpivot.pivot_table <- function(pt,
                                 include_page = TRUE,
                                 na.rm = TRUE)
{
  n_col <- pt$n_col_labels
  n_row <- pt$n_row_labels
  if (include_page & length(pt$page) > 0) {
    names <- "page"
    page_v <- pt$page
    n_pag <- 1
  } else {
    names <- NULL
    page_v <- NULL
    n_pag <- 0
  }
  df <-
    data.frame(matrix(
      ncol = n_pag + n_col + n_row + 1,
      nrow = (ncol(pt$df) - n_col) * (nrow(pt$df) - n_row)
    ),
    stringsAsFactors = FALSE)
  if (n_col > 0) {
    seq_c <- 1:n_col
    for (c in seq_c) {
      names <- c(names, sprintf("col%d", c))
    }
  } else {
    seq_c <- NULL
  }
  if (n_row > 0) {
    seq_r <- 1:n_row
    for (r in seq_r) {
      names <- c(names, sprintf("row%d", r))
    }
  } else {
    seq_r <- NULL
  }
  colnames(df) <- c(names, "value")

  k <- 1
  if (n_pag == 1) {
    df[, k] <- page_v
    k <- k + 1
  }
  for (j in 1:n_col) {
    df[, k] <-
      rep(pt$df[(n_row + 1):nrow(pt$df), j], each = ncol(pt$df) - n_col)
    k <- k + 1
  }
  for (i in 1:n_row) {
    df[, k] <-
      rep(t(pt$df[i, (n_col + 1):ncol(pt$df)]), nrow(pt$df) - n_row)
    k <- k + 1
  }
  df[, k] <-
    as.vector(t(pt$df[(n_row + 1):nrow(pt$df), (n_col + 1):ncol(pt$df)]))
  if (na.rm) {
    df <- df[!is.na(df[, k]), ]
  }
  tibble::tibble(df)
}
