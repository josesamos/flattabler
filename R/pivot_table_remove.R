
# rows --------------------------------------------------------------------

#' Title
#'
#' @param pt
#' @param r
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
remove_rows <- function(pt, r) {
  UseMethod("remove_rows", pt)
}

#' Title
#'
#' @param pt
#' @param r
#'
#' @return
#'
#'
#' @rdname remove_rows
#' @export remove_rows.pivot_table
#' @method remove_rows pivot_table
#' @export
#'
#' @examples
remove_rows.pivot_table <- function(pt, r) {
  pt[-r,]
}


# cols -----------------------------------------------------------------

#' Title
#'
#' @param pt
#' @param c
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
remove_cols <- function(pt, c) {
  UseMethod("remove_cols", pt)
}

#' Title
#'
#' @param pt
#' @param c
#'
#' @return
#'
#'
#' @rdname remove_cols
#' @export remove_cols.pivot_table
#' @method remove_cols pivot_table
#' @export
#'
#' @examples
remove_cols.pivot_table <- function(pt, c) {
  page <- attr(pt, "page")
  n_col <- attr(pt, "n_col_labels")
  n_row <- attr(pt, "n_row_labels")
  pt <- pt[,-c]
  new_pivot_table(
    pt,
    page = page,
    n_col_labels = n_col,
    n_row_labels = n_row
  )
}


# empty_rows --------------------------------------------------------------

#' Title
#'
#' @param pt
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
remove_empty <- function(pt) {
  UseMethod("remove_empty", pt)
}

#' Title
#'
#' @param pt
#'
#' @return
#'
#'
#' @rdname remove_empty
#' @export remove_empty.pivot_table
#' @method remove_empty pivot_table
#' @export
#'
#' @examples
remove_empty.pivot_table <- function(pt) {
  page <- attr(pt, "page")
  n_col <- attr(pt, "n_col_labels")
  n_row <- attr(pt, "n_row_labels")
  # empty cells with NA
  df <-
    data.frame(lapply(pt, function(x)
      dplyr::na_if(stringr::str_trim(x), "")))
  pt[rowSums(is.na(df)) != ncol(df), colSums(is.na(df)) != nrow(df)]
  new_pivot_table(
    pt,
    page = page,
    n_col_labels = n_col,
    n_row_labels = n_row
  )
}


# top ----------------------------------------------------------------

#' Title
#'
#' @param pt
#' @param n
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
remove_top <- function(pt, n) {
  UseMethod("remove_top", pt)
}

#' Title
#'
#' @param pt
#' @param n
#'
#' @return
#'
#'
#' @rdname remove_top
#' @export remove_top.pivot_table
#' @method remove_top pivot_table
#' @export
#'
#' @examples
remove_top.pivot_table <- function(pt, n) {
  if (n > 0) {
    pt[c(-1:-n), ]
  } else {
    pt
  }
}

# bottom -------------------------------------------------------------

#' Title
#'
#' @param pt
#' @param n
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
remove_bottom <- function(pt, n) {
  UseMethod("remove_bottom", pt)
}

#' Title
#'
#' @param pt
#' @param n
#'
#' @return
#'
#'
#' @rdname remove_bottom
#' @export remove_bottom.pivot_table
#' @method remove_bottom pivot_table
#' @export
#'
#' @examples
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

#' Title
#'
#' @param pt
#' @param n
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
remove_left <- function(pt, n) {
  UseMethod("remove_left", pt)
}

#' Title
#'
#' @param pt
#' @param n
#'
#' @return
#'
#'
#' @rdname remove_left
#' @export remove_left.pivot_table
#' @method remove_left pivot_table
#' @export
#'
#' @examples
remove_left.pivot_table <- function(pt, n) {
  if (n > 0) {
    page <- attr(pt, "page")
    n_col <- attr(pt, "n_col_labels")
    n_row <- attr(pt, "n_row_labels")

    pt <- pt[, c(-1:-n)]

    new_pivot_table(
      pt,
      page = page,
      n_col_labels = n_col,
      n_row_labels = n_row
    )
  } else {
    pt
  }
}


# right -----------------------------------------------------------

#' Title
#'
#' @param pt
#' @param n
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
remove_right <- function(pt, n) {
  UseMethod("remove_right", pt)
}

#' Title
#'
#' @param pt
#' @param n
#'
#' @return
#'
#'
#' @rdname remove_right
#' @export remove_right.pivot_table
#' @method remove_right pivot_table
#' @export
#'
#' @examples
remove_right.pivot_table <- function(pt, n) {
  if (n > 0) {
    page <- attr(pt, "page")
    n_col <- attr(pt, "n_col_labels")
    n_row <- attr(pt, "n_row_labels")

    n_cols <- ncol(pt)
    first_col <- ncol(pt) - n + 1
    pt <- pt[, c(-first_col:-n_cols)]

    new_pivot_table(
      pt,
      page = page,
      n_col_labels = n_col,
      n_row_labels = n_row
    )
  } else {
    pt
  }
}
