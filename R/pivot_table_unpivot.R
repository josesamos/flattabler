

#' Title
#'
#' @param pt
#' @param include_page
#' @param include_na
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
unpivot <- function(pt, include_page = TRUE, include_na = FALSE) {
  UseMethod("unpivot", pt)
}

#' Title
#'
#' @param pt
#' @param include_page
#' @param include_na
#'
#' @return
#'
#'
#' @rdname unpivot
#' @export unpivot.pivot_table
#' @method unpivot pivot_table
#' @export
#'
#' @examples
unpivot.pivot_table <- function(pt,
                                include_page = TRUE,
                                include_na = FALSE)
{
  n_col <- attr(pt, "n_col_labels")
  n_row <- attr(pt, "n_row_labels")
  if (include_page) {
    names <- c("page")
    page_v <- c(attr(pt, "page")[length(attr(pt, "page"))])
    n_pag <- 1
  } else {
    names <- c()
    page_v <- c()
    n_pag <- 0
  }
  df <-
    data.frame(matrix(
      ncol = n_pag + n_col + n_row + 1,
      nrow = (ncol(pt) - n_col) * (nrow(pt) - n_row)
    ))
  if (n_col > 0) {
    seq_c <- c(1:n_col)
    for (c in seq_along(seq_c)) {
      names <- c(names, sprintf("col%d", c))
    }
  } else {
    seq_c <- c()
  }
  if (n_row > 0) {
    seq_r <- c(1:n_row)
    for (r in seq_along(seq_r)) {
      names <- c(names, sprintf("row%d", r))
    }
  } else {
    seq_r <- c()
  }
  colnames(df) <- c(names, "value")

  k <- 1
  for (i in (n_row + 1):nrow(pt)) {
    for (j in (n_col + 1):ncol(pt)) {
      df[k, ] <-
        as.vector(unlist(c(page_v, pt[i, seq_c], pt[seq_r, j], pt[i, j])))
      k <- k + 1
    }
  }
  if (!include_na) {
    df <- df[!is.na(df[, "value"]),]
  }
  tibble::tibble(df)
}
