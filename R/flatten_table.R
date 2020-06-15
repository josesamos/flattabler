#' Title
#'
#' @param lpt
#' @param FUN
#'
#' @return
#' @export
#'
#' @examples
flatten_table_list <- function(lpt = list(), FUN) {
  lft <- lapply(lpt, FUN)
  ft <- lft[[1]]
  if (length(lft) > 1) {
    for (i in  2:length(lft)) {
      ft <- dplyr::bind_rows(ft,lft[[i]])
    }
  }
  ft
}

#' Title
#'
#' @param pt
#' @param add_page_row
#' @param add_page_col
#' @param rem_top
#' @param rem_bottom
#' @param rem_left
#' @param rem_right
#' @param n_col_labels
#' @param n_row_labels
#' @param extract_label_col
#' @param extract_label_values
#' @param rem_k
#' @param rem_agg
#' @param include_page
#' @param include_na
#'
#' @return
#' @export
#'
#' @examples
flatten_table <-
  function(pt,
           add_page_row = 0,
           add_page_col = 0,
           rem_top = 0,
           rem_bottom = 0,
           rem_left = 0,
           rem_right = 0,
           n_col_labels = 1,
           n_row_labels = 1,
           extract_label_col = 0,
           extract_label_values = c(),
           rem_k = ".",
           rem_agg = NULL,
           include_page = TRUE,
           include_na = FALSE) {
    pt <- add_page(pt, add_page_row, add_page_col)
    pt <- remove_top(pt, rem_top)
    pt <- remove_bottom(pt, rem_bottom)
    pt <- remove_left(pt, rem_left)
    pt <- remove_right(pt, rem_right)
    pt <- remove_empty(pt)
    pt <-
      define_labels(pt, n_col = n_col_labels, n_row = n_row_labels)
    pt <-
      extract_label(pt, extract_label_col, extract_label_values)
    pt <- remove_k(pt, rem_k)
    pt <- change_dec(pt)
    pt <- fill_value(pt)
    pt <- fill_labels(pt)
    pt <- remove_agg(pt)
    if (!is.null(rem_agg)) {
      pt <- remove_agg(pt, rem_agg)
    }
    unpivot(pt, include_page = include_page, include_na = include_na)
  }
