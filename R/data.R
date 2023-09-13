#' Pivot table in data frame with with thousands indicator and decimal numbers
#'
#' Pivot table in data frame with with thousands indicator and decimal numbers.
#'
#' @family pivot table in data frame
#' @seealso \code{\link{pt_ex}}
#'
#' @format A data frame.
#'
"df_ex"

#' Pivot table with with thousands indicator and decimal numbers
#'
#' Pivot table with with thousands indicator and decimal numbers.
#'
#' @family pivot table
#' @seealso \code{\link{df_ex}}
#' @examples
#' # Defined by:
#'
#' pt_ex <- pivot_table(df_ex)
#'
#' @format A `pivot_table` object.
#'
"pt_ex"

#' Flat table with page from a pivot table with with thousands indicator and decimal numbers
#'
#' Flat table with page from a pivot table with with thousands indicator and decimal numbers.
#'
#' @family flat table
#' @seealso \code{\link{df_ex}}
#' @examples
#' # Defined by:
#'
#' ft_ex <- pivot_table(df_ex) |>
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
#' @format A `tibble` object.
#'
"ft_ex"

#' Flat table without page from a pivot table with with thousands indicator and decimal numbers
#'
#' Flat table without page from a pivot table with with thousands indicator and decimal numbers.
#'
#' @family flat table
#' @seealso \code{\link{df_ex}}
#' @examples
#' # Defined by:
#'
#' ft_ex_v2 <- pivot_table(df_ex) |>
#'   set_page(1, 1) |>
#'   remove_top(1) |>
#'   define_labels(n_col = 2, n_row = 2) |>
#'   remove_k() |>
#'   replace_dec() |>
#'   fill_values() |>
#'   fill_labels() |>
#'   remove_agg() |>
#'   unpivot(include_page = FALSE,
#'   na_rm = FALSE)
#'
#' @format A `tibble` object.
#'
"ft_ex_v2"

#' Pivot table in data frame with a column with data from two label fields
#'
#' Pivot table in data frame in compact table format: with a column with data
#' from two label fields.
#'
#' @family pivot table in data frame
#' @seealso \code{\link{pf_ex_compact}}
#'
#' @format A data frame.
#'
"df_ex_compact"

#' Pivot table result of transforming a data frame with a column with data from two label fields
#'
#' Pivot table result of transforming a data frame in compact table format: with a column with data
#' from two label fields.
#'
#' @family pivot table
#' @seealso \code{\link{df_ex_compact}}
#' @examples
#' # Defined by:
#'
#' pf_ex_compact <- pivot_table(df_ex_compact) |>
#'   extract_labels(col = 1,
#'                  labels = c("b1", "b2", "b3", "b4", "Total general"))
#'
#' @format A`pivot_table` object.
#'
"pf_ex_compact"

#' Set of pivot tables placed horizontally on one sheet
#'
#' Set of pivot tables placed horizontally on one sheet.
#'
#' @family pivot table set in data frame
#' @seealso \code{\link{df_ex}}
#'
#' @format A data frame.
#'
"df_set_h"

#' Set of pivot tables on one sheet
#'
#' Example of a set of pivot tables located horizontally and vertically on one
#' sheet.
#'
#' @family pivot table set in data frame
#' @seealso \code{\link{df_ex}}
#'
#' @format A data frame.
#'
"df_set_h_v"

#' Set of pivot tables placed vertically on one sheet
#'
#' Set of pivot tables placed vertically on one sheet.
#'
#' @family pivot table set in data frame
#' @seealso \code{\link{df_ex}}
#'
#' @format A data frame.
#'
"df_set_v"

#' Flat table with page from a pivot table with with thousands indicator and decimal numbers
#'
#' Flat table with page from a pivot table with with thousands indicator and decimal numbers.
#'
#' @family flat table
#' @seealso \code{\link{df_set_h_v}}
#' @examples
#' # Defined by:
#'
#' f <- function(pt) {
#'  pt |>
#'     set_page(1, 1) |>
#'     remove_top(1) |>
#'     define_labels(n_col = 2, n_row = 2) |>
#'     remove_k() |>
#'     replace_dec() |>
#'     fill_values() |>
#'     fill_labels() |>
#'     remove_agg() |>
#'     unpivot()
#' }
#'
#' pt <- pivot_table(df_set_h_v)
#' lpt <- pt |> divide()
#' ft_set <- flatten_table_list(lpt, f)
#'
#' @format A `tibble` object.
#'
"ft_set"

#' Pivot table with basic and subtotal labels in the same column
#'
#' A dataset containing number of train passengers, generated with the
#' `pivottabler` package. It contains basic and subtotal labels in the same column.
#'
#' @family pivot table in data frame
#'
#' @format A data frame.
#' @source
#'   \url{https://CRAN.R-project.org/package=pivottabler}
#'
"df_pivottabler"
