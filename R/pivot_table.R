

#' Title
#' low-level constructor that efficiently creates new objects with the correct structure
#' @param df
#' @param page
#' @param n_col_labels
#' @param n_row_labels
#'
#' @return
#' @export
#'
#' @examples
new_pivot_table <-
  function(df = data.frame(),
           page = vector("character"),
           n_col_labels = 0,
           n_row_labels = 0) {
    # Check the type of the base object and the types of each attribute
    stopifnot(is.data.frame(df))
    stopifnot(is.vector(page))

    structure(
      df,
      class = unique(append(class(df), "pivot_table")),
      page = page,
      n_col_labels = n_col_labels,
      n_row_labels = n_row_labels
    )
  }

# validator that performs more computationally expensive checks to ensure that the object has correct values
# You don’t need a validator for very simple classes
validate_pivot_table <- function(dt) {
  for (n in seq_along(colnames(dt))) {
    if (class(dt[, n]) != "character") {
      stop("All columns of the data frame must be of type character",
           call. = FALSE)
    }
  }
  dt
}

#' Title
#' helper that provides a convenient way for others to create objects of your class
#' you can skip the helper if the class is for internal use only
#' If you expect users to also create objects, you should create a friendly helper function

#' @param df
#' @param page
#' @param n_col_labels
#' @param n_row_labels
#'
#' @return
#' @export
#'
#' @examples
pivot_table <- function(df,
                        page = vector("character"),
                        n_col_labels,
                        n_row_labels) {
  df <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  page <- as.character(page)
  new_pivot_table(df, page,
                  n_col_labels,
                  n_row_labels)
}
