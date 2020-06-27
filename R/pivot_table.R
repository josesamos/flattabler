#' pivot_table S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @param df A data frame, contains one or more pivot tables.
#' @param page A string, additional information associated with the pivot table.
#' @param n_col_labels A number, number of columns containing pivot table labels.
#' @param n_row_labels A number, number of rows containing pivot table labels.
#' @param n_extract A number, number of new columns added to the table.
#'
#' @return A pivot_table object.
#'
new_pivot_table <-
  function(df = data.frame(),
           page = vector("character"),
           n_col_labels = 0,
           n_row_labels = 0,
           n_extract = 0) {
    # Check the type of the base object and the types of each attribute
    stopifnot(is.data.frame(df))
    stopifnot(is.vector(page))

    structure(
      df,
      class = unique(append(class(df), "pivot_table")),
      page = page,
      n_col_labels = n_col_labels,
      n_row_labels = n_row_labels,
      n_extract = n_extract
    )
  }


#' pivot_table S3 class
#'
#' Creates a pivot_table object from a data frame. Additional information
#' associated with the pivot table can be indicated. Data frame data is
#' converted to character type.

#' @inheritParams new_pivot_table
#'
#' @return A pivot_table object.
#' @export
#'
#' @examples
#' df <- data.frame(unclass(pt_m4)[c(1:7)])
#' pt <- pivot_table(df)
#'
#' pt <- pivot_table(df, page = "M4")
#'
pivot_table <- function(df,
                        page = vector("character")) {
  df <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  page <- as.character(page)
  new_pivot_table(df, page,
                  n_col_labels = 0,
                  n_row_labels = 0,
                  n_extract = 0)
}
