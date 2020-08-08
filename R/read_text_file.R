#' Import text file
#'
#' Reads a text file and creates a `pivot_table` object. The file is expected to
#' contain one or more pivot tables. Each line in the file corresponds to a row
#' in a table; within each row, columns are defined by a separator character.
#' The file name is included as part of the object attributes.
#'
#' When multiple files are handled, the file name may contain information
#' associated with the pivot table, it could be the table page information. In
#' order not to lose this information, it is always stored in the `pivot_table`
#' object.
#'
#' @param file A string, name of a text file.
#' @param sep Column separator character.
#' @param encoding A string, encoding to be assumed for input strings.
#'
#' @return A `pivot_table` object.
#'
#' @family import functions
#' @seealso
#'
#' @examples
#' file <- system.file("extdata", "csv/ine2871.csv", package = "flattabler")
#' pt <- read_text_file(file)
#'
#' @export
read_text_file <- function(file, sep = ';', encoding = "UTF-8") {
  new_pivot_table(
    utils::read.table(
      file,
      sep = sep,
      header = FALSE,
      stringsAsFactors = FALSE,
      encoding = encoding,
      colClasses = c("character")
    ),
    file
  )
}
