

#' Import text data file
#'
#' Reads a UTF-8 text file with column separators and creates a list of metadata and a data frame with data from
#' it.
#'
#' @param file UTF-8 encoded text file whose data is to be read. Each line in
#'   the file corresponds to a row in a table.
#' @param sep Column separator character. Within each row, columns are separated
#'   by this character.
#'
#' @return A two elements list of metadata and character data frame containing the data in the file.
#' @export
#'
#' @seealso \code{\link[utils]{View}}
#'
#' @examples
read_text_file <- function(file, sep = ';') {
  new_pivot_table(
    utils::read.table(
      file,
      sep = sep,
      header = FALSE,
      stringsAsFactors = FALSE,
      encoding = "UTF-8",
      colClasses = c("character")
    ),
    file
  )
}
