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
#' @param define_page A boolean, include file name as pivot_table page definition.
#'
#' @return A `pivot_table` object.
#'
#' @family import functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' file <- system.file("extdata", "csv/ine2871.csv", package = "flattabler")
#' pt <- read_text_file(file)
#'
#' @export
read_text_file <- function(file, sep = ';', encoding = "UTF-8", define_page = TRUE) {
  if (define_page) {
    page <- basename(file)
  } else {
    page <- NULL
  }
  pivot_table(
    utils::read.table(
      file,
      sep = sep,
      header = FALSE,
      stringsAsFactors = FALSE,
      encoding = encoding,
      colClasses = c("character")
    ),
    page
  )
}

#' Import all text files in a folder
#'
#' Reads all text files in a folder and creates a list of `pivot_table` objects,
#' one from each file. Each file is expected to contain a pivot table. Each line
#' in a file corresponds to a row in a table; within each row, columns are
#' defined by a separator character. File name is included as part of each
#' object attributes.
#'
#' When multiple files are handled, the file name may contain information
#' associated with the pivot table, it could be the table page information. In
#' order not to lose this information, it is always stored in each `pivot_table`
#' object.
#'
#' @param folder A string, folder name.
#' @inheritParams read_text_file
#'
#' @return A `pivot_table` object list.
#'
#' @family import functions
#' @seealso \code{\link{pivot_table}}
#'
#' @examples
#'
#' folder <- system.file("extdata", "csvfolder", package = "flattabler")
#' lpt <- read_text_folder(folder)
#'
#' @export
read_text_folder <-
  function (folder,
            sep = ';',
            encoding = "UTF-8") {
    lf <- list.files(path = folder, full.names = TRUE)
    do.call(list, lapply(lf, read_text_file, sep = sep, encoding = encoding, define_page = TRUE))
  }
