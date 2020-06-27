#' Import all text files in a folder
#'
#' Reads all text files in a folder and creates a list of pivot_table objects,
#' one from each file. Each file is expected to contain a pivot table. Each line
#' in a file corresponds to a row in a table; within each row, columns are
#' defined by a separator character. File name is included as part of each
#' object attributes.
#'
#' When multiple files are handled, the file name may contain information
#' associated with the pivot table, it could be the table page information. In
#' order not to lose this information, it is always stored in each pivot_table
#' object.
#'
#' @param folder A string, folder name.
#' @inheritParams read_text_file
#'
#' @return A pivot_table object list.
#' @export
#'
#' @examples
#' folder <- system.file("extdata", "csvfolder", package = "flattabler")
#' lpt <- read_text_folder(folder)
#'
read_text_folder <-
  function (folder,
            sep = ';',
            encoding = "UTF-8") {
    lf <- list.files(path = folder, full.names = TRUE)
    do.call(list, lapply(lf, read_text_file, sep = sep, encoding = encoding))
  }
