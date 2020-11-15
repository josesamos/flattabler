#' Import one sheet from each Excel file in a folder
#'
#' Reads one sheet (or all sheets) from each of the Excel files in a folder and
#' creates a list of `pivot_table` objects, one from each sheet. Each sheet is
#' expected to contain a pivot table. Each line in a file corresponds to a row
#' in a table. File and sheet names are included as part of each object
#' attributes.
#'
#' When multiple files or sheets are handled, the file and/or sheet names may
#' contain information associated with the pivot table, they could be the table
#' page information. In order not to lose this information, they are always
#' stored in each `pivot_table` object.
#'
#' @param folder A string, folder name.
#' @param allSheets A boolean.
#' @inheritParams read_excel_sheet
#'
#' @return A `pivot_table` object list.
#'
#' @family import functions
#' @seealso
#'
#' @examples
#' folder <- system.file("extdata", "excelfolder", package = "flattabler")
#' lpt <- read_excel_folder(folder)
#'
#' lpt <- read_excel_folder(folder, allSheets = TRUE)
#'
#' @export
read_excel_folder <- function (folder,
                               sheetIndex = 1,
                               sheetName = NULL,
                               allSheets = FALSE) {
  lf <- list.files(path = folder, full.names = TRUE)
  if (allSheets) {
    do.call(c,
            lapply(
              lf,
              read_excel_file,
              sheetIndexes = NULL,
              sheetNames = NULL
            ))
  } else {
    do.call(list,
            lapply(
              lf,
              read_excel_sheet,
              sheetIndex = sheetIndex,
              sheetName = sheetName
            ))
  }
}
