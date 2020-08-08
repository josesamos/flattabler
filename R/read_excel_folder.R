#' Import one sheet from each Excel file in a folder
#'
#' Reads one sheet from each of the Excel files in a folder and creates a list
#' of `pivot_table` objects, one from each sheet or, which is the same in this
#' case, one from each file. Each sheet is expected to contain a pivot table.
#' Each line in a file corresponds to a row in a table. File and sheet names are
#' included as part of each object attributes.
#'
#' When multiple files or sheets are handled, the file and/or sheet names may
#' contain information associated with the pivot table, they could be the table
#' page information. In order not to lose this information, they are always
#' stored in each `pivot_table` object.
#'
#' @param folder A string, folder name.
#' @inheritParams read_excel_sheet
#'
#' @return A `pivot_table` object list.
#'
#' @family import functions
#' @seealso
#'
#' @examples
#' folder <- system.file("extdata", "excelfolder", package = "flattabler")
#' # lpt <- read_excel_folder(folder)
#'
#' @export
read_excel_folder <- function (folder,
                               sheetIndex = 1,
                               sheetName = NULL,
                               encoding = "UTF-8",
                               password = NULL) {
  lf <- list.files(path = folder, full.names = TRUE)
  do.call(list,
          lapply(
            lf,
            read_excel_sheet,
            sheetIndex = sheetIndex,
            sheetName = sheetName,
            encoding = encoding,
            password = password
          ))
}
