#' Import Excel file
#'
#' Reads sheets from an Excel file and creates a pivot_table object list, one
#' from each sheet. Each sheet is expected to contain a pivot table. Each line
#' in a sheet corresponds to a row in a table. The file and sheet names are
#' included as part of each object attributes.
#'
#' When multiple files or sheets are handled, the file and/or sheet names may
#' contain information associated with the pivot table, they could be the table
#' page information. In order not to lose this information, they are always
#' stored in each pivot_table object.
#'
#' @param sheetIndexes A vector of numbers, sheet indexes in the workbook.
#' @param sheetNames A vector of strings, sheet names.
#' @inheritParams read_excel_sheet
#'
#' @return A pivot_table object list.
#' @export
#'
#' @examples
#' file <- system.file("extdata", "excel/set_sheets.xlsx", package = "flattabler")
#' # lpt <- read_excel_file(file)
#'
#' # lpt <- read_excel_file(file, sheetIndexes = 1:4)
#'
#' # lpt <- read_excel_file(file, sheetNames = c("M1", "M2", "M3", "M4"))
#'
read_excel_file <- function (file,
                             sheetIndexes = c(),
                             sheetNames = c(),
                             encoding = "UTF-8",
                             password = NULL) {
  if (length(sheetIndexes) == 0 && length(sheetNames) == 0) {
    wb <- xlsx::loadWorkbook(file, password = password)
    sheetNames <- names(xlsx::getSheets(wb))
  }
  lpt <- list()
  if (length(sheetNames) > 0) {
    for (name in sheetNames) {
      sh <- xlsx::read.xlsx(
        file,
        sheetName = name,
        header = FALSE,
        colClasses = c("character"),
        encoding = encoding,
        password = password,
        stringsAsFactors = FALSE
      )
      if (!is.null(sh)) {
        lpt <- c(lpt, list(new_pivot_table(sh, c(file, name))))
      }
    }
  } else {
    wb <- xlsx::loadWorkbook(file, password = password)
    sheetNames <- names(xlsx::getSheets(wb))
    for (index in sheetIndexes) {
      sh <- xlsx::read.xlsx(
        file,
        sheetIndex = index,
        header = FALSE,
        colClasses = c("character"),
        encoding = "UTF-8",
        password = password,
        stringsAsFactors = FALSE
      )
      if (!is.null(sh)) {
        lpt <- c(lpt, list(new_pivot_table(sh, c(file, sheetNames[index]))))
      }
    }
  }
  lpt
}
