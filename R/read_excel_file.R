#' Import Excel file
#'
#' Reads sheets from an Excel file and creates a `pivot_table` object list, one
#' from each sheet. Each sheet is expected to contain a pivot table. Each line
#' in a sheet corresponds to a row in a table. The file and sheet names are
#' included as part of each object attributes.
#'
#' When multiple files or sheets are handled, the file and/or sheet names may
#' contain information associated with the pivot table, they could be the table
#' page information. In order not to lose this information, they are always
#' stored in each `pivot_table` object.
#'
#' @param sheetIndexes A vector of numbers, sheet indexes in the workbook.
#' @param sheetNames A vector of strings, sheet names.
#' @inheritParams read_excel_sheet
#'
#' @return A `pivot_table` object list.
#'
#' @family import functions
#' @seealso
#'
#' @examples
#' file <- system.file("extdata", "excel/set_sheets.xlsx", package = "flattabler")
#' lpt <- read_excel_file(file)
#'
#' lpt <- read_excel_file(file, sheetIndexes = 1:4)
#'
#' lpt <- read_excel_file(file, sheetNames = c("M1", "M2", "M3", "M4"))
#'
#' @export
read_excel_file <- function (file,
                             sheetIndexes = NULL,
                             sheetNames = NULL) {
  if (is.null(sheetIndexes) && is.null(sheetNames)) {
    sheetNames <- readxl::excel_sheets(file)
  } else if (is.null(sheetNames)) {
    sheetNames <- readxl::excel_sheets(file)[sheetIndexes]
  }
  lpt <- list()
  for (name in sheetNames) {
    ft <- suppressMessages(
      readxl::read_excel(
        file,
        sheet = name,
        col_names = FALSE,
        col_types = "text",
        trim_ws = TRUE
      )
    )
    if (nrow(ft) > 0) {
      ft <- as.data.frame(ft)
      names(ft) <- paste("X", 1:length(names(ft)), sep = "")
      lpt <- c(lpt, list(pivot_table(ft, c(file, name))))
    }
  }
  lpt
}
