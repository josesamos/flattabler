#' Import Excel file sheet
#'
#' Reads an Excel file sheet and creates a `pivot_table` object. The sheet is
#' expected to contain one or more pivot tables. Each line in the sheet
#' corresponds to a row in a table. The file and sheet names are included as
#' part of the object attributes.
#'
#' When multiple files or sheets are handled, the file and/or sheet names may
#' contain information associated with the pivot table, they could be the table
#' page information. In order not to lose this information, they are always
#' stored in the `pivot_table` object.

#' @param file A string, name of an Excel file.
#' @param sheetIndex A number, sheet index in the workbook.
#' @param sheetName A string, sheet name.
#'
#' @return A `pivot_table` object.
#'
#' @family import functions
#' @seealso
#'
#' @examples
#' file <- system.file("extdata", "excel/ine2871.xlsx", package = "flattabler")
#' pt <- read_excel_sheet(file)
#'
#' pt <- read_excel_sheet(file, sheetName = "tabla-2871")
#'
#' @export
read_excel_sheet <- function(file,
                             sheetIndex = 1,
                             sheetName = NULL) {
  if (is.null(sheetName)) {
    wb <- readxl::excel_sheets(file)
    sheetName <- wb[sheetIndex]
  }
  info <- c(file, sheetName)
  ft <- suppressMessages(
    readxl::read_excel(
      file,
      sheet = sheetName,
      col_names = FALSE,
      col_types = "text",
      trim_ws = TRUE
    )
  )
  names(ft) <- paste("X", 1:length(names(ft)), sep = "")
  pivot_table(as.data.frame(ft), info)
}
