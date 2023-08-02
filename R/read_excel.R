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
#' @seealso \code{\link{pivot_table}}
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
#' @seealso \code{\link{pivot_table}}
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
#' @seealso \code{\link{pivot_table}}
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
