

#' Title
#'
#' @param file
#' @param sheetIndexes
#' @param sheetNames
#' @param password
#'
#' @return
#' @export
#'
#' @examples
read_excel_file <- function (file,
                             sheetIndexes = c(),
                             sheetNames = c(),
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
        encoding = "UTF-8",
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
