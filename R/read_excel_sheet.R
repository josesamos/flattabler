
#' Title
#'
#' @param file
#' @param sheetIndex
#' @param sheetName
#' @param password
#'
#' @return
#' @export
#'
#' @examples
read_excel_sheet <- function(file,
                             sheetIndex = 1,
                             sheetName = NULL,
                             password = NULL) {
  info <- file
  if (!is.null(sheetName)) {
    info <- c(file, sheetName)
  }
  new_pivot_table(xlsx::read.xlsx(file,
                                  sheetIndex,
                                  sheetName,
                                  header = FALSE,
                                  colClasses = c("character"),
                                  encoding = "UTF-8",
                                  password = password,
                                  stringsAsFactors = FALSE),
                  info)
}
