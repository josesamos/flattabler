
#' Title
#'
#' @param folder
#' @param sheetIndex
#' @param sheetName
#'
#' @return
#' @export
#'
#' @examples
read_excel_folder <- function (folder,
                               sheetIndex = 1,
                               sheetName = NULL) {
  lf <- list.files(path = folder, full.names = TRUE)
  do.call(list, lapply(lf, read_excel_sheet, sheetIndex, sheetName))
}
