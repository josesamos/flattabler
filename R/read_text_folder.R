

#' Title
#'
#' @param folder
#' @param sep
#'
#' @return
#' @export
#'
#' @examples
read_text_folder <- function (folder, sep = ';') {
  lf <- list.files(path = folder, full.names = TRUE)
  do.call(list, lapply(lf, read_text_file, sep))
}
