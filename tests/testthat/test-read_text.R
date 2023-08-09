# test_that("read_text_file()", {
#   expect_equal({
#     file <-
#       system.file("extdata", "csvfolder/m4.csv", package = "flattabler")
#     pt <- read_text_file(file)
#     pt |> set_page(page = "M4")
#   }, pt_ex |> set_page(page = "M4"))
# })
#
# test_that("read_text_folder()", {
#   expect_equal({
#     folder <-
#       system.file("extdata", "csvfolder", package = "flattabler")
#     ptl <- read_text_folder(folder)
#     ptl[[4]] |> set_page(page = "M4")
#   }, pt_ex |> set_page(page = "M4"))
# })
