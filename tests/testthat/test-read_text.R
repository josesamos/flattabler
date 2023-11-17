test_that("read_text_file()", {
  expect_equal({
    file <-
      system.file("extdata", "m4.csv", package = "flattabler")
    pt <- read_text_file(file)
    r <- pt |> set_page(page = "M4")
    names(r)
    c("df", "page", "n_col_labels", "n_row_labels")
    }, c("df", "page", "n_col_labels", "n_row_labels"))
})

test_that("read_text_folder()", {
  expect_equal({
    folder <-
      system.file("extdata", "csvfolder", package = "flattabler")
    ptl <- read_text_folder(folder)
    r <- ptl[[4]] |> set_page(page = "M4")
    names(r)
    c("df", "page", "n_col_labels", "n_row_labels")
  }, c("df", "page", "n_col_labels", "n_row_labels"))
})
