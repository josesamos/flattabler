test_that("read_text_file()", {
  expect_equal({
    file <-
      system.file("extdata", "csvfolder/m4.csv", package = "flattabler")
    read_text_file(file)
  }, pt_ex |> set_page(page = "m4.csv"))
})

test_that("read_text_folder()", {
  expect_equal({
    folder <-
      system.file("extdata", "csvfolder", package = "flattabler")
    ptl <- read_text_folder(folder)
    ptl[[4]]
  }, pt_ex |> set_page(page = "m4.csv"))
})
