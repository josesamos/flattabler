context("test read_excel_folder")

test_that("read_excel_folder works", {
  # skip("Skip Excel test")
  result <- c(NA, "Total e1", NA, NA, "41", "28", "24", "32", "125", "20",
              "39", "33", "40", "30", "162", "14", "6", "12", "14", "42", "88",
              "25", "33", "32", "28", "16", "134", "509")
  folder <- system.file("extdata", "excelfolder", package = "flattabler")
  lpt <- read_excel_folder(folder)
  pt <- lpt[[2]][, 5]
  expect_equal(pt, result)
})
