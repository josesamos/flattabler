context("test read_excel_file")

test_that("read_excel_file works", {
  # skip("Skip Excel test")
  result <- c(NA, "Total e1", NA, "78.31", "73.68", "48.12", "78.01", "278.12",
              "70.7", "93.9", "35.01", "66", "72", "337.61", "39.66", "13.98",
              "40.92", "42.98", "100.92", "238.46", "40.72", "45.57", "47.04",
              "49.76", "43.2", "226.29", "1080.48")
  file <- system.file("extdata", "excel/set_sheets.xlsx", package = "flattabler")
  lpt <- read_excel_file(file)
  pt <- lpt[[2]][, 5]
  expect_equal(pt, result)

  lpt <- read_excel_file(file, sheetIndexes = 1:4)
  expect_equal(pt, result)

  lpt <- read_excel_file(file, sheetNames = c("M1", "M2", "M3", "M4"))
  expect_equal(pt, result)
})
