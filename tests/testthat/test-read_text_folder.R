context("test read_text_folder")

test_that("read_text_folder works", {
  result <- c("", "Total e1", "", "78,31", "73,68", "48,12", "78,01", "278,12",
              "70,70", "93,90", "35,01", "66,00", "72,00", "337,61", "39,66",
              "13,98", "40,92", "42,98", "100,92", "238,46", "40,72", "45,57",
              "47,04", "49,76", "43,20", "226,29", "1.080,48")
  folder <- system.file("extdata", "csvfolder", package = "flattabler")
  lpt <- read_text_folder(folder)
  pt <- lpt[[3]][, 5]
  expect_equal(pt, result)
})
