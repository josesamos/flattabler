library(tidyr)

context("test read_text_file")

test_that("read_text_file works", {
  file <- system.file("extdata", "csv/ine2871.csv", package = "flattabler")
  pt <- read_text_file(file)
  pt <- pt %>% set_page(page = get_page(pt_ine2871))
  expect_equal(pt, pt_ine2871)
})
