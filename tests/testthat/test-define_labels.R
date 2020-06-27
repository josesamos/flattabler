library(tidyr)

context("test define_labels")

test_that("define_labels works", {
  pt <- pt_m4 %>% define_labels(n_col = 2, n_row = 2)
  apt <- attributes(pt)
  expect_equal(apt$n_col_labels, 2)
  expect_equal(apt$n_row_labels, 2)
})
