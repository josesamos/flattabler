library(tidyr)

context("test divide")

test_that("divide works", {
  df <-
    data.frame(lapply(pt_set_h_v, function(x)
      dplyr::na_if(stringr::str_trim(x), "")))
  x <- spacer_rows(df)
  y <- spacer_columns(df)
  expect_equal(x, c(1, 29, 58))
  expect_equal(y, c(1, 21))

  lpt <- pt_set_h_v %>% divide()
  expect_equal(lpt, list_pt)
})
