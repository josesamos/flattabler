context("test pivot_table")

test_that("pivot_table works", {
  df <- data.frame(unclass(list_pt_ie[[1]])[c(1:9)])
  pt <- pivot_table(df, page = attr(list_pt_ie[[1]],"page"))
  expect_equal(pt, list_pt_ie[[1]])
})
