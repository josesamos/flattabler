
context("test page")

test_that("page works", {
  expect_equal(get_page(pt_m4), "inst/extdata/csvfolder/m4.csv")

  pt <- pt_m4 |> set_page(1, 1)
  expect_equal(get_page(pt), "M4")

  pt <- pt_m4 |> set_page(page = "test")
  expect_equal(get_page(pt), "test")
})
