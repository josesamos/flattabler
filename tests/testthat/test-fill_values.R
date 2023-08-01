
context("test fill_values")

test_that("fill_values works", {
  result <-
    structure(
      list(
        V4 = c(
          "",
          "d2",
          "1,02",
          "3,65",
          NA,
          "4,67",
          "1,94",
          "7,72",
          "6,38",
          "16,04",
          "20,71"
        ),
        V7 = c(
          "",
          "d2",
          "1,32",
          NA,
          NA,
          "1,32",
          "2,13",
          "2,94",
          "1,78",
          "6,85",
          "8,17"
        )
      ),
      class = c("data.frame",
                "pivot_table"),
      row.names = 2:12
    )
  pt <-
    list_pt_ie[[1]] |>
    remove_top(1) |>
    define_labels(n_col = 2, n_row = 2) |>
    fill_values()
  expect_equal(pt[, c(4, 7)], result)
})
