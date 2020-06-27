library(tidyr)

context("test remove_agg")

test_that("remove_agg works", {
  result <-
    structure(
      list(
        V1 = c("", "A", "a1", "", "", "a2", "", ""),
        V2 = c("",
               "B", "b1", "b2", "b3", "b1", "b2", "b3"),
        V3 = c("e1", "d1",
               "2,99", "3,89", "2,33", "5,62", "3,82", "5,36"),
        V4 = c("", "d2",
               "1,02", "3,65", "", "1,94", "7,72", "6,38"),
        V6 = c("e2", "d1",
               "4,06", "5,55", "1,87", "4,59", "4,78", "1,69"),
        V7 = c("", "d2",
               "1,32", "", "", "2,13", "2,94", "1,78")
      ),
      class = c("data.frame",
                "pivot_table"),
      row.names = c(2L, 3L, 4L, 5L, 6L, 8L, 9L, 10L),
      page = "inst/extdata/csv/set_v_ie.csv",
      n_col_labels = 2,
      n_row_labels = 2,
      n_extract = 0
    )
  pt <-
    list_pt_ie[[1]] %>%
    remove_top(1) %>%
    define_labels(n_col = 2, n_row = 2) %>%
    remove_agg()
  expect_equal(pt, result)
})
