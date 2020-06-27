library(tidyr)

context("test fill_labels")

test_that("fill_labels works", {
  input <-
    c(
      "",
      "B",
      "b1",
      "",
      "",
      "",
      "Total b1",
      "b2",
      "",
      "",
      "",
      "",
      "Total b2",
      "b3",
      "",
      "",
      "",
      "Total b3",
      "b4",
      "",
      "",
      "",
      "",
      "Total b4",
      "Total general"
    )
  result <-
    c(
      "",
      "B",
      "b1",
      "b1",
      "b1",
      "b1",
      "Total b1",
      "b2",
      "b2",
      "b2",
      "b2",
      "b2",
      "Total b2",
      "b3",
      "b3",
      "b3",
      "b3",
      "Total b3",
      "b4",
      "b4",
      "b4",
      "b4",
      "b4",
      "Total b4",
      "Total general"
    )
  expect_equal(fill_vector(input), result)

  input2 <-
    structure(
      list(
        V1 = "",
        V2 = "",
        V3 = "e2",
        V4 = "",
        V5 = "",
        V6 = "Total e2",
        V7 = "Total general"
      ),
      page = "M4",
      n_col_labels = 2,
      n_row_labels = 2,
      n_extract = 0,
      row.names = 2L,
      class = c("data.frame",
                "pivot_table")
    )
  result2 <-
    c("", "", "e2", "e2", "e2", "Total e2", "Total general")
  expect_equal(fill_vector(input2), result2)

  result3 <-
    structure(
      list(
        V1 = c("", "A"),
        V2 = c("", "B"),
        V3 = c("e1",
               "d1"),
        V4 = c("e1", "d2"),
        V5 = c("Total e1", ""),
        V6 = c("e2",
               "d1"),
        V7 = c("e2", "d2"),
        V8 = c("Total e2", ""),
        V9 = c("Total general",
               "")
      ),
      page = "inst/extdata/csv/set_v_ie.csv",
      n_col_labels = 2,
      n_row_labels = 2,
      n_extract = 0,
      row.names = 2:3,
      class = c("data.frame",
                "pivot_table")
    )
  result4 <-
    structure(
      list(
        V1 = c(
          "",
          "A",
          "a1",
          "a1",
          "a1",
          "Total a1",
          "a2",
          "a2",
          "a2",
          "Total a2",
          "Total general"
        ),
        V2 = c("", "B",
               "b1", "b2", "b3", "", "b1", "b2", "b3", "", "")
      ),
      class = c("data.frame",
                "pivot_table"),
      row.names = 2:12
    )
  pt <-
    list_pt_ie[[1]] %>%
    remove_top(1) %>%
    define_labels(n_col = 2, n_row = 2) %>%
    fill_labels()
  expect_equal(pt[c(1, 2),], result3)
  expect_equal(pt[, c(1, 2)], result4)
})
