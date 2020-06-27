library(tidyr)

context("test remove")

test_that("remove works", {
  result <-
    structure(
      list(
        V1 = "A",
        V2 = "B",
        V3 = "d1",
        V4 = "d2",
        V5 = "",
        V6 = "d1",
        V7 = "d2",
        V8 = "",
        V9 = ""
      ),
      page = "inst/extdata/csv/set_v_ie.csv",
      n_col_labels = 0,
      n_row_labels = 0,
      n_extract = 0,
      row.names = 3L,
      class = c("data.frame",
                "pivot_table")
    )
  pt <- list_pt_ie[[1]] %>% remove_rows(c(-3))
  expect_equal(pt, result)

  result <-
    structure(
      list(
        `pt[, -c]` = c(
          "E",
          "e1",
          "d1",
          "2,99",
          "3,89",
          "2,33",
          "9,21",
          "5,62",
          "3,82",
          "5,36",
          "14,8",
          "24,01"
        )
      ),
      row.names = c(NA,
                    12L),
      class = c("data.frame", "pivot_table"),
      page = "inst/extdata/csv/set_v_ie.csv",
      n_col_labels = 0,
      n_row_labels = 0,
      n_extract = 0
    )
  pt <- list_pt_ie[[1]] %>% remove_cols(c(-3))
  expect_equal(pt, result)

  pt <- pt_ine2871 %>% remove_empty()
  expect_equal(nrow(pt), 184)
  expect_equal(ncol(pt), 70)

  result <-
    structure(
      list(
        V1 = "Total general",
        V2 = "",
        V3 = "24,01",
        V4 = "20,71",
        V5 = "44,72",
        V6 = "22,54",
        V7 = "8,17",
        V8 = "30,71",
        V9 = "75,43"
      ),
      page = "inst/extdata/csv/set_v_ie.csv",
      n_col_labels = 0,
      n_row_labels = 0,
      n_extract = 0,
      row.names = 12L,
      class = c("data.frame",
                "pivot_table")
    )
  pt <- list_pt_ie[[1]] %>% remove_top(11)
  expect_equal(pt, result)

  result <-
    structure(
      list(
        V1 = "M1",
        V2 = "",
        V3 = "E",
        V4 = "D",
        V5 = "",
        V6 = "",
        V7 = "",
        V8 = "",
        V9 = ""
      ),
      page = "inst/extdata/csv/set_v_ie.csv",
      n_col_labels = 0,
      n_row_labels = 0,
      n_extract = 0,
      row.names = 1L,
      class = c("data.frame",
                "pivot_table")
    )
  pt <- list_pt_ie[[1]] %>% remove_bottom(11)
  expect_equal(pt, result)

  result <-
    structure(
      list(
        `pt[, c(-1:-n)]` = c(
          "",
          "Total general",
          "",
          "9,39",
          "13,09",
          "4,2",
          "26,68",
          "14,28",
          "19,26",
          "15,21",
          "48,75",
          "75,43"
        )
      ),
      row.names = c(NA, 12L),
      class = c("data.frame", "pivot_table"),
      page = "inst/extdata/csv/set_v_ie.csv",
      n_col_labels = 0,
      n_row_labels = 0,
      n_extract = 0
    )
  pt <- list_pt_ie[[1]] %>% remove_left(8)
  expect_equal(pt, result)

  result <-
    structure(
      list(
        `pt[, c(-first_col:-n_cols)]` = c(
          "M1",
          "",
          "A",
          "a1",
          "",
          "",
          "Total a1",
          "a2",
          "",
          "",
          "Total a2",
          "Total general"
        )
      ),
      row.names = c(NA, 12L),
      class = c("data.frame", "pivot_table"),
      page = "inst/extdata/csv/set_v_ie.csv",
      n_col_labels = 0,
      n_row_labels = 0,
      n_extract = 0
    )
  pt <- list_pt_ie[[1]] %>% remove_right(8)
  expect_equal(pt, result)
})
