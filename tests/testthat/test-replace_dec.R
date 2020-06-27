library(tidyr)

context("test replace_dec")

test_that("replace_dec works", {
  result <-
    structure(
      list(
        V1 = c(
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
        ),
        V2 = c("", "B", "b1", "b2",
               "b3", "", "b1", "b2", "b3", "", ""),
        V3 = c(
          "e1",
          "d1",
          "2.99",
          "3.89",
          "2.33",
          "9.21",
          "5.62",
          "3.82",
          "5.36",
          "14.8",
          "24.01"
        ),
        V4 = c(
          "",
          "d2",
          "1.02",
          "3.65",
          "",
          "4.67",
          "1.94",
          "7.72",
          "6.38",
          "16.04",
          "20.71"
        ),
        V5 = c(
          "Total e1",
          "",
          "4.01",
          "7.54",
          "2.33",
          "13.88",
          "7.56",
          "11.54",
          "11.74",
          "30.84",
          "44.72"
        ),
        V6 = c(
          "e2",
          "d1",
          "4.06",
          "5.55",
          "1.87",
          "11.48",
          "4.59",
          "4.78",
          "1.69",
          "11.06",
          "22.54"
        ),
        V7 = c(
          "",
          "d2",
          "1.32",
          "",
          "",
          "1.32",
          "2.13",
          "2.94",
          "1.78",
          "6.85",
          "8.17"
        ),
        V8 = c(
          "Total e2",
          "",
          "5.38",
          "5.55",
          "1.87",
          "12.8",
          "6.72",
          "7.72",
          "3.47",
          "17.91",
          "30.71"
        ),
        V9 = c(
          "Total general",
          "",
          "9.39",
          "13.09",
          "4.2",
          "26.68",
          "14.28",
          "19.26",
          "15.21",
          "48.75",
          "75.43"
        )
      ),
      page = "inst/extdata/csv/set_v_ie.csv",
      n_col_labels = 2,
      n_row_labels = 2,
      n_extract = 0,
      row.names = 2:12,
      class = c("data.frame",
                "pivot_table")
    )
  pt <-
    list_pt_ie[[1]] %>%
    remove_top(1) %>%
    define_labels(n_col = 2, n_row = 2)
  pt1 <- pt %>% replace_dec()
  expect_equal(pt1, result)

  pt2 <- pt %>% replace_dec(",")
  expect_equal(pt2, pt)
})
