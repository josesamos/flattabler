test_that("pivot_table() creates a pivot_table object", {
  expect_equal({
    df <- data.frame()
    pivot_table(df)
  }, structure(
    list(
      df = structure(
        list(),
        names = character(0),
        class = "data.frame",
        row.names = integer(0)
      ),
      page = character(0),
      n_col_labels = 0,
      n_row_labels = 0
    ),
    class = "pivot_table"
  ))
})

test_that("pivot_table() creates a pivot_table object", {
  expect_equal({
    pivot_table(df_ex)
  }, pt_ex)
})


test_that("set_page(), get_page() pivot_table page", {
  expect_equal({
    df <- data.frame()
    pt <- pivot_table(df)
    pt <- set_page(pt, page = "A")
    get_page(pt)
  }, "A")
})

test_that("set_page(), get_page() pivot_table page", {
  expect_equal({
    pt <- pivot_table(df_ex)
    pt <- set_page(pt, 1, 1)
    get_page(pt)
  }, "M4")
})


test_that("define_labels(), pivot_table", {
  expect_equal({
    df <- data.frame()
    pt <- pivot_table(df)
    pt <- define_labels(pt, n_col = 2, n_row = 2)
    pt
  }, structure(
    list(
      df = structure(
        list(),
        names = character(0),
        class = "data.frame",
        row.names = integer(0)
      ),
      page = character(0),
      n_col_labels = 2,
      n_row_labels = 2
    ),
    class = "pivot_table"
  ))
})

test_that("remove_rows(), pivot_table", {
  expect_equal({
    pt <- pivot_table(df_ex)
    pt |> remove_rows(seq(from = 1, to = 25, by = 2))
  }, structure(
    list(
      df = structure(
        list(
          V1 = c(
            "",
            "b1",
            "",
            "Total b1",
            "",
            "",
            "Total b2",
            "",
            "",
            "b4",
            "",
            "",
            "Total general"
          ),
          V2 = c(
            "",
            "a05",
            "a13",
            "",
            "a06",
            "a14",
            "",
            "a07",
            "a19",
            "a04",
            "a12",
            "a20",
            ""
          ),
          V3 = c(
            "e2",
            "70,40",
            "421,08",
            "1.261,79",
            "1.058,40",
            "4.698,00",
            "7.622,24",
            "92,00",
            "393,96",
            "263,13",
            "346,00",
            "34,88",
            "14.184,90"
          ),
          V4 = c(
            "",
            "1.089,00",
            "1.055,12",
            "3.491,96",
            "494,19",
            "40,96",
            "2.966,72",
            "1.466,08",
            "1.056,25",
            "204,80",
            "1.008,61",
            "261,95",
            "12.248,10"
          ),
          V5 = c(
            "",
            "",
            "64,68",
            "609,96",
            "139,65",
            "",
            "213,15",
            "",
            "",
            "489,00",
            "124,74",
            "83,00",
            "2.357,37"
          ),
          V6 = c(
            "Total e2",
            "1.159,40",
            "1.540,88",
            "5.363,71",
            "1.692,24",
            "4.738,96",
            "10.802,11",
            "1.558,08",
            "1.450,21",
            "956,93",
            "1.479,35",
            "379,83",
            "28.790,37"
          ),
          V7 = c(
            "Total general",
            "1.159,40",
            "1.540,88",
            "5.363,71",
            "1.692,24",
            "4.738,96",
            "10.802,11",
            "1.558,08",
            "1.450,21",
            "956,93",
            "1.479,35",
            "379,83",
            "28.790,37"
          )
        ),
        row.names = c(NA, 13L),
        class = "data.frame"
      ),
      page = character(0),
      n_col_labels = 0,
      n_row_labels = 0
    ),
    class = "pivot_table"
  ))
})

test_that("remove_cols(), pivot_table", {
  expect_equal({
    pt <- pivot_table(df_ex)
    pt |> remove_cols(seq(from = 1, to = 7, by = 2))
  }, structure(
    list(
      df = structure(
        list(
          V1 = c(
            "",
            "",
            "A",
            "a05",
            "a09",
            "a13",
            "a17",
            "",
            "a02",
            "a06",
            "a10",
            "a14",
            "a18",
            "",
            "a03",
            "a07",
            "a15",
            "a19",
            "",
            "a04",
            "a08",
            "a12",
            "a16",
            "a20",
            "",
            ""
          ),
          V2 = c(
            "D",
            "",
            "d4",
            "1.089,00",
            "",
            "1.055,12",
            "1.347,84",
            "3.491,96",
            "1.867,02",
            "494,19",
            "121,03",
            "40,96",
            "443,52",
            "2.966,72",
            "203,52",
            "1.466,08",
            "184,96",
            "1.056,25",
            "2.910,81",
            "204,80",
            "1.261,17",
            "1.008,61",
            "142,08",
            "261,95",
            "2.878,61",
            "12.248,10"
          ),
          V3 = c(
            "",
            "Total e2",
            "",
            "1.159,40",
            "674,31",
            "1.540,88",
            "1.989,12",
            "5.363,71",
            "2.865,32",
            "1.692,24",
            "912,07",
            "4.738,96",
            "593,52",
            "10.802,11",
            "1.010,56",
            "1.558,08",
            "2.772,14",
            "1.450,21",
            "6.790,99",
            "956,93",
            "1.432,33",
            "1.479,35",
            "1.585,12",
            "379,83",
            "5.833,56",
            "28.790,37"
          )
        ),
        class = "data.frame",
        row.names = c(NA,
                      26L)
      ),
      page = character(0),
      n_col_labels = 0,
      n_row_labels = 0
    ),
    class = "pivot_table"
  ))
})

test_that("remove_empty(), pivot_table", {
  expect_equal({
    df <- df_ex
    df[seq(from = 1, to = 25, by = 2), ] <- " "
    df[, seq(from = 1, to = 7, by = 2)] <- " "
    pt <- pivot_table(df)
    pt |> remove_empty()
  }, structure(
    list(
      df = structure(
        list(
          V1 = c(
            NA,
            "a05",
            "a13",
            NA,
            "a06",
            "a14",
            NA,
            "a07",
            "a19",
            "a04",
            "a12",
            "a20",
            NA
          ),
          V2 = c(
            NA,
            "1.089,00",
            "1.055,12",
            "3.491,96",
            "494,19",
            "40,96",
            "2.966,72",
            "1.466,08",
            "1.056,25",
            "204,80",
            "1.008,61",
            "261,95",
            "12.248,10"
          ),
          V3 = c(
            "Total e2",
            "1.159,40",
            "1.540,88",
            "5.363,71",
            "1.692,24",
            "4.738,96",
            "10.802,11",
            "1.558,08",
            "1.450,21",
            "956,93",
            "1.479,35",
            "379,83",
            "28.790,37"
          )
        ),
        row.names = c(NA, 13L),
        class = "data.frame"
      ),
      page = character(0),
      n_col_labels = 0,
      n_row_labels = 0
    ),
    class = "pivot_table"
  ))
})

test_that("remove_top(), pivot_table", {
  expect_equal({
    pt_ex |> remove_top(25)
  }, structure(
    list(
      df = structure(
        list(
          V1 = "Total general",
          V2 = "",
          V3 = "14.184,90",
          V4 = "12.248,10",
          V5 = "2.357,37",
          V6 = "28.790,37",
          V7 = "28.790,37"
        ),
        row.names = 1L,
        class = "data.frame"
      ),
      page = character(0),
      n_col_labels = 0,
      n_row_labels = 0
    ),
    class = "pivot_table"
  ))
})


test_that("remove_top(), pivot_table", {
  expect_equal({
    pt_ex |> remove_top(30)
  }, structure(
    list(
      df = structure(
        list(
          V1 = character(0),
          V2 = character(0),
          V3 = character(0),
          V4 = character(0),
          V5 = character(0),
          V6 = character(0),
          V7 = character(0)
        ),
        row.names = integer(0),
        class = "data.frame"
      ),
      page = character(0),
      n_col_labels = 0,
      n_row_labels = 0
    ),
    class = "pivot_table"
  ))
})

test_that("remove_bottom(), pivot_table", {
  expect_equal({
    pt_ex |> remove_bottom(25)
  }, structure(
    list(
      df = structure(
        list(
          V1 = "M4",
          V2 = "",
          V3 = "E",
          V4 = "D",
          V5 = "",
          V6 = "",
          V7 = ""
        ),
        row.names = 1L,
        class = "data.frame"
      ),
      page = character(0),
      n_col_labels = 0,
      n_row_labels = 0
    ),
    class = "pivot_table"
  ))
})


test_that("remove_bottom(), pivot_table", {
  expect_equal({
    pt_ex |> remove_bottom(30)
  }, structure(
    list(
      df = structure(
        list(
          V1 = character(0),
          V2 = character(0),
          V3 = character(0),
          V4 = character(0),
          V5 = character(0),
          V6 = character(0),
          V7 = character(0)
        ),
        row.names = integer(0),
        class = "data.frame"
      ),
      page = character(0),
      n_col_labels = 0,
      n_row_labels = 0
    ),
    class = "pivot_table"
  ))
})


test_that("remove_left(), pivot_table", {
  expect_equal({
    pt_ex |> remove_left(6)
  }, structure(
    list(
      df = structure(
        list(
          V1 = c(
            "",
            "Total general",
            "",
            "1.159,40",
            "674,31",
            "1.540,88",
            "1.989,12",
            "5.363,71",
            "2.865,32",
            "1.692,24",
            "912,07",
            "4.738,96",
            "593,52",
            "10.802,11",
            "1.010,56",
            "1.558,08",
            "2.772,14",
            "1.450,21",
            "6.790,99",
            "956,93",
            "1.432,33",
            "1.479,35",
            "1.585,12",
            "379,83",
            "5.833,56",
            "28.790,37"
          )
        ),
        row.names = c(NA, 26L),
        class = "data.frame"
      ),
      page = character(0),
      n_col_labels = 0,
      n_row_labels = 0
    ),
    class = "pivot_table"
  ))
})


test_that("remove_left(), pivot_table", {
  expect_equal({
    pt_ex |> remove_left(10)
  }, structure(
    list(
      df = structure(
        list(),
        names = character(0),
        class = "data.frame",
        row.names = c(NA,
                      26L)
      ),
      page = character(0),
      n_col_labels = 0,
      n_row_labels = 0
    ),
    class = "pivot_table"
  ))
})

test_that("remove_right(), pivot_table", {
  expect_equal({
    pt_ex |> remove_right(6)
  }, structure(
    list(
      df = structure(
        list(
          V1 = c(
            "M4",
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
        ),
        row.names = c(NA, 26L),
        class = "data.frame"
      ),
      page = character(0),
      n_col_labels = 0,
      n_row_labels = 0
    ),
    class = "pivot_table"
  ))
})


test_that("remove_right(), pivot_table", {
  expect_equal({
    pt_ex |> remove_right(10)
  }, structure(
    list(
      df = structure(
        list(),
        names = character(0),
        class = "data.frame",
        row.names = c(NA,
                      26L)
      ),
      page = character(0),
      n_col_labels = 0,
      n_row_labels = 0
    ),
    class = "pivot_table"
  ))
})

test_that("fill_labels(), pivot_table", {
  expect_equal({
    pt <- pt_ex |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      fill_labels(down = TRUE, right = TRUE)
    c(pt$df[, 1], t(pt$df[1,]))
  }, c(
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
    "Total general",
    "",
    "",
    "e2",
    "e2",
    "e2",
    "Total e2",
    "Total general"
  ))
})


test_that("fill_labels(), pivot_table", {
  expect_equal({
    pt <- pt_ex |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      fill_labels(down = FALSE, right = FALSE)
    c(pt$df[, 1], t(pt$df[1,]))
  }, c(
    "B",
    "B",
    "b1",
    "Total b1",
    "Total b1",
    "Total b1",
    "Total b1",
    "b2",
    "Total b2",
    "Total b2",
    "Total b2",
    "Total b2",
    "Total b2",
    "b3",
    "Total b3",
    "Total b3",
    "Total b3",
    "Total b3",
    "b4",
    "Total b4",
    "Total b4",
    "Total b4",
    "Total b4",
    "Total b4",
    "Total general",
    "B",
    "e2",
    "e2",
    "Total e2",
    "Total e2",
    "Total e2",
    "Total general"
  ))
})

test_that("remove_agg(), pivot_table", {
  expect_equal({
    pt <- pt_ex |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      remove_agg()
    c(pt$df[, 1], t(pt$df[1,]))
  }, c(
    "",
    "B",
    "b1",
    "",
    "",
    "",
    "b2",
    "",
    "",
    "",
    "",
    "b3",
    "",
    "",
    "",
    "b4",
    "",
    "",
    "",
    "",
    "",
    "",
    "e2",
    "",
    ""
  ))
})


test_that("fill_values(), pivot_table", {
  expect_equal({
    pt <- pt_ex |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      fill_values()
    c(pt$df[, 5])
  }, c(
    "",
    "d5",
    NA,
    NA,
    "64,68",
    "545,28",
    "609,96",
    "73,50",
    "139,65",
    NA,
    NA,
    NA,
    "213,15",
    "148,48",
    NA,
    "544,18",
    NA,
    "692,66",
    "489,00",
    "101,50",
    "124,74",
    "43,36",
    "83,00",
    "841,60",
    "2.357,37"
  ))
})


test_that("remove_k(), pivot_table", {
  expect_equal({
    pt <-
      pt_ex |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2)
    pt$df[4, 4] <- "1.000.000,00"
    pt <- pt |>
      remove_k()
    c(pt$df[, 4])
  }, c(
    "",
    "d4",
    "1089,00",
    "1000000,00",
    "1055,12",
    "1347,84",
    "3491,96",
    "1867,02",
    "494,19",
    "121,03",
    "40,96",
    "443,52",
    "2966,72",
    "203,52",
    "1466,08",
    "184,96",
    "1056,25",
    "2910,81",
    "204,80",
    "1261,17",
    "1008,61",
    "142,08",
    "261,95",
    "2878,61",
    "12248,10"
  ))
})

test_that("replace_dec(), pivot_table", {
  expect_equal({
    pt <-
      pt_ex |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      replace_dec()
    c(pt$df[, 4])
  }, c(
    "",
    "d4",
    "1.089.00",
    "",
    "1.055.12",
    "1.347.84",
    "3.491.96",
    "1.867.02",
    "494.19",
    "121.03",
    "40.96",
    "443.52",
    "2.966.72",
    "203.52",
    "1.466.08",
    "184.96",
    "1.056.25",
    "2.910.81",
    "204.80",
    "1.261.17",
    "1.008.61",
    "142.08",
    "261.95",
    "2.878.61",
    "12.248.10"
  ))
})

test_that("extract_labels(), pivot_table", {
  expect_equal({
    pivot_table(df_ex_compact) |>
      extract_labels(col = 1,
                     labels = c("b1", "b2", "b3", "b4", "Total general"))
  }, pf_ex_compact)
})

test_that("extract_labels(), pivot_table", {
  expect_equal({
    a_df <- cbind("A", df_ex_compact)
    pivot_table(a_df) |>
      extract_labels(col = 2,
                     labels = c("b1", "b2", "b3", "b4", "Total general")) |>
      remove_cols(1)
  }, pf_ex_compact)
})


test_that("divide() divides a pt in a pt list", {
  expect_equal({
    pt <- pivot_table(df_set_h_v)
    lpt <- pt |> divide()
    lpt[[1]]
  }, pt_ex)
})

test_that("divide() divides a pt in a pt list", {
  expect_equal({
    pt <- pivot_table(df_set_h)
    lpt <- pt |> divide()
    lpt[[1]]
  }, pt_ex)
})

test_that("divide() divides a pt in a pt list", {
  expect_equal({
    pt <- pivot_table(df_set_v)
    lpt <- pt |> divide()
    lpt[[1]]
  }, pt_ex)
})




test_that("unpivot(), pivot_table", {
  expect_equal({
    pt_ex |>
      set_page(1, 1) |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      remove_k() |>
      replace_dec() |>
      fill_values() |>
      fill_labels() |>
      remove_agg() |>
      unpivot(include_page = TRUE,
              na.rm = TRUE)
  }, ft_ex)
})

test_that("unpivot(), pivot_table", {
  expect_equal({
    pt_ex |>
      set_page(1, 1) |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      remove_k() |>
      replace_dec() |>
      fill_values() |>
      fill_labels() |>
      remove_agg() |>
      unpivot(include_page = FALSE,
              na.rm = FALSE)
  }, ft_ex_v2)
})

