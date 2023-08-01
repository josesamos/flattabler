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
      n_row_labels = 0,
      n_extract = 0
    ),
    class = "pivot_table"
  ))
})

test_that("pivot_table() creates a pivot_table object", {
  expect_equal({
    df <- data.frame(unclass(pt_m4)[c(1:7)])
    pivot_table(df)
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
          ),
          V2 = c(
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
          V3 = c(
            "E",
            "e2",
            "d3",
            "70,40",
            "674,31",
            "421,08",
            "96,00",
            "1.261,79",
            "924,80",
            "1.058,40",
            "791,04",
            "4.698,00",
            "150,00",
            "7.622,24",
            "658,56",
            "92,00",
            "2.043,00",
            "393,96",
            "3.187,52",
            "263,13",
            "69,66",
            "346,00",
            "1.399,68",
            "34,88",
            "2.113,35",
            "14.184,90"
          ),
          V4 = c(
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
          V5 = c(
            "",
            "",
            "d5",
            "",
            "",
            "64,68",
            "545,28",
            "609,96",
            "73,50",
            "139,65",
            "",
            "",
            "",
            "213,15",
            "148,48",
            "",
            "544,18",
            "",
            "692,66",
            "489,00",
            "101,50",
            "124,74",
            "43,36",
            "83,00",
            "841,60",
            "2.357,37"
          ),
          V6 = c(
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
          ),
          V7 = c(
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
        class = "data.frame",
        row.names = c(NA,-26L)
      ),
      page = character(0),
      n_col_labels = 0,
      n_row_labels = 0,
      n_extract = 0
    ),
    class = "pivot_table"
  ))
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
    df <- data.frame(unclass(pt_m4)[c(1:7)])
    pt <- pivot_table(df)
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
      n_row_labels = 2,
      n_extract = 0
    ),
    class = "pivot_table"
  ))
})
