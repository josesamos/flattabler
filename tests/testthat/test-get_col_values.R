context("test get_col_values")

test_that("get_col_values works", {
  result <-
    sort(
      c(
        "a01",
        "a02",
        "a03",
        "a04",
        "a05",
        "a06",
        "a07",
        "a08",
        "a09",
        "a10",
        "a11",
        "a12",
        "a13",
        "a14",
        "a15",
        "a16",
        "a17",
        "a18",
        "a19",
        "a20",
        "b1",
        "b2",
        "b3",
        "b4",
        "Total general"
      )
    )
  df <- get_col_values(list_pt_compact, start_row = 4)
  labels <- sort(unique(df$label))
  expect_equal(labels, result)
})
