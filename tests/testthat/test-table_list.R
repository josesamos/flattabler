test_that("divide() divides a df in a df list", {
  expect_equal({
    divide(df_set_h_v)[[1]]
  }, pt_ex)
})

test_that("divide() divides a df in a df list", {
  expect_equal({
    divide(df_set_h)[[1]]
  }, pt_ex)
})

test_that("divide() divides a df in a df list", {
  expect_equal({
    divide(df_set_v)[[1]]
  }, pt_ex)
})

test_that("flatten_table_list() flats a list of pivot tables", {
  expect_equal({
    f <- function(pt) {
      pt |>
        set_page(1, 1) |>
        remove_top(1) |>
        define_labels(n_col = 2, n_row = 2) |>
        remove_k() |>
        replace_dec() |>
        fill_values() |>
        fill_labels() |>
        remove_agg() |>
        unpivot()
    }

    lpt <- divide(df_set_h_v)
    flatten_table_list(lpt, f)
  }, ft_set)
})

test_that("get_col_values() gets values of a column in a list of tables", {
  expect_equal({
    lpt <- divide(df_set_h_v)
    get_col_values(lpt, col = 1, start_row = 4)
  }, structure(
    list(
      label = c(
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
        "Total general",
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
        "",
        "Total b3",
        "b4",
        "",
        "",
        "",
        "",
        "Total b4",
        "Total general",
        "b1",
        "",
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
        "",
        "Total b3",
        "b4",
        "",
        "",
        "",
        "",
        "Total b4",
        "Total general",
        "b1",
        "",
        "",
        "",
        "",
        "Total b1",
        "b4",
        "",
        "",
        "",
        "",
        "Total b4",
        "Total general"
      ),
      table = c(
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        2L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        3L,
        4L,
        4L,
        4L,
        4L,
        4L,
        4L,
        4L,
        4L,
        4L,
        4L,
        4L,
        4L,
        4L
      )
    ),
    class = "data.frame",
    row.names = c(NA,-85L)
  ))
})
