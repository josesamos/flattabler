test_that("read_excel_sheet()", {
  expect_equal({
    file <-
      system.file("extdata", "excelfolder/m4.xlsx", package = "flattabler")
    pt <- read_excel_sheet(file, define_page = 0) |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      fill_values() |>
      fill_labels() |>
      remove_agg() |>
      unpivot()
    pt$value <- substr(pt$value, 1, 2)
    pt
  }, {
    pt2 <- pt_ex |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      remove_k() |>
      replace_dec() |>
      fill_values() |>
      fill_labels() |>
      remove_agg() |>
      unpivot()
    pt2$value <- substr(pt$value, 1, 2)
    pt2
  })
})

test_that("read_excel_sheet()", {
  expect_equal({
    file <-
      system.file("extdata", "excelfolder/m4.xlsx", package = "flattabler")
    pt <- read_excel_sheet(file, sheetName = "Hoja2", define_page = 0) |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      fill_values() |>
      fill_labels() |>
      remove_agg() |>
      unpivot()
    pt$value <- substr(pt$value, 1, 2)
    pt
  }, {
    pt2 <- pt_ex |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      remove_k() |>
      replace_dec() |>
      fill_values() |>
      fill_labels() |>
      remove_agg() |>
      unpivot()
    pt2$value <- substr(pt$value, 1, 2)
    pt2
  })
})

test_that("read_excel_file()", {
  expect_equal({
    file <- system.file("extdata", "excel/set_sheets.xlsx", package = "flattabler")
    lpt <- read_excel_file(file, define_page = 2)
    pt <- lpt[[1]] |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      fill_values() |>
      fill_labels() |>
      remove_agg() |>
      unpivot()
    pt$value <- substr(pt$value, 1, 2)
    pt
  }, {
    pt2 <- pt_ex |>
      set_page(page = "M4") |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      remove_k() |>
      replace_dec() |>
      fill_values() |>
      fill_labels() |>
      remove_agg() |>
      unpivot()
    pt2$value <- substr(pt$value, 1, 2)
    pt2
  })
})


test_that("read_excel_file()", {
  expect_equal({
    file <- system.file("extdata", "excel/set_sheets.xlsx", package = "flattabler")
    lpt <- read_excel_file(file, sheetNames = c("M1", "M2", "M3", "M4"), define_page = 2)
    pt <- lpt[[4]] |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      fill_values() |>
      fill_labels() |>
      remove_agg() |>
      unpivot()
    pt$value <- substr(pt$value, 1, 2)
    pt
  }, {
    pt2 <- pt_ex |>
      set_page(page = "M4") |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      remove_k() |>
      replace_dec() |>
      fill_values() |>
      fill_labels() |>
      remove_agg() |>
      unpivot()
    pt2$value <- substr(pt$value, 1, 2)
    pt2
  })
})


test_that("read_excel_folder()", {
  expect_equal({
    folder <- system.file("extdata", "excelfolder", package = "flattabler")
    lpt <- read_excel_folder(folder, define_page = 0)
    pt <- lpt[[4]] |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      fill_values() |>
      fill_labels() |>
      remove_agg() |>
      unpivot()
    pt$value <- substr(pt$value, 1, 2)
    pt
  }, {
    pt2 <- pt_ex |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      remove_k() |>
      replace_dec() |>
      fill_values() |>
      fill_labels() |>
      remove_agg() |>
      unpivot()
    pt2$value <- substr(pt$value, 1, 2)
    pt2
  })
})


test_that("read_excel_folder()", {
  expect_equal({
    folder <- system.file("extdata", "excelfolder", package = "flattabler")
    lpt <- read_excel_folder(folder, allSheets = TRUE, define_page = 0)
    pt <- lpt[[4]] |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      fill_values() |>
      fill_labels() |>
      remove_agg() |>
      unpivot()
    pt$value <- substr(pt$value, 1, 2)
    pt
  }, {
    pt2 <- pt_ex |>
      remove_top(1) |>
      define_labels(n_col = 2, n_row = 2) |>
      remove_k() |>
      replace_dec() |>
      fill_values() |>
      fill_labels() |>
      remove_agg() |>
      unpivot()
    pt2$value <- substr(pt$value, 1, 2)
    pt2
  })
})
