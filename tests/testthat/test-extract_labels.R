
context("test extract_labels")

test_that("extract_labels works", {
  result <-
    structure(
      list(
        V2_1 = c(
          "",
          "",
          "",
          "b1",
          "",
          "",
          "",
          "",
          "b2",
          "",
          "",
          "",
          "",
          "",
          "b3",
          "",
          "",
          "",
          "",
          "b4",
          "",
          "",
          "",
          "",
          "",
          "Total general"
        ),
        V2 = c(
          "M4",
          "",
          "Etiquetas de fila",
          "",
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
          ""
        )
      ),
      class = c("data.frame", "pivot_table"),
      row.names = c(NA,
                    26L)
    )
  pt <- pt_m4_compact |>
    extract_labels(col = 1,
                   labels = c("b1", "b2", "b3", "b4", "Total general"))
  expect_equal(pt[, c(1, 2)], result)
})
