
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flattabler

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/josesamos/flattabler.svg?branch=master)](https://travis-ci.com/josesamos/flattabler)
<!-- badges: end -->

Pivot tables are generally used to present raw and summary data. They
are generated from spreadsheets and, more recently, also from R
([`pivottabler`](https://CRAN.R-project.org/package=pivottabler)).

If we generate pivot tables from our own data, `flattabler` package is
not necessary. But, if we get data in pivot table format and need to
represent or analyse it using another tool, this package can be very
helpful: It can save us several hours of programming or manual
transformation.

`flattabler` package offers a set of operations that allow us to
transform one or more pivot tables into a flat table.

## Installation

You can install the released version of `flattabler` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("flattabler")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("josesamos/flattabler")
```

## Example

A pivot table contains label rows and columns, and an array of values,
usually numeric data. It can contain additional information, such as
table header or footer.

|      V1       | V2  |  V3   |  V4   |    V5    |  V6   |  V7  |    V8    |      V9       |
|:-------------:|:---:|:-----:|:-----:|:--------:|:-----:|:----:|:--------:|:-------------:|
|      M1       |     |   E   |   D   |          |       |      |          |               |
|               |     |  e1   |       | Total e1 |  e2   |      | Total e2 | Total general |
|       A       |  B  |  d1   |  d2   |          |  d1   |  d2  |          |               |
|      a1       | b1  | 2,99  | 1,02  |   4,01   | 4,06  | 1,32 |   5,38   |     9,39      |
|               | b2  | 3,89  | 3,65  |   7,54   | 5,55  |      |   5,55   |     13,09     |
|               | b3  | 2,33  |       |   2,33   | 1,87  |      |   1,87   |      4,2      |
|   Total a1    |     | 9,21  | 4,67  |  13,88   | 11,48 | 1,32 |   12,8   |     26,68     |
|      a2       | b1  | 5,62  | 1,94  |   7,56   | 4,59  | 2,13 |   6,72   |     14,28     |
|               | b2  | 3,82  | 7,72  |  11,54   | 4,78  | 2,94 |   7,72   |     19,26     |
|               | b3  | 5,36  | 6,38  |  11,74   | 1,69  | 1,78 |   3,47   |     15,21     |
|   Total a2    |     | 14,8  | 16,04 |  30,84   | 11,06 | 6,85 |  17,91   |     48,75     |
| Total general |     | 24,01 | 20,71 |  44,72   | 22,54 | 8,17 |  30,71   |     75,43     |

The transformation to obtain a flat table from the pivot table using
`flattabler` package is as follows:

``` r
library(flattabler)

ft <- pt |>
  set_page(1, 1) |>
  define_labels(n_col = 2, n_row = 2) |>
  remove_top(1) |>
  fill_labels() |>
  remove_agg() |>
  fill_values() |>
  remove_k() |>
  replace_dec() |>
  unpivot()
```

The result obtained is as follows:

| page | col1 | col2 | row1 | row2 | value |
|:----:|:----:|:----:|:----:|:----:|:-----:|
|  M1  |  a1  |  b1  |  e1  |  d1  | 2.99  |
|  M1  |  a1  |  b1  |  e1  |  d2  | 1.02  |
|  M1  |  a1  |  b1  |  e2  |  d1  | 4.06  |
|  M1  |  a1  |  b1  |  e2  |  d2  | 1.32  |
|  M1  |  a1  |  b2  |  e1  |  d1  | 3.89  |
|  M1  |  a1  |  b2  |  e1  |  d2  | 3.65  |
|  M1  |  a1  |  b2  |  e2  |  d1  | 5.55  |
|  M1  |  a1  |  b3  |  e1  |  d1  | 2.33  |
|  M1  |  a1  |  b3  |  e2  |  d1  | 1.87  |
|  M1  |  a2  |  b1  |  e1  |  d1  | 5.62  |
|  M1  |  a2  |  b1  |  e1  |  d2  | 1.94  |
|  M1  |  a2  |  b1  |  e2  |  d1  | 4.59  |
|  M1  |  a2  |  b1  |  e2  |  d2  | 2.13  |
|  M1  |  a2  |  b2  |  e1  |  d1  | 3.82  |
|  M1  |  a2  |  b2  |  e1  |  d2  | 7.72  |
|  M1  |  a2  |  b2  |  e2  |  d1  | 4.78  |
|  M1  |  a2  |  b2  |  e2  |  d2  | 2.94  |
|  M1  |  a2  |  b3  |  e1  |  d1  | 5.36  |
|  M1  |  a2  |  b3  |  e1  |  d2  | 6.38  |
|  M1  |  a2  |  b3  |  e2  |  d1  | 1.69  |
|  M1  |  a2  |  b3  |  e2  |  d2  | 1.78  |

The table above is a flat table whose data has been obtained from the
pivot table through `flattabler`. It only contains raw data and the
labels that characterize it. An additional label has been added with the
value that identifies the pivot table, the pivot table *page*. `NA`
values have not been included.

Once we have defined the necessary transformations for a pivot table, we
can apply them to any other with the same structure. Candidate tables
can have different number of rows or columns, depending on the number of
labels, but they must have the same number of rows and columns of
labels, and the same number of header or footer rows, so that the
transformations are the same for each table.

To easily perform this operation, we define a function `f` from the
transformations, as shown below.

``` r
f <- function(pt) {
  pt |>
    set_page(1, 1) |>
    define_labels(n_col = 2, n_row = 2) |>
    remove_top(1) |>
    fill_labels() |>
    remove_agg() |>
    fill_values() |>
    remove_k() |>
    replace_dec() |>
    unpivot()
}

ft <- flatten_table_list(list_pt_ie, f)
```

In this way we can generate a flat table from a list of pivot tables.
The list of pivot tables is generated using package functions to import
them from various data sources.
