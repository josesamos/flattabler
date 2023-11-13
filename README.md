
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flattabler <a href="https://josesamos.github.io/flattabler/"><img src="man/figures/logo.png" align="right" height="139" alt="flattabler website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/flattabler)](https://CRAN.R-project.org/package=flattabler)
[![R-CMD-check](https://github.com/josesamos/flattabler/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/josesamos/flattabler/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/josesamos/flattabler/branch/master/graph/badge.svg)](https://app.codecov.io/gh/josesamos/flattabler?branch=master)
[![Downloads](http://cranlogs.r-pkg.org/badges/flattabler?color=brightgreen)](https://www.r-pkg.org:443/pkg/flattabler)
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

Below is an example of a pivot table obtained from the
[`pivottabler`](https://CRAN.R-project.org/package=pivottabler) package.
It is included in `flattabler` package in the form of the variable
`df_pivottabler`, defined as a data frame.

|        V1        |         V2          |        V3         |  V4   | V5  |  V6   |         V7         |  V8   |  V9   |  V10  |
|:----------------:|:-------------------:|:-----------------:|:-----:|:---:|:-----:|:------------------:|:-----:|:-----:|:-----:|
|                  |                     | Express Passenger |       |     |       | Ordinary Passenger |       |       | Total |
|                  |                     |        DMU        |  EMU  | HST | Total |        DMU         |  EMU  | Total |       |
| Number of Trains | Arriva Trains Wales |       3079        |       |     | 3079  |        830         |       |  830  | 3909  |
|                  |    CrossCountry     |       22133       |       | 732 | 22865 |         63         |       |  63   | 22928 |
|                  |   London Midland    |       5638        | 8849  |     | 14487 |        5591        | 28201 | 33792 | 48279 |
|                  |    Virgin Trains    |       2137        | 6457  |     | 8594  |                    |       |       | 8594  |
|                  |        Total        |       32987       | 15306 | 732 | 49025 |        6484        | 28201 | 34685 | 83710 |
|  Maximum Speed   | Arriva Trains Wales |        90         |       |     |  90   |         90         |       |  90   |  90   |
|                  |    CrossCountry     |        125        |       | 125 |  125  |        100         |       |  100  |  125  |
|                  |   London Midland    |        100        |  110  |     |  110  |        100         |  100  |  100  |  110  |
|                  |    Virgin Trains    |        125        |  125  |     |  125  |                    |       |       |  125  |
|                  |        Total        |        125        |  125  | 125 |  125  |        100         |  100  |  100  |  125  |

The transformation to obtain a flat table from the previous pivot table
using `flattabler` package is as follows:

``` r
library(flattabler)

ft <- pivot_table(df_pivottabler) |>
  define_labels(n_col = 2, n_row = 2) |>
  fill_labels() |>
  remove_agg() |>
  fill_values() |>
  unpivot(na_rm = TRUE)
```

The result is a `tibble` object that can be further transformed, for
example, by the `dplyr` package to remove the added data.

``` r
ft <- ft |>
  dplyr::filter(col2 != "Total") |>
  dplyr::filter(row2 != "Total")
```

The result obtained is as follows:

|       col1       |        col2         |        row1        | row2 | value |
|:----------------:|:-------------------:|:------------------:|:----:|:-----:|
| Number of Trains | Arriva Trains Wales | Express Passenger  | DMU  | 3079  |
| Number of Trains | Arriva Trains Wales | Ordinary Passenger | DMU  |  830  |
| Number of Trains |    CrossCountry     | Express Passenger  | DMU  | 22133 |
| Number of Trains |    CrossCountry     | Express Passenger  | HST  |  732  |
| Number of Trains |    CrossCountry     | Ordinary Passenger | DMU  |  63   |
| Number of Trains |   London Midland    | Express Passenger  | DMU  | 5638  |
| Number of Trains |   London Midland    | Express Passenger  | EMU  | 8849  |
| Number of Trains |   London Midland    | Ordinary Passenger | DMU  | 5591  |
| Number of Trains |   London Midland    | Ordinary Passenger | EMU  | 28201 |
| Number of Trains |    Virgin Trains    | Express Passenger  | DMU  | 2137  |
| Number of Trains |    Virgin Trains    | Express Passenger  | EMU  | 6457  |
|  Maximum Speed   | Arriva Trains Wales | Express Passenger  | DMU  |  90   |
|  Maximum Speed   | Arriva Trains Wales | Ordinary Passenger | DMU  |  90   |
|  Maximum Speed   |    CrossCountry     | Express Passenger  | DMU  |  125  |
|  Maximum Speed   |    CrossCountry     | Express Passenger  | HST  |  125  |
|  Maximum Speed   |    CrossCountry     | Ordinary Passenger | DMU  |  100  |
|  Maximum Speed   |   London Midland    | Express Passenger  | DMU  |  100  |
|  Maximum Speed   |   London Midland    | Express Passenger  | EMU  |  110  |
|  Maximum Speed   |   London Midland    | Ordinary Passenger | DMU  |  100  |
|  Maximum Speed   |   London Midland    | Ordinary Passenger | EMU  |  100  |
|  Maximum Speed   |    Virgin Trains    | Express Passenger  | DMU  |  125  |
|  Maximum Speed   |    Virgin Trains    | Express Passenger  | EMU  |  125  |

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

folder <- system.file("extdata", "csvfolder", package = "flattabler")
lpt <- read_text_folder(folder)

lft <- flatten_table_list(lpt, f)

lft
#> # A tibble: 201 × 6
#>    page  col1  col2  row1  row2  value
#>    <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 M1    b1    a01   e2    d4    1.88 
#>  2 M1    b1    a05   e1    d1    1.91 
#>  3 M1    b1    a05   e2    d3    1.10 
#>  4 M1    b1    a05   e2    d4    2.25 
#>  5 M1    b1    a09   e1    d1    2.55 
#>  6 M1    b1    a09   e1    d2    2.74 
#>  7 M1    b1    a09   e2    d3    3.99 
#>  8 M1    b1    a13   e1    d1    2.99 
#>  9 M1    b1    a13   e1    d2    1.02 
#> 10 M1    b1    a13   e2    d3    3.48 
#> # ℹ 191 more rows
```

In this way we can generate a flat table from a list of pivot tables.
The list of pivot tables is generated using package functions to import
them from various data sources.
