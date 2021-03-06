% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pivot_table_unpivot.R
\name{unpivot}
\alias{unpivot}
\alias{unpivot.pivot_table}
\title{Unpivot a pivot table}
\usage{
unpivot(pt, include_page = TRUE, na.rm = TRUE)

\method{unpivot}{pivot_table}(pt, include_page = TRUE, na.rm = TRUE)
}
\arguments{
\item{pt}{A \code{pivot_table} object.}

\item{include_page}{A boolean, indicates whether a column with the page
information is included or not.}

\item{na.rm}{A boolean, indicates whether NA values from the array of values
are removed or not.}
}
\value{
A \code{tibble}.
}
\description{
Transforms a pivot table into a flat table (implemented by a \code{tibble}). An
additional column with page information can be included. NA values can be
excluded from the array of values.
}
\details{
A pivot table should only contain label rows and columns, and an array of
values, usually numeric data.

To correctly carry out this operation, the number of rows and columns that
contain labels must be defined, and the table must only contain the pivot
table rows and columns.
}
\examples{
library(tidyr)

a_tibble <-
  pt_m4 \%>\%
  remove_top(1) \%>\%
  define_labels(n_col = 2, n_row = 2) \%>\%
  unpivot(include_page = FALSE)

a_tibble <-
  pt_m4 \%>\%
  set_page(1, 1) \%>\%
  remove_top(1) \%>\%
  define_labels(n_col = 2, n_row = 2) \%>\%
  remove_k() \%>\%
  replace_dec() \%>\%
  fill_values() \%>\%
  fill_labels() \%>\%
  remove_agg() \%>\%
  unpivot()

a_tibble <-
  pt_pivottabler \%>\%
  define_labels(n_col = 2, n_row = 2) \%>\%
  fill_values() \%>\%
  fill_labels() \%>\%
  remove_agg("Total") \%>\%
  remove_agg() \%>\%
  unpivot(include_page = FALSE, na.rm = FALSE)

}
\seealso{


Other flat table generation functions: 
\code{\link{flatten_table_list}()}
}
\concept{flat table generation functions}
