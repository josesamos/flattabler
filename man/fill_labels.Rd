% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pivot_table.R
\name{fill_labels}
\alias{fill_labels}
\alias{fill_labels.pivot_table}
\title{Fill in missing labels}
\usage{
fill_labels(pt, down, right)

\method{fill_labels}{pivot_table}(pt, down = TRUE, right = TRUE)
}
\arguments{
\item{pt}{A \code{pivot_table} object.}

\item{down}{A boolean, fill down.}

\item{right}{A boolean, fill right.}
}
\value{
A \code{pivot_table} object.
}
\description{
Fills missing values in row and column labels for a pivot table. By default,
columns are filled down and rows are filled right.
}
\details{
A pivot table should only contain label rows and columns, and an array of
values, usually numeric data. The row and column closest to the data array
are not filled (they must have data defined for each cell).

To correctly carry out this operation, the number of rows and columns that
contain labels must be defined, and the table must only contain the pivot
table rows and columns.
}
\examples{

pt <-
  pt_ex |>
  remove_top(1) |>
  define_labels(n_col = 2, n_row = 2) |>
  fill_labels(down = TRUE, right = TRUE)

}
\seealso{
\code{\link{pivot_table}}

Other pivot table transformation functions: 
\code{\link{extract_labels}()},
\code{\link{fill_values}()},
\code{\link{remove_agg}()},
\code{\link{remove_bottom}()},
\code{\link{remove_cols}()},
\code{\link{remove_empty}()},
\code{\link{remove_k}()},
\code{\link{remove_left}()},
\code{\link{remove_right}()},
\code{\link{remove_rows}()},
\code{\link{remove_top}()},
\code{\link{replace_dec}()},
\code{\link{unpivot}()}
}
\concept{pivot table transformation functions}
