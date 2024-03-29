% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pivot_table.R
\name{remove_left}
\alias{remove_left}
\alias{remove_left.pivot_table}
\title{Remove left columns from a pivot table}
\usage{
remove_left(pt, n)

\method{remove_left}{pivot_table}(pt, n)
}
\arguments{
\item{pt}{A \code{pivot_table} object.}

\item{n}{A number, number of columns to remove.}
}
\value{
A \code{pivot_table} object.
}
\description{
Remove left columns from the pivot table represented by the object.
}
\details{
A pivot table should only contain label rows and columns, and an array of
values, usually numeric data.

All columns not belonging to the pivot table must be removed.
}
\examples{

pt <- pt_ex |> remove_left(3)

}
\seealso{
\code{\link{pivot_table}}

Other pivot table transformation functions: 
\code{\link{extract_labels}()},
\code{\link{fill_labels}()},
\code{\link{fill_values}()},
\code{\link{remove_agg}()},
\code{\link{remove_bottom}()},
\code{\link{remove_cols}()},
\code{\link{remove_empty}()},
\code{\link{remove_k}()},
\code{\link{remove_right}()},
\code{\link{remove_rows}()},
\code{\link{remove_top}()},
\code{\link{replace_dec}()},
\code{\link{unpivot}()}
}
\concept{pivot table transformation functions}
