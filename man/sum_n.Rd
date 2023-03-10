% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sum_n.R
\name{sum_n}
\alias{sum_n}
\title{Computing row sums}
\usage{
sum_n(x, n = 1)
}
\arguments{
\item{x}{a data frame or matrix}

\item{n}{a numeric value indicates the minimum amount of valid values (not NA) per row to calculate row sum (default = 1). Numeric value between 0 and 1 indicates a proportion of valid values per row to calculate the row sum}
}
\value{
row sums
}
\description{
Computing row sums from a \code{data.frame} or \code{matrix} and dealing with NAs
}
\examples{
\dontrun{
sum_n(df[, 2:5], 3)
sum_n(df[, 1:5], 0.5)
sum_n(mtcars[, c("mpg","cyl")], 2)
}
}
