% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mean_n.R
\name{mean_n}
\alias{mean_n}
\title{Computing row means}
\usage{
mean_n(x, n = 1)
}
\arguments{
\item{x}{a data frame or matrix}

\item{n}{a numeric value indicates the minimum amount of valid values (not NA) per row to calculate row mean (default = 1). Numeric value between 0 and 1 indicates a proportion of valid values per row to calculate the row mean}
}
\value{
row means
}
\description{
Computing row means from a \code{data.frame} or \code{matrix} and dealing with NAs
}
\examples{
\dontrun{
mean_n(df[, 2:5], 3)
mean_n(df[, 1:5], 0.5)
mean_n(mtcars[, c("mpg","cyl")], 2)
}
}
