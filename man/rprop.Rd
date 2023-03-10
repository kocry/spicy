% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rprop.R
\name{rprop}
\alias{rprop}
\title{Row percentages}
\usage{
rprop(
  tab,
  digits = 1,
  total = TRUE,
  percent = FALSE,
  drop = TRUE,
  n = FALSE,
  ...
)
}
\arguments{
\item{tab}{frequency table}

\item{digits}{number of digits to display}

\item{total}{if \code{TRUE}, add a column with the sum of percentages and a row with global percentages}

\item{percent}{if \code{TRUE}, add a percent sign after the values when printing}

\item{drop}{if \code{TRUE}, lines or columns with a sum of zero, which would generate \code{NaN} percentages, are dropped.}

\item{n}{if \code{TRUE}, display number of observations per row.}

\item{...}{parameters passed to other methods.}
}
\value{
The result is an object of class \code{table} and \code{proptab}.
}
\description{
Row percentages of a two-way frequency table
}
\examples{
\dontrun{
## Sample table
data(mtcars4)
tab <- table(mtcars$cyl, mtcars$vs)
## Column percentages
rprop(tab)
mtcars |> dplyr::select(cyl,vs) |> table() |> rprop()
## Row percentages with custom display
rprop(tab, digits=2, percent=TRUE, total=FALSE)
}
}
