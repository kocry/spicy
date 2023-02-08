#' @title Row percentages
#'
#' @description Row percentages of a two-way frequency table
#'
#' @param tab frequency table
#' @param digits number of digits to display
#' @param total if \code{TRUE}, add a column with the sum of percentages and a row with global percentages
#' @param percent if \code{TRUE}, add a percent sign after the values when printing
#' @param drop if \code{TRUE}, lines or columns with a sum of zero, which would generate \code{NaN} percentages, are dropped.
#' @param n if \code{TRUE}, display number of observations per row.
#' @param ... parameters passed to other methods.
#'
#' @return The result is an object of class \code{table} and \code{proptab}.
#' @export
#'
#' @examples
#' \dontrun{
#' ## Sample table
#' data(mtcars4)
#' tab <- table(mtcars$cyl, mtcars$vs)
#' ## Column percentages
#' rprop(tab)
#' mtcars |> dplyr::select(cyl,vs) |> table() |> rprop()
#' ## Row percentages with custom display
#' rprop(tab, digits=2, percent=TRUE, total=FALSE)
#' }

rprop <- function(tab, digits = 1, total = TRUE, percent = FALSE, drop = TRUE, n=FALSE, ...) {
  # subset to non-empty rows/columns
  if(drop) tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop=FALSE]
  dn <- names(dimnames(tab))
  if (total) {
    .tmp.rownames <- rownames(tab)
    tab <- rbind(tab, apply(tab, 2, sum))
    rownames(tab) <- c(.tmp.rownames, gettext("All", domain="R-spicy"))
  }
  if (n) effectifs <- apply(tab, 1, sum)
  tab <- base::prop.table(tab, 1) * 100
  if (total) {
    .tmp.colnames <- colnames(tab)
    tab <- cbind(tab, Total = apply(tab, 1, sum))
    colnames(tab) <- c(.tmp.colnames, gettext("Total", domain="R-spicy"))
  }
  if (n) tab <- cbind(tab, n = effectifs)
  result <- as.table(tab)
  names(dimnames(result)) <- dn
  class(result) <- c("proptab", class(result))
  attr(result, "percent") <- percent
  attr(result, "digits") <- digits
  attr(result, "total") <- total
  attr(result, "col.n") <- n
  return(result)
  }

