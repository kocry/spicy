#' Cross table with row percentages
#'
#' Two-way frequency table with row percentages
#'
#' @param x row variable
#' @param y column variable
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
#' rtab(d$vote, d$sex, n = TRUE)
#' rtab(d$vote, d$sex, percent = T, n = TRUE)
#' }
rtab <- function (x, y, digits = 1, total = TRUE, percent = FALSE, drop = TRUE, n = FALSE, ...) {
  RowData <- deparse(substitute(x))
  ColData <- deparse(substitute(y))
  tab <- table(x, y)
  # subset to non-empty rows/columns
  if(drop) tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop=FALSE]
  dn <- names(dimnames(tab))
  if (total) {
    .tmp.rownames <- rownames(tab)
    tab <- rbind(tab, apply(tab, 2, sum))
    rownames(tab) <- c(.tmp.rownames, gettext("All", domain="R-questionr"))
  }
  if (n) effectifs <- apply(tab, 1, sum)
  tab <- prop.table(tab, 1) * 100
  if (total) {
    .tmp.colnames <- colnames(tab)
    tab <- cbind(tab, Total = apply(tab, 1, sum))
    colnames(tab) <- c(.tmp.colnames, gettext("Total", domain="R-questionr"))
  }
  if (n) tab <- cbind(tab, n = effectifs)
  result <- as.table(tab)
  names(dimnames(result)) <- dn
  class(result) <- c("proptab", class(result))
  attr(result, "percent") <- percent
  attr(result, "digits") <- digits
  attr(result, "total") <- total
  attr(result, "col.n") <- n
  result
}
