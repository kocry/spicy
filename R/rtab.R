#' Cross table with row percentages
#'
#' Two-way frequency table with row percentages
#'
#' @param x row variable
#' @param y column variable
#' @param digits number of digits to display
#' @param total if \code{TRUE}, add a column with the sum of percentages and a row with global percentages
#' @param n if \code{TRUE}, display number of observations per row.
#' @param statistics if \code{TRUE}, add Chi-2 test, p-value and Cramer's V in table note
#' @param drop if \code{TRUE}, lines or columns with a sum of zero, which would generate \code{NaN} percentages, are dropped.
#' @param ... parameters passed to other methods.
#'
#' @return The result is an object of class \code{table} and \code{proptab}.
#' @importFrom stats chisq.test
#' @export
#'
#' @examples
#' \dontrun{
#' rtab(d$vote, d$sex)
#' rtab(d$vote, d$sex, total = FALSE, n = FALSE, statistics = FALSE)
#' }
rtab <- function (x, y, digits = 1, total = TRUE, n = TRUE, statistics = TRUE, drop = TRUE, ...) {
  tab <- table(x, y)
  # subset to non-empty rows/columns
  if(drop) tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop=FALSE]
  dn <- names(dimnames(tab))
  if (total) {
    .tmp.rownames <- rownames(tab)
    tab <- rbind(tab, apply(tab, 2, sum))
    rownames(tab) <- c(.tmp.rownames, gettext("All", domain="R-spicy"))
  }
  if (n) effectifs <- apply(tab, 1, sum)
  tab <- prop.table(tab, 1) * 100
  if (total) {
    .tmp.colnames <- colnames(tab)
    tab <- cbind(tab, Total = apply(tab, 1, sum))
    colnames(tab) <- c(.tmp.colnames, gettext("Total", domain="R-spicy"))
  }
  if (n) tab <- cbind(tab, N = effectifs)
  result <- as.data.frame.array(tab)
  class(result) <- c("proptab", class(result))
  chi_table <- stats::chisq.test(table(x, y))
  chi2 <- as.numeric(chi_table[1])
  df <- as.numeric(chi_table[2])
  p_value <- as.numeric(chi_table[3])
  cramer <- cramer_v(table(x, y))
  note <- Glue("Chi-2 = {sprintf('%.1f', chi2)}, df = {sprintf('%.0f', df)}, p-value = {sprintf('%.3f', p_value)}, Cramer's V = {sprintf('%.2f', cramer)}")
  ifelse(statistics,
         return(print_table(result, digits = 1, note = note)),
         return(print_table(result, digits = 1)))
}
