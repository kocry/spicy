#' Cross table
#'
#' Two-way frequency table with column (default) or row percentages
#'
#' @param d data.frame
#' @param x row variable
#' @param y column variable
#' @param digits number of digits to display
#' @param rowprct if \code{TRUE}, compute row percentages. Default \code{FALSE} computes column percentages.
#' @param total if \code{TRUE} (default), add a column with the sum of percentages and a row with global percentages.
#' @param n if \code{TRUE} (default), display number of observations per row.
#' @param statistics if \code{TRUE} (default), add Chi-2 test, p-value and Cramer's V in table note, a title and print a three-line table. If \code{FALSE}, return a tibble object,
#' @param drop if \code{TRUE}, lines or columns with a sum of zero, which would generate \code{NaN} percentages, are dropped.
#' @param ... parameters passed to other methods.
#'
#' @return The result is an object of class \code{table} and \code{proptab}.or a tibble if statistics argument is FALSE
#' @importFrom stats chisq.test
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' tab(mtcars, cyl, vs)
#' mtcars |> tab(cyl, vs)
#' tab(mtcars, cyl, vs, rowprct = TRUE, total = FALSE, n = FALSE, statistics = FALSE)
#' tab(mtcars, cyl, vs, total = FALSE, n = FALSE, statistics = FALSE)
#' }

ctab <- function (d, x, y, digits = 1, rowprct = FALSE, total = TRUE, n = TRUE, statistics = TRUE, drop = TRUE, ...) {
  tab <- eval(substitute(table(d$x, d$y)))
  # subset to non-empty rows/columns
  if(drop) tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop=FALSE]
  dn <- names(dimnames(tab))
  if (total) {
    .tmp.colnames <- colnames(tab)
    tab <- cbind(tab, apply(tab, 1, sum))
    colnames(tab) <- c(.tmp.colnames, gettext("All", domain="R-spicy"))
  }
  if (n) effectifs <- apply(tab, 2, sum)
  tab <- prop.table(tab, 2) * 100
  if (total) {
    .tmp.rownames <- rownames(tab)
    tab <- rbind(tab, Total = apply(tab, 2, sum))
    rownames(tab) <- c(.tmp.rownames, gettext("Total", domain="R-spicy"))
  }
  if (n) tab <- rbind(tab, N = effectifs)
  #result <- as_tibble(as.data.frame.array(tab), rownames = "modalities")
  result <- as.data.frame.array(tab)


  # row percent
  if (rowprct) {

  tab <- eval(substitute(table(d$x, d$y)))
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
  }

  class(result) <- c("proptab", class(result))
  chi_table <- stats::chisq.test(eval(substitute(table(d$x, d$y))))
  chi2 <- as.numeric(chi_table[1])
  df <- as.numeric(chi_table[2])
  p_value <- as.numeric(chi_table[3])
  cramer <- suppressWarnings(cramer_v(eval(substitute(table(d$x, d$y)))))
  note <- Glue("Chi-2 = {sprintf('%.1f', chi2)} (df = {sprintf('%.0f', df)}), p-value = {sprintf('%.3f', p_value)}
               Cramer's V = {sprintf('%.2f', cramer)}")

  #result1 <- as_tibble(as.data.frame.array(result), rownames = "modalities")

  g <- eval(substitute(select(d, x,y)))
  g <- attributes(g)$names
  title <- Glue("Cross-table: {first(g)} {Print('<<silver x>>')} {last(g)} (%)")

  ifelse(statistics,
         return(print_table(result, row.names = T, digits = digits, title = title, note = note, )),
         return(as_tibble(as.data.frame.array(result), rownames = "modalities")))
        # return(print_table(result, digits = digits))
}

