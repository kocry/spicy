#' @title Computing row sums
#'
#' @description Computing row sums from a `data.frame` or `matrix` and dealing with NAs
#'
#' @param x a data frame or matrix
#' @param n a numeric value indicates the minimum amount of valid values (not NA) per row to calculate row sum (default = 1). Numeric value between 0 and 1 indicates a proportion of valid values per row to calculate the row sum
#'
#' @return row sums
#' @export
#'
#' @examples
#' \dontrun{
#' sum_n(df[, 2:5], 3)
#' sum_n(df[, 1:5], 0.5)
#' sum_n(mtcars[, c("mpg","cyl")], 2)
#' }
sum_n <- function(x, n = 1) {
  digs <- n%%1
  if (digs != 0) {n <- round(ncol(x) * digs)}
  if (ncol(x) < n) {
    warning("\'n\' must be smaller or equal to number of columns in data frame or matrix.", immediate. = T)}
  apply(x, 1, function(x) ifelse(sum(!is.na(x)) >= n, sum(x, na.rm = TRUE),
                                 NA))
}
