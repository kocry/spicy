#' Computing row means from a `data.frame` or `matrix` and dealing with NAs
#'
#' @param x a data frame or matrix
#' @param n a numeric value indicates the minimum amount of valid values (not NA) per row to calculate row mean (default = 1). Numeric value between 0 and 1 indicates a proportion of valid values per row to calculate the row mean
#'
#' @return row means
#' @export
#'
#' @examples
#' \dontrun{
#' mean_n(df[, 2:5], 3)
#' mean_n(df[, 1:5], 0.5)
#' mean_n(mtcars[, c("mpg","cyl")], 2)
#' }
mean_n <- function(x, n = 1) {
  digs <- n%%1
  if (digs != 0) {n <- round(ncol(x) * digs)}
  if (ncol(x) < n) {
    warning("\'n\' must be smaller or equal to number of columns in data frame or matrix.", immediate. = T)}
  apply(x, 1, function(x) ifelse(sum(!is.na(x)) >= n, mean(x, na.rm = TRUE),
                                 NA))
}

