#' Generate frequency table of missing values.
#'
#' Generate a frequency table of missing values as raw counts and percentages.
#'
#' @param data either a vector or a data frame object
#' @param ... parameters passed to other methods
#' @return The result is an object of class data.frame.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## Examine a single vector.
#' fre_na(d$sex)
#' ## Examine a data frame.
#' fre_na(d)
#' ## Examine several variables.
#' fre_na(d, "vote", "sex")
#' ## To see only variables with the most number of missing values
#' head(fre_na(d))
#' }

fre_na <- function(data, ...) {
  d = NULL
  if (inherits(data, "data.frame")) {
    s <- lookfor(data, ...)$variable
    d = data[, c(s)]
  }
  else {
    d = as.data.frame(data)
  }
  if (is.null(dim(d))) {
    c = length(d)
  }
  else {
    c = nrow(d)
  }
  d = is.na(as.matrix(d))
  d = as.matrix(colSums(d))
  d = cbind(d, 100 * round(d / c, 2))
  d = d[order(d[, 1], decreasing = TRUE), ]
  n = c("missing", "%")
  if(is.null(dim(d)))
    names(d) = n
  else
    colnames(d) = n

  return(d)
}
