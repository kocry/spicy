

#' @title Convert NA values to NA levels of a data.frame
#'
#' @description Convert NA values to NA levels for all factors of a data.frame. All the variables must be factors.
#' @param x a data.frame
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' gender <- factor(c("female", "male", "male", NA, "female"))
#' vote <- factor(c("yes", NA, "no", "yes", "yes"))
#' df <- data.frame(gender, vote)
#' df
#' fcts_na_val(df)
#' }
#'
fcts_na_val <- function(x) {
  if (!is.data.frame(x))
    stop("fcts_na_val only works with a data.frame")
  if (!all(sapply(x, is.factor)))
    stop("There is at least one variable that is not a factor")
  if (!any(sapply(x, is.na)))
    stop("There is no NA in the data.frame")
  addNAfct <- function(z) {
    if (is.factor(z)) {
      return(factor(z, levels = c(levels(z), "NA")))
    }
    return(z)
  }
  x[] <- lapply(x, addNAfct)
  x[is.na(x)] <- "NA"
  x
}

