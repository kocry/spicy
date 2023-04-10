

#' @title Convert NA levels ("NA") to NA values (NA) of a data.frame
#'
#' @description Convert NA levels ("NA") to NA values (NA) for all factors of a data.frame. All the variables must be factors. This is the inverse function of \code{fcts_na_val()}
#' @param x a data.frame
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' gender <- factor(c("female", "male", "male", "NA", "female"))
#' vote <- factor(c("yes", "NA", "no", "yes", "yes"))
#' df <- data.frame(gender, vote)
#' df
#' fcts_val_na(df)
#' }

fcts_val_na <- function(x) {
  if (!is.data.frame(x))
    stop("fcts_na_val only works with a data.frame")
  if (!all(sapply(x, is.factor)))
    stop("There is at least one variable that is not a factor")
  if (any(sapply(x, is.na)))
    stop("There is NA in the data.frame")
  x[x == "NA"] <- NA
  x
}
