
#' @title Identify the indexes of the NA categories
#'
#' @description To use with speMCA function of GDAtools package to treat all NA categories as passive categories in the "excl" argument.
#'
#' @param x a data.frame
#'
#' @return numeric vector indicating the indexes of the NA categories
#' @export
#'
#' @examples
#' \dontrun{
#' library(GDAtools)
#' mca <- speMCA(x, ncp = 5, excl = excl_na(x))
#' }
utils::globalVariables(c("mod"))

excl_na <- function(x) {
  dichoto <- function (data, out = "numeric")
  {
    data <- as.data.frame(data)
    res <- matrix(nrow = nrow(data), ncol = length(levels(data[,
                                                               1])))
    for (i in 1:ncol(data)) {
      if (is.factor(data[, i]) == FALSE)
        data[, i] <- factor(data[, i])
      nlevels <- length(levels(data[, i]))
      temp <- matrix(nrow = nrow(data), ncol = nlevels)
      for (j in 1:nlevels) {
        temp[, j] <- 0
        temp[data[, i] == levels(data[, i])[j], j] <- 1
      }
      colnames(temp) <- paste(names(data)[i], levels(data[,
                                                          i]), sep = ".")
      if (i == 1)
        res <- temp
      else res <- cbind(res, temp)
    }
    res <- as.data.frame(res)
    if (out == "factor")
      for (i in 1:ncol(res)) res[, i] <- as.factor(res[, i])
    res
  }
  getindex <- function (x)
  {
    x <- as.data.frame(x)
    for (i in 1:ncol(x)) x[, i] <- factor(x[, i])
    colnames(dichoto(x))
  }
  addNAfct <- function(x) {
    if (is.factor(x)) {
      return(factor(x, levels = c(levels(x), "NA")))
    }
    return(x)
  }
  x[] <- lapply(x, addNAfct)
  x[is.na(x)] <- "NA"
  excl <- getindex(x)
  excl <- as.data.frame(excl)
  names(excl)[1] <- "mod"
  excl$rank_na <- 1:nrow(excl)
  excl <- subset(excl, grepl(".NA", mod))
  excl$rank_na
}
