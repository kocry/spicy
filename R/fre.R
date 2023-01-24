#' Generate frequency tables.
#'
#' Generate and format frequency tables from a variable or a table, with percentages and formatting options.
#'
#' @param x either a vector to be tabulated, or a table object
#' @param digits number of digits to keep for the percentages
#' @param cum if TRUE, display cumulative percentages
#' @param format if TRUE, print a three-line table, display variable label and data type (the class)
#' @param total if TRUE, add a final row with totals
#' @param exclude vector of values to exclude from the tabulation (if \code{x} is a vector)
#' @param sort if specified, allow to sort the table by increasing ("inc") or decreasing ("dec") frequencies
#' @param valid if TRUE, display valid percentages
#' @param levels the desired levels for the factor in case of labelled vector (\pkg{labelled} package
#'    must be installed): "labels" for value labels, "values" for values or
#'    "prefixed" for labels prefixed with values
#' @param na.last if TRUE, NA values are always be last table row
#' @return The result is an object of class data.frame.
#'
#' @importFrom labelled to_factor
#' @export
#'
#' @examples
#' \dontrun{
#' # factor
#' fre(d$educ)
#' fre(d$educ, cum = TRUE, total = TRUE)
#' fre(d$educ, cum = TRUE, total = TRUE, sort = "dec")
#'
#' # labelled data
#' fre(d$region)
#' fre(d$region, levels = "l")
#' fre(d$region, levels = "v")
#' }

fre <- function(x, digits = 1, cum = FALSE, format = FALSE, total = TRUE, exclude = NULL, sort = "",
                 valid = !(NA %in% exclude), levels = c("prefixed", "labels", "values"),
                 na.last = TRUE) {

  levels <- match.arg(levels)

  if (is.table(x)) {
    tab <- x
  } else {
    tab <- table(labelled::to_factor(x, levels), exclude = exclude)
  }

  effectifs <- as.vector(tab)
  pourc <- as.vector(effectifs / sum(effectifs) * 100)
  result <- data.frame(n = effectifs, pourc = pourc)

  if (valid) {
    user_na <- unique(as.character(labelled::to_factor(x, levels)[is.na(x)]))
    NA.position <- which(is.na(names(tab)) | names(tab) %in% user_na)
    n.na <- sum(tab[NA.position])
    valid.pourc <- as.vector(effectifs / (sum(effectifs) - n.na) * 100)
    valid.pourc[NA.position] <- 0 # temporary 0 for cumsum
    result <- cbind(result, valid.pourc)
  }

  ## Avoid duplicate row names if both NA and "NA" in tab
  if ("NA" %in% names(tab)) {
    names(tab)[names(tab) == "NA"] <- "\"NA\""
  }
  rownames(result) <- ifelse(is.na(names(tab)), "NA", names(tab))

  if (sort == "inc") result <- result[order(result$n),]
  if (sort == "dec") result <- result[order(result$n, decreasing = TRUE),]

  if (na.last && "NA" %in% rownames(result)) {
    result <- rbind(result[-which(rownames(result) == "NA"), ], result["NA", ])
  }

  if (total) result <- rbind(result, Total = apply(result,2,sum))
  if (total & valid)
    result[length(result$pourc),"valid.pourc"] <- 100

  if (cum) {
    pourc.cum <- cumsum(result$pourc)
    if (total) pourc.cum[length(pourc.cum)] <- 100
    result <- cbind(result, pourc.cum)
    if (valid) {
      valid.pourc.cum <- cumsum(result$valid.pourc)
      if (total) valid.pourc.cum[length(valid.pourc.cum)] <- 100
      result <- cbind(result, valid.pourc.cum)
    }
  }

  if (valid) {
    NA.position <- which(rownames(result) == "NA" | rownames(result) %in% user_na)
    result[NA.position, "valid.pourc"] <- NA
    if (cum)
      result[NA.position, "valid.pourc.cum"] <- NA
  }

  names(result)[names(result) == "pourc"] <- "%"
  names(result)[names(result) == "valid.pourc"] <- "valid_%"
  names(result)[names(result) == "pourc.cum"] <- "%_cum"
  names(result)[names(result) == "valid.pourc.cum"] <- "valid_%_cum"

  class(result) <- c("freqtab", class(result))

  labelx <- attr(x,"label")
  classx <- class(x)
  note1 <- Glue("Label: {labelx}","\n", "Type: {classx}")
  note2 <- Glue("Type: {classx}")

  ifelse(format & !is.null(attr(x,"label")),
         return(print_table(result, digits = digits, note=note1)),
         ifelse(format & is.null(attr(x,"label")),
                                 return(print_table(result, digits = digits, note=note2)),
                                 return(round(result, digits = digits))))
  }
