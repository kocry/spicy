#' @title Generate frequency tables.
#'
#' @description Generate and format frequency tables from a variable or a table, with percentages and formatting options. Support also R base language \code{fre(d$x)} and native pipe \code{d |> fre(x)}.
#'
#' @param d a data.frame
#' @param x variable to be tabulated
#' @param digits number of digits to keep for the percentages
#' @param cum if TRUE (default: FALSE), display cumulative percentages
#' @param format if TRUE (default), print a three-line table, display variable label and data type (the class). If FALSE, return a data.frame object.
#' @param total if FALSE (default: TRUE), remove a final row with totals
#' @param exclude vector of values to exclude from the tabulation (if \code{x} is a vector)
#' @param sort if specified, allow to sort the table by increasing ("inc") or decreasing ("dec") frequencies
#' @param valid if TRUE, display valid percentages
#' @param levels the desired levels for the factor in case of labelled vector (\pkg{labelled} package
#'    must be installed): "labels" for value labels, "values" for values or
#'    "prefixed" for labels prefixed with values
#' @param na.last if TRUE, NA values are always be last table row
#' @param file File name of MS Word (\code{.doc}). It doesn't work if \code{format = FALSE}
#' @return The result is an object of class data.frame.
#'
#' @importFrom glue glue glue_collapse
#' @importFrom labelled to_factor
#' @export
#'
#' @examples
#' \dontrun{
#' # factor
#' data(mtcars)
#' fre(mtcars, vs)
#' fre(mtcars, mtcars$vs)
#' fre(mtcars$vs)
#' mtcars |> fre(vs)
#' fre(mtcars, vs, cum = TRUE, total = TRUE)
#' fre(mtcars, vs, cum = TRUE, total = TRUE, sort = "inc")
#'
#' # labelled data
#' fre(d$region)
#' fre(d$region, levels = "l")
#' fre(d$region, levels = "v")
#' }


fre <- function(d = parent.frame(), x, digits = 1, cum = FALSE, format = TRUE, total = TRUE, exclude = NULL, sort = "",
                valid = !(NA %in% exclude), levels = c("prefixed", "labels", "values"),
                na.last = TRUE, file = NULL) {

  levels <- match.arg(levels)

  if (missing(x)){
    gx <- deparse(substitute(d))
    gx <- gsub("^.*\\$","", gx)
  }
  else {
    gx <- deparse(substitute(x))
    gx <- gsub("^.*\\$","", gx)
  }


  if (missing(x)){
    d <- d}
  else  {
    d <- eval(substitute(as.data.frame(d)), d)
  }



  if (missing(x)){
    tab <- eval(substitute(table(labelled::to_factor({{d}}, levels), exclude = exclude)))
  }

  else {
    tab <- eval(substitute(table(labelled::to_factor(x, levels), exclude = exclude)), d)
  }

  #tab <- eval(substitute(table(labelled::to_factor(d$x, levels), exclude = exclude)), d)




  # if (is.table(d) & is.null(x)) {
  #  tab <- d
  #} else {
  #  tab <- eval(substitute(table(labelled::to_factor(d$x, levels), exclude = exclude)))
  #}
  # tab <- eval(substitute(table(labelled::to_factor(d$x, levels), exclude = exclude)))

  effectifs <- as.vector(tab)
  pourc <- as.vector(effectifs / sum(effectifs) * 100)
  result <- data.frame(n = effectifs, pourc = pourc)

  if (valid) {
    if (missing(x)){
      user_na <- eval(substitute(unique(as.character(labelled::to_factor({{d}}, levels)[is.na({{d}})]))))
    }

    else {
      user_na <- eval(substitute(unique(as.character(labelled::to_factor(x, levels)[is.na(x)]))), d)
    }

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


  if (missing(x)){
    labelx <- attr(d, "label")
  }
  else {
    labelx <- eval(substitute(attr(x, "label")), d)
  }

  if(!is.null(labelx)){
    labelx <- gsub("'", "\u2019", labelx)
    labelx <- gsub("\"", "\u2019", labelx)
    labelx <- gsub("''", "\u2019", labelx)
  }
  else{
    labelx
  }


  if (missing(x)){
    classx <- class(d)
  }
  else {
    classx <- eval(substitute(class(x)), d)
  }

  classx <- ifelse(length(classx) > 1,
                   glue::glue("{glue::glue_collapse(classx,  sep = ', ')}"), classx)

  if(is.null(file)){
    note1 <- Glue("<<silver Label: {labelx}","\n", "Type: {classx}>>")
    note2 <- Glue("<<silver Type: {classx}>>")
  }
  else{
    note1 <- Glue("Label: {labelx}.
                  Type: {classx}")
    note2 <- Glue("Type: {classx}")
  }


  if(is.null(file))
    title <- Glue("<<silver Frequency Table: >> {gx}")
  else
    title <- Glue("Frequency Table: {gx}")


  ifelse(format & !is.null(labelx),
         return(print_table(result, digits = c(0, digits, digits, digits, digits), title = title, note=note1, file = file)),
         ifelse(format & is.null(labelx),
                return(print_table(result, digits = c(0, digits, digits, digits, digits), title = title, note=note2, file = file)),
                return(round(result, digits = digits))))
}
