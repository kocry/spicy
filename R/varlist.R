#' @title List of variables
#'
#' @description Data frame of variables (list of variables) contains descriptions of the attributes of each variable (label, values, class, typeof, number of valid rows and NAs)
#'
#' @param x a data.frame
#' @param values if values = FALSE (default), display minimum and maximum values of columns. If values = TRUE, display all values of columns.
#' @param tbl if FALSE (default), return a data frame of variables in source editor of RStudio; if TRUE, print a tibble data format in the console
#'
#' @return a data.frame
#' @importFrom collapse vlabels funique na_omit ffirst flast fndistinct fmax fndistinct.data.frame fnobs.data.frame fnrow fnobs qTBL
#' @importFrom dplyr nth
#' @importFrom lubridate is.POSIXct is.POSIXlt is.POSIXt is.Date
#' @importFrom tibble view
#' @export
#'
#' @examples
#' \dontrun{
#' varlist(x)
#' varlist(x, values = TRUE, tbl = FALSE)
#' }

varlist <- function(x, values = FALSE, tbl = FALSE) {

  if (!is.data.frame(x))
    stop("varlist only works with data frames")
  res <- list(Variable = attr(x, "names"))
  # attributes(x) <- NULL
  res$Label <- collapse::vlabels(x, attrn = "label", use.names = FALSE)
  attr(res, "row.names") <- c(NA_integer_, -length(x))

  options(scipen = 999)
  min_max <- lapply(x, function(x) {
    if (is.factor(x)) {
      paste(levels(x), collapse = ", ")
    } else if (is.logical(x) & (length(x) == sum(is.na(x)))) {
      "Full NA"
    } else if (is.logical(x) & (length(x) > sum(is.na(x)))) {
      paste(collapse::funique(collapse::na_omit(x), sort = T), collapse = ", ")
    } else if (is.character(x)) {
      paste(collapse::ffirst(collapse::na_omit(substr(x, 1, 20))), "...", collapse::flast(collapse::na_omit(substr(x, 1, 20))))
    } else if (all(is.na(x))) {
      "Full NA"
    } else if (is.list(x)) {
      "Lists"
    } else if (lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x) |
               lubridate::is.POSIXt(x) | lubridate::is.Date(x)) {
      paste(min(x, na.rm = T), "...", max(x, na.rm = T))
    } else if (collapse::fndistinct(x, na.rm = T) < 6) {
      paste(collapse::funique(collapse::na_omit(x), sort = T), collapse = ", ")
    } else {
      paste(paste(dplyr::nth(collapse::funique(collapse::na_omit(x), sort = T), 1),
                  dplyr::nth(collapse::funique(collapse::na_omit(x), sort = T), 2),
                  dplyr::nth(collapse::funique(collapse::na_omit(x), sort = T), 3),
                  dplyr::nth(collapse::funique(collapse::na_omit(x), sort = T), 4),
                  dplyr::nth(collapse::funique(collapse::na_omit(x), sort = T), 5),
                  sep = ", "
      ), " ... ", collapse::fmax(x, na.rm = T), sep = "")
    }
  })

  all <- lapply(x, function(x) {
    if (is.factor(x)) {
      paste(levels(x), collapse = ", ")
    } else if (is.logical(x) & (length(x) == sum(is.na(x)))) {
      "Full NA"
    } else if (is.logical(x) & (length(x) > sum(is.na(x)))) {
      paste(collapse::funique(collapse::na_omit(x), sort = T), collapse = ", ")
    } else if (is.character(x)) {
      paste(collapse::funique(collapse::na_omit(x), sort = T), collapse = ", ")
    } else if (all(is.na(x))) {
      "Full NA"
    } else if (is.list(x)) {
      "Lists"
    } else {
      paste(collapse::funique(collapse::na_omit(x), sort = T), collapse = ", ")
    }
  })

  if (values) res$Values <- all
  else res$Values <- min_max

  oldClass(res) <- "data.frame"
  pasteclass <- function(x) if(length(cx <- class(x)) > 1L) paste(cx, collapse = ", ") else cx
  res$Class <- vapply(x, pasteclass, character(1), USE.NAMES = FALSE)
  res$Ndist_val <- collapse::fndistinct.data.frame(x)
  res$N_valid <- collapse::fnobs.data.frame(x)
  res$NAs <- collapse::fnrow(x) - collapse::fnobs(x)
  res <- collapse::qTBL(res)
  title <- paste("VARLIST", deparse(substitute(x)), sep = " ")
  ifelse(tbl,
         return(collapse::qTBL(res)),
         return(tibble::view(res, title = title)))
}



