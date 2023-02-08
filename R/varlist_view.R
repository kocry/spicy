#' @title List of variables in the viewer
#'
#' @description List variables in the viewer, with buttons to copy, save (CSV/XLS/PDF) and print the table
#'
#' @param x a data.frame
#' @param values if values = "min_max" (default), display minimum and maximum values of columns, if values = "all", display all values of columns
#' @param to_df if TRUE, return a tibble data format; if FALSE (default), returns a data frame
#'
#' @return a data.frame
#' @importFrom dplyr first last nth n_distinct
#' @importFrom DT datatable
#' @importFrom lubridate is.POSIXct is.POSIXlt is.POSIXt is.Date
#' @importFrom stats na.omit
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#' \dontrun{
#' varlist_view(df) # df is a data frame
#' varlist_view(df, values = "all")
#' }
varlist_view <- function(x, values = c("min_max", "all"), to_df = FALSE) {
  getlab <- function(x) attributes(x)[["label"]]
  label <- sapply(x, getlab)
  variables <- colnames(x)
  varlist <- as.data.frame(cbind(variables, label))
  varlist$label[varlist$label == "NULL"] <- NA
  options(scipen = 999)
  min_max <- lapply(x, function(x) {
    if (is.factor(x)) {
      paste(levels(x), collapse = ", ")
    } else if (is.logical(x) & (length(x) == sum(is.na(x)))) {
      "Full NA"
    } else if (is.logical(x) & (length(x) > sum(is.na(x)))) {
      paste(sort(unique(stats::na.omit(x))), collapse = ", ")
    } else if (is.character(x)) {
      paste(dplyr::first(stats::na.omit(substr(x, 1, 20))), "...", dplyr::last(stats::na.omit(substr(x, 1, 20))))
    } else if (all(is.na(x))) {
      "Full NA"
    } else if (is.list(x)) {
      "Lists"
    } else if (lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x) |
               lubridate::is.POSIXt(x) | lubridate::is.Date(x)) {
      paste(min(x, na.rm = T), "...", max(x, na.rm = T))
    } else if (dplyr::n_distinct(x, na.rm = T) < 6) {
      paste(sort(unique(stats::na.omit(x))), collapse = ", ")
    } else {
      paste(paste(dplyr::nth(sort(unique(stats::na.omit(x))), 1),
                  dplyr::nth(sort(unique(stats::na.omit(x))), 2),
                  dplyr::nth(sort(unique(stats::na.omit(x))), 3),
                  dplyr::nth(sort(unique(stats::na.omit(x))), 4),
                  dplyr::nth(sort(unique(stats::na.omit(x))), 5),
                  sep = ", "
      ), " ... ", max(x, na.rm = T), sep = "")
    }
  })

  all <- lapply(x, function(x) {
    if (is.factor(x)) {
      paste(levels(x), collapse = ", ")
    } else if (is.logical(x) & (length(x) == sum(is.na(x)))) {
      "Full NA"
    } else if (is.logical(x) & (length(x) > sum(is.na(x)))) {
      paste(sort(unique(stats::na.omit(x))), collapse = ", ")
    } else if (is.character(x)) {
      paste(sort(unique(stats::na.omit(x))), collapse = ", ")
    } else if (all(is.na(x))) {
      "Full NA"
    } else if (is.list(x)) {
      "Lists"
    } else if (lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x) |
               lubridate::is.POSIXt(x) | lubridate::is.Date(x)) {
      paste(sort(unique(stats::na.omit(x))), collapse = ", ")
    } else {
      paste(sort(unique(stats::na.omit(x))), collapse = ", ")
    }
  })

  class <- lapply(x, function(x) {
    if (lubridate::is.POSIXt(x) |
        lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x)) {
      paste(class(x), collapse = ", ")
    } else if (length(class(x)) > 1) {
      paste(class(x), collapse = ", ")
    } else {
      class(x)
    }
  })

  values <- match.arg(values)

  type <- sapply(x, typeof)
  varlist$values <- switch(values,
                           min_max = min_max,
                           all = all
  )
  varlist$class <- class
  varlist$type <- type
  varlist$valid <- apply(x, 2, function(x) length(x) - sum(is.na(x)))
  varlist$na <- apply(x, 2, function(x) sum(is.na(x)))
  varlist <- as.data.frame(lapply(varlist, unlist))
  varlist <- tibble::as_tibble(varlist)
  DT::datatable(varlist,
                editable = F,
                filter = 'none',
                caption = 'List of variables',
                selection = 'none',
                extensions = list("ColReorder" = NULL,
                                  "Buttons" = NULL,
                                  "KeyTable" = NULL,
                                  "FixedHeader" =NULL,
                                  "Select" = TRUE),
                options = list(dom = 'Blfrtip',
                               autoWidth=TRUE,
                               pageLength = 10,
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               colReorder = TRUE,
                               keys = TRUE,
                               searchHighlight = TRUE,
                               fixedHeader = TRUE
                )
  )
}
