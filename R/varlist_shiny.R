#' @title List of variables in a table widget in shiny
#'
#' @description List variables in a table widget in shiny, with buttons to copy, save (as CSV/XLS/PDF) and print the table
#'
#' @param x a data frame
#' @param values if values = FALSE (default), display minimum and maximum values of columns. If values = TRUE, display all values of columns.
#'
#' @return a data frame
#' @importFrom collapse vlabels funique na_omit ffirst flast fndistinct fmax fndistinct.data.frame fnobs.data.frame fnrow fnobs qTBL
#' @importFrom dplyr nth
#' @importFrom DT DTOutput renderDT
#' @importFrom lubridate is.POSIXct is.POSIXlt is.POSIXt is.Date
#' @importFrom shiny shinyApp fluidPage
#' @export
#'
#' @examples
#' \dontrun{
#' varlist_shiny(df) # df is a data frame
#' varlist_shiny(df, values = "all")
#' }

varlist_shiny <- function(x, values = FALSE) {

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
  shiny::shinyApp(
    ui = shiny::fluidPage(DT::DTOutput('tbl')),
    server = function(input, output) {
      output$tbl = DT::renderDT(res,
                                server = FALSE,
                                editable = F,
                                # caption = "List of variables",
                                filter = "none",
                                selection = "none",
                                extensions = list("ColReorder" = TRUE,
                                                  "Buttons" = TRUE,
                                                  "KeyTable" = NULL,
                                                  "FixedHeader" = TRUE,
                                                  "Select" = TRUE,
                                                  "Responsive" = TRUE),
                                options = list(dom = "Blfrtip",
                                               fixedHeader = TRUE,
                                               autoWidth = F,
                                               pageLength = 10,
                                               info = FALSE,
                                               # lengthMenu = list(c(10, 20, 50, 100, -1), c("10","20", "50", "100", "All")),
                                               buttons = list("copy", "print", "csv", "excel", "pdf"),
                                               colReorder = TRUE,
                                               keys = TRUE,
                                               searchHighlight = TRUE,
                                               columnDefs = list(list(targets = 4, width = '10px'),
                                                                 list(targets = 5, width = '10px'),
                                                                 list(targets = 6, width = '5px'),
                                                                 list(targets = 7, width = '5px'),
                                                                 list(searchPanes = list(show = FALSE), targets = 1:4)),
                                               scrollX = TRUE
                                )
      )
    }
  )
}
