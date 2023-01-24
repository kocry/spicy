
#' Print strings with rich formats and colors.
#'
#' @describeIn Print Paste and print strings.
#'
#' @description
#' Be frustrated with \code{print()} and \code{cat()}? Try \code{Print()}!
#' Run examples to see what it can do.
#'
#' @details
#' Possible formats/colors that can be used in \code{"<< >>"} include:
#'
#' (1) bold, italic, underline, reset, blurred, inverse, hidden, strikethrough;
#'
#' (2) black, white, silver, red, green, blue, yellow, cyan, magenta;
#'
#' (3) bgBlack, bgWhite, bgRed, bgGreen, bgBlue, bgYellow, bgCyan, bgMagenta.
#'
#' See more details in \code{\link[glue:glue]{glue::glue()}} and \code{\link[glue:glue]{glue::glue_col()}}.
#'
#' @param ... Character strings enclosed by \code{"{ }"} will be evaluated as R code.
#'
#' Character strings enclosed by \code{"<< >>"} will be printed as formatted and colored text.
#'
#' Long strings are broken by line and concatenated together.
#'
#' Leading whitespace and blank lines from the first and last lines are automatically trimmed.
#'
#' @return Formatted text.
#' @importFrom glue glue glue_col
#' @export
#'
#' @examples
#' \dontrun{
#' name = "Doe"
#' Print("My name is <<underline <<bold {name}>>>>.
#'        <<bold <<blue Pi = {pi:.15}.>>>>
#'        <<italic <<green 1 + 1 = {1 + 1}.>>>>
#'        sqrt({x}) = <<red {sqrt(x):.3}>>", x=10)
#' }

#' @importFrom glue glue glue_col
Print = function(...) {
  tryCatch({
    output = glue::glue(..., .transformer=sprintf_transformer, .envir=parent.frame())
    output_color = glue::glue_col( gsub("<<", "{", gsub(">>", "}", output)) )
    print(output_color)
  }, error = function(e) {
    warning(e)
    print(...)
  })
}


#' Paste strings
#'
#' @param ... parameters passed to other methods.
#'
#' @importFrom glue glue glue_col
#' @export
#'

Glue = function(...) {
  output = glue::glue(..., .transformer=sprintf_transformer, .envir=parent.frame())
  output_color = glue::glue_col( gsub("<<", "{", gsub(">>", "}", output)) )
  return(output_color)
}


sprintf_transformer = function(text, envir) {
  text = glue::glue(text, .envir=envir)
  m = regexpr(":.+$", text)
  if(m!=-1) {
    format = substring(regmatches(text, m), 2)
    regmatches(text, m) = ""
    res = eval(parse(text=text, keep.source=FALSE), envir)
    do.call(sprintf, list(glue("%{format}f"), res))
  } else {
    eval(parse(text=text, keep.source=FALSE), envir)
  }
}

