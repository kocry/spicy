
#' Split up a string (with separators) into a character vector.
#'
#' Split up a string (with separators) into a character vector
#' (whitespace around separator is trimmed).
#'
#' @param ... Character string(s).
#' @param sep Pattern for separation.
#' Default is \code{"auto"}:
#' \code{,} \code{;} \code{|} \code{\\n} \code{\\t}
#' @param trim Remove whitespace from start and end of string(s)?
#' Default is \code{TRUE}.
#'
#' @return Character vector.
#' @importFrom stringr str_trim str_split
#' @export
#'
#' @examples
#' \dontrun{
#' cc("a,b,c,d,e")
#'
#' cc(" a , b , c , d , e ")
#'
#' cc(" a , b , c , d , e ", trim=FALSE)
#'
#' cc("1, 2, 3, 4, 5")
#'
#' cc("A 1 , B 2 ; C 3 | D 4 \t E 5")
#'
#' cc("A, B, C",
#'    " D | E ",
#'    c("F", "G"))
#'
#' cc("
#' American
#' British
#' Chinese
#' ")
#' }

cc = function(..., sep="auto", trim=TRUE) {
  dots = list(...)
  x = paste(sapply(dots, function(i) paste(i, collapse=",")), collapse=",")
  x = ifelse(trim, stringr::str_trim(x), x)
  sep = ifelse(sep=="auto",
               ifelse(trim, "\\s*[,;\\|\\n\\t]\\s*", "[,;\\|\\n\\t]"),
               sep)
  as.character(stringr::str_split(x, sep, simplify=TRUE))
}
