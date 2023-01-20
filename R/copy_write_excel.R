#' Copy in the clipboard a data frame from View's RStudio to Excel
#'
#' @param x a data frame or matrix
#' @param row.names either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.
#' @param col.names either a logical value indicating whether the column names of x are to be written along with x, or a character vector of column names to be written.
#' @param ...
#'
#' @return a data frame in the clipboard
#' @export
#'
#' @examples
#' \dontrun{
#' copy_write_excel(x)
#' }

copy_write_excel <- function(x, row.names = FALSE, col.names = TRUE,...) {
  utils::write.table(x,"clipboard-1000000", sep="\t", row.names = row.names, col.names = col.names,...)}
