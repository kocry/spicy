#' @title Copy in the clipboard
#'
#' @description Copy in the clipboard a data frame from View's RStudio to Excel
#'
#' @param x a data frame, matrix, array (2d) or table
#' @param row.names either a logical value (default TRUE) indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.
#' @param col.names either a logical value (default NA) indicating whether the column names of x are to be written along with x, or a character vector of column names to be written.
#' @param ... parameters passed to other methods.
#'
#' @return a data frame in the clipboard
#' @export
#'
#' @examples
#' \dontrun{
#' copy_write_excel(x)
#' }

copy_write_excel <- function(x, row.names = TRUE, col.names = NA,...) {
  utils::write.table(x,"clipboard-1000000", sep="\t", row.names = row.names, col.names = col.names,...)}
