#' Column widths
#
#' Find column widths for a table.
#'
#' @param x `data.frame` object.
#'
#' @return `integer` value.
#'
#' @noRd
column_widths <- function(x) {
  assertthat::assert_that(inherits(x, "data.frame"))
  n <- nchar(names(x))
  n2 <- vapply(x, FUN.VALUE = numeric(1), function(y) {
    y[is.na(y)] <- 5
    max(nchar(as.character(y)))
  })
  pmax(n, n2)
}
