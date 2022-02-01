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

#' Extract strings
#'
#' Extract valid strings from a `character` vector.
#'
#' @param x `character` vector
#'
#' @details
#' Valid strings are `character` values that are
#' (i) not missing (`NA`) values and
#' (ii) not empty (`""`) values.
#'
#' @return A `character` vector of valid strings from `x`.
#'
#' @noRd
extract_strings <- function(x) {
  assertthat::assert_that(is.character(x))
  x[which(nchar(x) > 0 & !is.na(x))]
}
