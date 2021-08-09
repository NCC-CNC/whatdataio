#' Application parameters
#'
#' Load application parameters for testing functions.
#'
#' @return `list` object with parameters.
#'
#' @export
app_parameters <- function() {
  path <- system.file("extdata", "config.toml", package = "whatdata")
  RcppTOML::parseTOML(path)
}
