#' Read data configuration
#'
#' Load data format configuration parameters.
#'
#' @return `list` object with parameters.
#'
#' @export
read_data_configuration <- function() {
  path <- system.file("extdata", "config.toml", package = "whatdataio")
  RcppTOML::parseTOML(path)
}
