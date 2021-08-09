#' @include internal.R
NULL

#' Read spreadsheet data
#'
#' Read prioritization input data from an Excel Spreadsheet. Note that
#' this function assumes that the data are formatted correctly, following
#' the NCC Data App.
#'
#' @param x `character` file path.
#'
#' @param parameters `list` configuration parameters for
#'
#' @return `list` object.
#'
#' @export
read_spreadsheet_data <- function(x, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    file.exists(x),
    is.list(parameters))
  # load workbook
  w <- openxlsx::loadWorkbook(x, xlsxFile = NULL, isUnzipped = FALSE)

  # extract parameters
  site_data_sheet_name <- parameters$site_data_sheet$sheet_name
  site_status_sheet_name <- parameters$site_status_sheet$sheet_name
  feature_data_sheet_name <- parameters$feature_data_sheet$sheet_name
  action_expectation_sheet_names <-
    setdiff(names(w), c(site_data_sheet_name, site_status_sheet_name,
                        feature_data_sheet_name))

  # import data
  ## site data
  site_data <- suppressWarnings(openxlsx::read.xlsx(
    x, sheet = site_data_sheet_name, colNames = TRUE, startRow = 3))
  ## site status data
  site_status_data <- suppressWarnings(openxlsx::read.xlsx(
    x, sheet = site_status_sheet_name, colNames = TRUE, startRow = 3))
  ## feature data
  feature_data <- suppressWarnings(openxlsx::read.xlsx(
    x, sheet = feature_data_sheet_name, colNames = TRUE, startRow = 3))
  ## action expectation data
  action_expectation_data <- lapply(
    action_expectation_sheet_names,
    function(y) {
      suppressWarnings(openxlsx::read.xlsx(
        x, sheet = y, colNames = TRUE, startRow = 3))
    })

  # convert data to tibble format
  site_data <- tibble::as_tibble(site_data)
  site_status_data <- tibble::as_tibble(site_status_data)
  feature_data <- tibble::as_tibble(feature_data)
  action_expectation_data <- lapply(action_expectation_data, tibble::as_tibble)

  # extract column names
  site_data_names <-
    unlist(
      openxlsx::read.xlsx(
        w, sheet = site_data_sheet_name, colNames = FALSE,
        cols = seq_len(ncol(site_data)), rows = 3),
      recursive = TRUE, use.names = FALSE)
  site_status_data_names <-
    unlist(
      openxlsx::read.xlsx(
        w, sheet = site_status_sheet_name, colNames = FALSE,
        cols = seq_len(ncol(site_data)), rows = 3),
      recursive = TRUE, use.names = FALSE)
  feature_data_names <-
    unlist(
      openxlsx::read.xlsx(
        w, sheet = feature_data_sheet_name, colNames = FALSE,
        cols = seq_len(ncol(site_data)), rows = 3),
      recursive = TRUE, use.names = FALSE)
  action_expectation_data_names <-
    unlist(
      openxlsx::read.xlsx(
        w, sheet = action_expectation_sheet_names[[1]],
        colNames = FALSE, cols = seq_len(ncol(site_data)), rows = 3),
      recursive = TRUE, use.names = FALSE)

  # fix column names
  names(site_data) <- site_data_names
  names(site_status_data) <- site_status_data_names
  names(feature_data) <- feature_data_names
  action_expectation_data <- lapply(action_expectation_data, function(x) {
    names(x) <- action_expectation_data_names
    x
  })

  # ensure that correct data types
  ## site data
  site_data[[1]] <- as.character(site_data[[1]])
  for (i in seq(2, ncol(site_data))) {
    site_data[[i]] <- as.numeric(site_data[[i]])
  }

  ## site status data
  site_status_data[[1]] <- as.character(site_status_data[[1]])
  for (i in seq(2, ncol(site_status_data))) {
    site_status_data[[i]] <- as.numeric(site_status_data[[i]])
  }

  ## feature data
  feature_data[[1]] <- as.character(feature_data[[1]])
  for (i in seq(2, ncol(feature_data))) {
    feature_data[[i]] <- as.numeric(feature_data[[i]])
  }

  ## action expectation data
  action_expectation_data <- lapply(
    action_expectation_data, function(x) {
      x[[1]] <- as.character(x[[1]])
      for (i in seq(2, ncol(x))) {
        x[[i]] <- as.numeric(x[[i]])
      }
      x
    })

  # set names for list
  names(action_expectation_data) <-
    paste0("action_", seq_along(action_expectation_data))

  # extract action names
  action_names <- unlist(
    unglue::unglue(
      action_expectation_sheet_names,
      parameters$action_expectation_sheet$sheet_name),
    recursive = TRUE, use.names = FALSE)

  # return result
  list(
    site_names =
      site_data[[parameters$site_data_sheet$name_header]],
    feature_names =
      feature_data[[parameters$feature_data_sheet$name_header]],
    action_names = action_names,
    site_data = site_data,
    site_status_data = site_status_data,
    feature_data = feature_data,
    action_expectation_data = action_expectation_data)
}