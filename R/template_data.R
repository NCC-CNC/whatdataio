#' @include internal.R
NULL

#' Template site data
#'
#' Create site data for the template workbook.
#'
#' @inheritParams create_template_workbook
#'
#' @return `data.frame` object.
#'
#' @noRd
template_site_data <- function(site_names, action_names, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_names), assertthat::noNA(site_names),
    is.character(action_names), assertthat::noNA(action_names),
    is.list(parameters))
  # extract parameters
  p <- parameters$site_data_sheet
  # create data
  d <- tibble::tibble(site_name = site_names, longitude = "", latitude = "")
  names(d) <- c(p$name_header, p$longitude_header, p$latitude_header)
  d2 <- tibble::as_tibble(as.data.frame(matrix(
      NA_real_, ncol = length(action_names))))
  names(d2) <- as.character(glue::glue(
      p$action_cost_header, action_names = action_names))
  d <- dplyr::bind_cols(d, d2)
  # return data
  d
}

#' Template site status data
#'
#' Create site status data for the template workbook.
#'
#' @inheritParams create_template_workbook
#'
#' @return `data.frame` object.
#'
#' @noRd
template_site_status_data <- function(site_names, action_names, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_names), assertthat::noNA(site_names),
    is.character(action_names), assertthat::noNA(action_names),
    is.list(parameters))
  # extract parameters
  p <- parameters$site_status_sheet
  # create data
  d <- tibble::tibble(site_name = site_names)
  names(d) <- c(p$name_header)
  d2 <- tibble::as_tibble(as.data.frame(matrix(
      1L, ncol = length(action_names))))
  names(d2) <- as.character(glue::glue(
      p$action_status_header, action_names = action_names))
  d <- dplyr::bind_cols(d, d2)
  # return data
  d
}

#' Template feature data
#'
#' Create feature data for the template workbook.
#'
#' @inheritParams create_template_workbook
#'
#' @return `data.frame` object.
#'
#' @noRd
template_feature_data <- function(feature_names, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(feature_names), assertthat::noNA(feature_names),
    is.list(parameters))
  # extract parameters
  p <- parameters$feature_data_sheet
  # create data
  d <- tibble::tibble(feature_name = feature_names, target = "", weight = "")
  names(d) <- c(p$name_header, p$target_header, p$weight_header)
  # return data
  d
}

#' Template action expectation data
#'
#' Create action expectation data for the template workbook.
#'
#' @param action_name `character` name of the action.
#'
#' @inheritParams create_export_workbook
#'
#' @return `data.frame` object.
#'
#' @noRd
template_action_expectation_data <- function(
  site_names, feature_names, action_name, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_names), assertthat::noNA(site_names),
    is.character(feature_names), assertthat::noNA(feature_names),
    assertthat::is.string(action_name), assertthat::noNA(action_name),
    is.list(parameters))
  # extract parameters
  p <- parameters$action_expectation_sheet
  # create data
  d <- tibble::tibble(site_name = site_names)
  names(d) <- c(p$name_header)
  d2 <- tibble::as_tibble(as.data.frame(matrix(
    NA_real_, ncol = length(feature_names))))
  names(d2) <- as.character(glue::glue(
      p$action_expectation_header, feature_names = feature_names))
  d <- dplyr::bind_cols(d, d2)
  # return data
  d
}
