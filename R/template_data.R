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
template_site_data <- function(site_ids, action_ids, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_ids), assertthat::noNA(site_ids),
    is.character(action_ids), assertthat::noNA(action_ids),
    is.list(parameters))
  # extract parameters
  p <- parameters$site_data_sheet
  # create data
  d <- tibble::tibble(site_id = site_ids, longitude = "", latitude = "")
  names(d) <- c(p$name_header, p$longitude_header, p$latitude_header)
  d2 <- tibble::as_tibble(as.data.frame(matrix(
      NA_real_, ncol = length(action_ids))))
  names(d2) <- as.character(glue::glue(
      p$action_cost_header, action_ids = action_ids))
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
template_site_status_data <- function(site_ids, action_ids, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_ids), assertthat::noNA(site_ids),
    is.character(action_ids), assertthat::noNA(action_ids),
    is.list(parameters))
  # extract parameters
  p <- parameters$site_status_sheet
  # create data
  d <- tibble::tibble(site_id = site_ids)
  names(d) <- c(p$name_header)
  d2 <- tibble::as_tibble(as.data.frame(matrix(
    0L, ncol = length(action_ids))))
  names(d2) <- as.character(glue::glue(
      p$action_status_header, action_ids = action_ids))
  d <- dplyr::bind_cols(d, d2)
  # return data
  d
}

#' Template site feasibility data
#'
#' Create site feasibility data for the template workbook.
#'
#' @inheritParams create_template_workbook
#'
#' @return `data.frame` object.
#'
#' @noRd
template_site_feasibility_data <- function(site_ids, action_ids, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_ids), assertthat::noNA(site_ids),
    is.character(action_ids), assertthat::noNA(action_ids),
    is.list(parameters))
  # extract parameters
  p <- parameters$site_feasibility_sheet
  # create data
  d <- tibble::tibble(site_id = site_ids)
  names(d) <- c(p$name_header)
  d2 <- tibble::as_tibble(as.data.frame(matrix(
      1L, ncol = length(action_ids))))
  names(d2) <- as.character(glue::glue(
      p$action_status_header, action_ids = action_ids))
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
template_feature_data <- function(feature_ids, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(feature_ids), assertthat::noNA(feature_ids),
    is.list(parameters))
  # extract parameters
  p <- parameters$feature_data_sheet
  # create data
  d <- tibble::tibble(feature_id = feature_ids, target = "", weight = "")
  names(d) <- c(p$name_header, p$target_header, p$weight_header)
  # return data
  d
}

#' Template action expectation data
#'
#' Create action expectation data for the template workbook.
#'
#' @param action_id `character` id of the action.
#'
#' @inheritParams create_export_workbook
#'
#' @return `data.frame` object.
#'
#' @noRd
template_action_expectation_data <- function(
  site_ids, feature_ids, action_id, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_ids), assertthat::noNA(site_ids),
    is.character(feature_ids), assertthat::noNA(feature_ids),
    assertthat::is.string(action_id), assertthat::noNA(action_id),
    is.list(parameters))
  # extract parameters
  p <- parameters$action_expectation_sheet
  # create data
  d <- tibble::tibble(site_id = site_ids)
  names(d) <- c(p$name_header)
  d2 <- tibble::as_tibble(as.data.frame(matrix(
    NA_real_, ncol = length(feature_ids))))
  names(d2) <- as.character(glue::glue(
      p$action_expectation_header, feature_ids = feature_ids))
  d <- dplyr::bind_cols(d, d2)
  # return data
  d
}
