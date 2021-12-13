#' @include internal.R
NULL

#' Template site comments
#'
#' Create site comments for the template workbook.
#'
#' @inheritParams create_solution_workbook
#'
#' @return `data.frame` object.
#'
#' @noRd
template_site_comments <- function(site_descriptions,
                                   action_descriptions,
                                   parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_descriptions), assertthat::noNA(site_descriptions),
    is.character(action_descriptions), assertthat::noNA(action_descriptions),
    is.list(parameters))
  # extract parameters
  p <- parameters$site_data_sheet
  # create data
  d <- tibble::tibble(
    site_id = site_descriptions,
    longitude = NA_character_,
    latitude = NA_character_,
    status = NA_character_
  )
  names(d) <- c(
    p$name_header, p$longitude_header, p$latitude_header, p$status_header
  )
  d2 <- tibble::as_tibble(as.data.frame(matrix(
      NA_real_, ncol = length(action_descriptions))))
  names(d2) <- as.character(action_descriptions)
  d <- dplyr::bind_cols(d, d2)
  # return data
  d
}

#' Template feasibility comments
#'
#' Create feasibility comments for the template workbook.
#'
#' @inheritParams create_template_workbook
#'
#' @return `data.frame` object.
#'
#' @noRd
template_feasibility_comments <- function(site_descriptions,
                                          action_descriptions,
                                          parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_descriptions), assertthat::noNA(site_descriptions),
    is.character(action_descriptions), assertthat::noNA(action_descriptions),
    is.list(parameters))
  # extract parameters
  p <- parameters$feasibility_data_sheet
  # create data
  d <- tibble::tibble(site_id = site_descriptions)
  names(d) <- c(p$name_header)
  d2 <- tibble::as_tibble(as.data.frame(matrix(
      NA_integer_, ncol = length(action_descriptions))))
  names(d2) <- as.character(action_descriptions)
  d <- dplyr::bind_cols(d, d2)
  # return data
  d
}

#' Template feature data
#'
#' Create feature comments for the template workbook.
#'
#' @inheritParams create_template_workbook
#'
#' @return `data.frame` object.
#'
#' @noRd
template_feature_comments <- function(feature_descriptions, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(feature_descriptions), assertthat::noNA(feature_descriptions),
    is.list(parameters))
  # extract parameters
  p <- parameters$feature_data_sheet
  # create data
  d <- tibble::tibble(
    feature_id = feature_descriptions,
    goal = NA_character_,
    weight = NA_character_
  )
  names(d) <- c(p$name_header, p$goal_header, p$weight_header)
  # return data
  d
}

#' Template action expectation data
#'
#' Create action expectation comments for the template workbook.
#'
#' @param action_id `character` id of the action.
#'
#' @inheritParams create_solution_workbook
#'
#' @return `data.frame` object.
#'
#' @noRd
template_action_expectation_comments <- function(
  site_descriptions, feature_descriptions, action_id, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_descriptions), assertthat::noNA(site_descriptions),
    is.character(feature_descriptions), assertthat::noNA(feature_descriptions),
    assertthat::is.string(action_id), assertthat::noNA(action_id),
    is.list(parameters))
  # extract parameters
  p <- parameters$action_expectation_sheet
  # create data
  d <- tibble::tibble(site_id = site_descriptions)
  names(d) <- c(p$name_header)
  d2 <- tibble::as_tibble(as.data.frame(matrix(
    NA_real_, ncol = length(feature_descriptions))))
  names(d2) <- as.character(feature_descriptions)
  d <- dplyr::bind_cols(d, d2)
  # return data
  d
}
