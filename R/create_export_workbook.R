#' @include internal.R
NULL

#' Create export workbook
#'
#' This function creates an Excel Workbook with results.
#'
#' @param site_names `character` names of sites. No missing
#'   (`NA`) values are permitted. This object must contain at least one
#'   name.
#'
#' @param feature_names `character` names of biodiversity features. No missing
#'   (`NA`) values are permitted. This object must contain at least one
#'    name.
#'
#' @param action_names `character` names of management actions. No missing
#'   (`NA`) values are permitted. This object must contain at least one
#'   name.
#'
#' @param site_data `data.frame` containing site data.
#'
#' @param site_status_data `data.frame`containing site status data.
#'
#' @param feature_data `data.frame` containing feature data.
#'
#' @param action_expectation_data `data.frame` containing expectation data.
#'
#' @param summary_results_data `data.frame` containing summary results data.
#'
#' @param site_results_data `data.frame` containing site results data.
#'
#' @param feature_results_data `data.frame` containing feature results data.
#'
#' @param parameters `list` object containing parameters to customize
#'  appearance of worksheet.
#'
#' @return `Workbook` object.
#'
#' @export
create_export_workbook <- function(
  site_names, feature_names, action_names,
  site_data, site_status_data, feature_data, action_expectation_data,
  summary_results_data, site_results_data, feature_results_data,
  parameters) {
  # validate arguments
  assertthat::assert_that(
    is.character(site_names), assertthat::noNA(site_names),
    is.character(feature_names), assertthat::noNA(feature_names),
    is.character(action_names), assertthat::noNA(action_names),
    inherits(site_data, "data.frame"),
    inherits(site_status_data, "data.frame"),
    inherits(feature_data, "data.frame"),
    inherits(action_expectation_data, "list"),
    inherits(summary_results_data, "data.frame"),
    inherits(site_results_data, "data.frame"),
    inherits(feature_results_data, "data.frame"))

  # create spreadsheet
  x <- openxlsx::createWorkbook()

  # add worksheets
  ## site data sheet
  x <- add_site_data_sheet(
    x, data = site_data, parameters = parameters)

  ## site action status  sheet
  x <- add_site_status_sheet(
    x, data = site_status_data, parameters = parameters)

  ## feature data sheet
  x <- add_feature_data_sheet(
    x, data = feature_data, parameters = parameters)

  ## feature expectation data sheet for each action
  for (i in seq_along(action_names)) {
    x <- add_action_expectation_sheet(
      x, data = action_expectation_data[[i]],
      action_name = action_names[i], parameters = parameters)
  }

  ## add summary results worksheet
  x <- add_summary_results_sheet(
    x, data = summary_results_data, parameters = parameters)

  ## add site results worksheet
  x <- add_site_results_sheet(
    x, data = site_results_data, parameters = parameters)

  ## add feature results worksheet
  x <- add_feature_results_sheet(
    x, data = feature_results_data, parameters = parameters)

  # return result
  x
}
