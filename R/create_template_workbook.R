#' @include internal.R
NULL

#' Create template workbook
#'
#' This function creates a data template Excel Workbook.
#'
#' @inheritParams create_export_workbook
#'
#' @inherit create_export_workbook return
#'
#' @export
create_template_workbook <- function(
  site_names, feature_names, action_names, parameters) {
  # validate arguments
  assertthat::assert_that(
    is.character(site_names), assertthat::noNA(site_names),
    is.character(feature_names), assertthat::noNA(feature_names),
    is.character(action_names), assertthat::noNA(action_names))

  # create spreadsheet
  x <- openxlsx::createWorkbook()

   # create template data
   site_data <- template_site_data(
     site_names, action_names, parameters)
   site_status_data <- template_site_status_data(
     site_names, action_names, parameters)
   site_feasibility_data <- template_site_feasibility_data(
     site_names, action_names, parameters)
   feature_data <- template_feature_data(
    feature_names, parameters)
   action_expectation_data <- lapply(
     action_names, template_action_expectation_data,
     site_names = site_names, feature_names = feature_names,
     parameters = parameters)

  # add worksheets
  ## site data sheet
  x <- add_site_data_sheet(
    x, data = site_data, parameters = parameters)

  ## site action status sheet
  x <- add_site_status_sheet(
    x, data = site_status_data, parameters = parameters)

  ## site action feasibility sheet
  x <- add_site_feasibility_sheet(
    x, data = site_feasibility_data, parameters = parameters)

  ## feature data sheet
  x <- add_feature_data_sheet(
    x, data = feature_data, parameters = parameters)

  ## feature expectation data sheet for each action
  for (i in seq_along(action_names)) {
    x <- add_action_expectation_sheet(
      x, data = action_expectation_data[[i]],
      action_name = action_names[i], parameters = parameters)
  }

  # return result
  x
}
