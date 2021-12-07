#' @include internal.R
NULL

#' Create data workbook
#'
#' This function creates an Excel Workbook with data.
#'
#' @inheritParams create_solution_workbook
#'
#' @return `Workbook` object.
#'
#' @export
create_project_workbook <- function(
  ## variables
  site_ids, site_descriptions,
  feature_ids, feature_descriptions,
  action_ids, action_descriptions,
  ## data
  site_data, feasibility_data,
  feature_data, action_expectation_data,
  ## parameter
  parameters) {
  # validate arguments
  assertthat::assert_that(
    ## ids
    is.character(site_ids), assertthat::noNA(site_ids),
    is.character(feature_ids), assertthat::noNA(feature_ids),
    is.character(action_ids), assertthat::noNA(action_ids),
    ## descriptions
    is.character(site_descriptions), assertthat::noNA(site_descriptions),
    is.character(feature_descriptions), assertthat::noNA(feature_descriptions),
    is.character(action_descriptions), assertthat::noNA(action_descriptions),
    identical(length(site_ids), length(site_descriptions)),
    identical(length(feature_ids), length(feature_descriptions)),
    identical(length(action_ids), length(action_descriptions)),
    ## input data
    inherits(site_data, "data.frame"),
    inherits(feasibility_data, "data.frame"),
    inherits(feature_data, "data.frame"),
    inherits(action_expectation_data, "list")
  )

  # create spreadsheet
  x <- openxlsx::createWorkbook()

  # add worksheets
  ## site data sheet
  x <- add_site_data_sheet(
    x = x,
    data = site_data,
    comments = template_site_comments(
      site_descriptions = site_descriptions,
      action_descriptions = action_descriptions,
      parameters = parameters
    ),
    parameters = parameters,
    n_actions = length(action_ids)
  )

  ## feasibility sheet
  x <- add_feasibility_data_sheet(
    x = x,
    data = feasibility_data,
    comments = template_feasibility_comments(
      site_descriptions = site_descriptions,
      action_descriptions = action_descriptions,
      parameters = parameters
    ),
    parameters = parameters
  )

  ## feature data sheet
  x <- add_feature_data_sheet(
    x = x,
    data = feature_data,
    comments = template_feature_comments(
      feature_descriptions = feature_descriptions,
      parameters = parameters
    ),
    parameters = parameters
  )

  ## feature expectation data sheet for each action
  for (i in seq_along(action_ids)) {
    x <- add_action_expectation_sheet(
      x = x,
      data = action_expectation_data[[i]],
      comments = template_action_expectation_comments(
        action_id = action_ids[[i]],
        site_descriptions = site_descriptions,
        feature_descriptions = feature_descriptions,
        parameters = parameters
      ),
      action_id = action_ids[i],
      parameters = parameters
    )
  }

  ## meta data sheet
  x <- add_metadata_sheet(
    x = x,
    site_ids = site_ids,
    action_ids = action_ids,
    feature_ids = feature_ids,
    site_descriptions = site_descriptions,
    feature_descriptions = feature_descriptions,
    action_descriptions = action_descriptions,
    parameters = parameters
  )

  # return result
  x
}
