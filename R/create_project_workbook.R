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
  ## data comments
  site_comments, feasibility_comments,
  feature_comments, action_expectation_comments,
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
    inherits(action_expectation_data, "list"),
    ## input comments
    inherits(site_comments, "data.frame"),
    inherits(feasibility_comments, "data.frame"),
    inherits(feature_comments, "data.frame"),
    inherits(action_expectation_comments, "list"),
    identical(dim(site_data), dim(site_comments)),
    identical(dim(feasibility_data), dim(feasibility_comments)),
    identical(dim(feature_data), dim(feature_comments)),
    identical(dim(action_expectation_data), dim(action_expectation_comments)),
    identical(dim(site_data), dim(site_comments))
  )

  # create spreadsheet
  x <- openxlsx::createWorkbook()

  # add worksheets
  ## site data sheet
  x <- add_site_data_sheet(
    x = x,
    data = site_data,
    comments = site_comments,
    parameters = parameters,
    n_actions = length(action_ids)
  )

  ## feasibility sheet
  x <- add_feasibility_data_sheet(
    x = x,
    data = feasibility_data,
    comments = feasibility_comments,
    parameters = parameters
  )

  ## feature data sheet
  x <- add_feature_data_sheet(
    x = x,
    data = feature_data,
    comments = feature_comments,
    parameters = parameters
  )

  ## feature expectation data sheet for each action
  for (i in seq_along(action_ids)) {
    x <- add_action_expectation_sheet(
      x = x,
      data = action_expectation_data[[i]],
      comments = action_expectation_comments[[i]],
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
