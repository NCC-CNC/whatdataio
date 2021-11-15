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
create_template_workbook <- function(site_ids, site_descriptions,
                                     feature_ids, feature_descriptions,
                                     action_ids, action_descriptions,
                                     parameters) {
  # validate arguments
  assertthat::assert_that(
    is.character(site_ids), assertthat::noNA(site_ids),
    is.character(site_descriptions), assertthat::noNA(site_descriptions),
    is.character(feature_ids), assertthat::noNA(feature_ids),
    is.character(feature_descriptions), assertthat::noNA(feature_descriptions),
    is.character(action_ids), assertthat::noNA(action_ids),
    is.character(action_descriptions), assertthat::noNA(action_descriptions)
  )

  # create spreadsheet
  x <- openxlsx::createWorkbook()

   # create template data
   site_data <- template_site_data(
     site_ids = site_ids,
    action_ids = action_ids,
    parameters = parameters)
   site_status_data <- template_site_status_data(
     site_ids = site_ids,
     action_ids = action_ids,
     parameters = parameters
   )
   site_feasibility_data <- template_site_feasibility_data(
     site_ids = site_ids,
     action_ids = action_ids,
     parameters = parameters
   )
   feature_data <- template_feature_data(
     feature_ids = feature_ids,
     parameters = parameters
   )
   action_expectation_data <- lapply(
     action_ids, template_action_expectation_data,
     site_ids = site_ids, feature_ids = feature_ids,
     parameters = parameters
   )

   # create template comments (these contain the names)
   site_comments <- template_site_comments(
     site_descriptions = site_descriptions,
     action_descriptions = action_descriptions,
     parameters = parameters
   )
   site_status_comments <- template_site_status_comments(
     site_descriptions = site_descriptions,
     action_descriptions = action_descriptions,
     parameters = parameters
   )
   site_feasibility_comments <- template_site_feasibility_comments(
     site_descriptions = site_descriptions,
     action_descriptions = action_descriptions,
     parameters = parameters)
   feature_comments <- template_feature_comments(
     feature_descriptions = feature_descriptions,
     parameters = parameters
   )
   action_expectation_comments <- lapply(
     action_descriptions, template_action_expectation_comments,
     site_descriptions = site_descriptions,
     feature_descriptions = feature_descriptions,
     parameters = parameters)

  # add worksheets
  ## site data sheet
  x <- add_site_data_sheet(
    x = x,
    data = site_data,
    comments = site_comments,
    parameters = parameters
  )

  ## site action status sheet
  x <- add_site_status_sheet(
    x = x,
    data = site_status_data,
    comments = site_status_comments,
    parameters = parameters
  )

  ## site action feasibility sheet
  x <- add_site_feasibility_sheet(
    x = x,
    data = site_feasibility_data,
    comments = site_feasibility_comments,
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

  # return result
  x
}
