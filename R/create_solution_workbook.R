#' @include internal.R
NULL

#' Create solution workbook
#'
#' This function creates an Excel Workbook with data and results.
#'
#' @param site_ids `character` identifiers for sites.
#'   No missing (`NA`) values are permitted. This object must contain at least
#'   one value.
#'
#' @param feature_ids `character` identifiers for biodiversity features.
#'   No missing (`NA`) values are permitted. This object must contain at least
#'   one value.
#'
#' @param action_ids `character` identifiers for management actions.
#'   No missing (`NA`) values are permitted. This object must contain at least
#'   one value.
#'
#' @param site_descriptions `character` descriptions of sites.
#'   No missing (`NA`) values are permitted. This object must contain at least
#'   one value.
#'
#' @param feature_descriptions `character` descriptions of biodiversity
#'   features.
#'   No missing (`NA`) values are permitted. This object must contain at least
#'   one value.
#'
#' @param action_descriptions `character` descriptions of management actions.
#'   No missing (`NA`) values are permitted. This object must contain at least
#'   one value.
#'
#' @param site_data `data.frame` containing site data.
#'
#' @param feasibility_data `data.frame`containing feasibility data.
#'
#' @param feature_data `data.frame` containing feature data.
#'
#' @param action_expectation_data `list` of `data.frame` objects
#'   containing expectation data.
#'
#' @param summary_results_data `data.frame` containing summary results data.
#'
#' @param site_results_data `data.frame` containing site results data.
#'
#' @param feature_results_data `data.frame` containing feature results data.
#'
#' @param summary_results_comments `data.frame` containing summary results
#'  comments.
#'  Defaults to `NULL` such that no comments are added.
#'
#' @param site_results_comments `data.frame` containing site results comments.
#'  Defaults to `NULL` such that no comments are added.
#'
#' @param feature_results_comments `data.frame` containing feature results
#'  comments.
#'  Defaults to `NULL` such that no comments are added.
#'
#' @param parameters `list` object containing parameters to customize
#'  appearance of worksheet.
#'
#' @return `Workbook` object.
#'
#' @export
create_solution_workbook <- function(
  ## variables
  site_ids, site_descriptions,
  feature_ids, feature_descriptions,
  action_ids, action_descriptions,
  ## data
  site_data, feasibility_data,
  feature_data, action_expectation_data,
  ## results
  summary_results_data, site_results_data, feature_results_data,
  ## results comments
  summary_results_comments = NULL,
  site_results_comments = NULL,
  feature_results_comments = NULL,
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
    ## results data
    inherits(summary_results_data, "data.frame"),
    inherits(site_results_data, "data.frame"),
    inherits(feature_results_data, "data.frame")
  )
  if (!is.null(summary_results_comments)) {
    assertthat::assert_that(
      inherits(summary_results_comments, "data.frame"),
      identical(dim(summary_results_data), dim(summary_results_comments))
    )
  }
  if (!is.null(site_results_comments)) {
    assertthat::assert_that(
      inherits(site_results_comments, "data.frame"),
      identical(dim(site_results_data), dim(site_results_comments))
    )
  }
  if (!is.null(feature_results_comments)) {
    assertthat::assert_that(
      inherits(feature_results_comments, "data.frame"),
      identical(dim(feature_results_data), dim(feature_results_comments))
    )
  }

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

  ## add summary results worksheet
  x <- add_summary_results_sheet(
    x = x,
    data = summary_results_data,
    comments = summary_results_comments,
    parameters = parameters
  )

  ## add site results worksheet
  x <- add_site_results_sheet(
    x = x,
    data = site_results_data,
    comments = site_results_comments,
    parameters = parameters
  )

  ## add feature results worksheet
  x <- add_feature_results_sheet(
    x = x,
    data = feature_results_data,
    comments = feature_results_comments,
    parameters = parameters
  )

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
