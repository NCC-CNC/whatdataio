#' @include internal.R
NULL

#' Add metadata worksheet
#'
#' This function adds a data worksheet to an Excel Workbook that contains
#' identifiers.
#'
#' @param x `Workbook` workbook object.
#'
#' @details
#' The site data worksheet is used to specify information on the
#' identifiers used for sites, actions, and features.
#'
#' @return An updated `Workbook` object.
#'
#' @noRd
add_metadata_sheet <- function(x,
                               site_ids,
                               action_ids,
                               feature_ids,
                               site_descriptions,
                               action_descriptions,
                               feature_descriptions,
                               parameters) {
  # validate arguments
  assertthat::assert_that(
    inherits(x, "Workbook"),
    is.character(site_ids),
    assertthat::noNA(site_ids),
    is.character(action_ids),
    assertthat::noNA(action_ids),
    is.character(feature_ids),
    is.character(site_descriptions),
    assertthat::noNA(site_descriptions),
    identical(length(site_ids), length(site_descriptions)),
    is.character(action_descriptions),
    assertthat::noNA(action_descriptions),
    identical(length(action_ids), length(action_descriptions)),
    is.character(feature_descriptions),
    assertthat::noNA(feature_descriptions),
    identical(length(feature_ids), length(feature_descriptions))
  )

  # create data
  n <- max(length(site_ids), length(action_ids), length(feature_ids))
  d <- tibble::tibble(
    site_ids = c(
      site_ids,
      rep("", n - length(site_ids))
    ),
    action_ids = c(
      action_ids,
      rep("", n - length(action_ids))
    ),
    feature_ids = c(
      feature_ids,
      rep("", n - length(feature_ids))
    ),
    site_descriptions = c(
      site_descriptions,
      rep("", n - length(site_descriptions))
    ),
    action_descriptions = c(
      action_descriptions,
      rep("", n - length(action_descriptions))
    ),
    feature_descriptions = c(
      feature_descriptions,
      rep("", n - length(feature_descriptions))
    )
  )

  # create sheet
  openxlsx::addWorksheet(
    x,
    sheetName = parameters$metadata_sheet$sheet_name,
    visible = !isTRUE(parameters$metadata_sheet$hidden)
  )

  # set up worksheet
  ## lock sheet
  openxlsx::protectWorksheet(
    wb = x,
    sheet = parameters$metadata_sheet$sheet_name,
    protect = TRUE,
    lockFormattingCells = FALSE,
    lockFormattingColumns = FALSE,
    lockInsertingColumns = TRUE,
    lockDeletingColumns = TRUE,
    lockInsertingRows = TRUE,
    lockDeletingRows = TRUE)

  # add data
  openxlsx::writeDataTable(
    x, parameters$metadata_sheet$sheet_name, x = d, startRow = 1
  )

  # return result
  x

}
