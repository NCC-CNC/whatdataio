#' @include internal.R
NULL

#' Add metadata worksheet
#'
#' This function adds a data worksheet to an Excel Workbook that contains
#' identifiers.
#'
#' @param x `Workbook` workbook object.
#'
#' @param site_ids
#'
#' @param comments `data.frame` object with template comments.
#'
#' @details
#' The site data worksheet is used to specify information on the
#' identifiers used for sites, actions, and features.
#'
#' @return An updated `Workbook` object.
#'
#' @noRd
add_meta_data_sheet <- function(x, site_ids, action_ids, feature_ids,
                                hidden = TRUE) {
  # validate arguments
  assertthat::assert_that(
    inherits(x, "Workbook"),
    is.character(site_ids),
    assertthat::noNA(site_ids),
    is.character(action_ids),
    assertthat::noNA(action_ids),
    is.character(feature_ids),
    assertthat::noNA(feature_ids),
    assertthat::is.flag(hidden),
    assertthat::noNA(hidden)
  )

  # create data
  n <- max(length(site_ids), length(action_ids), length(feature_ids))
  d <- tibble::tibble(
    site_ids = c(site_ids, rep("", n - length(site_ids))),
    action_ids = c(action_ids, rep("", n - length(action_ids))),
    feature_ids = c(feature_ids, rep("", n - length(feature_ids)))
  )

  # create sheet
  openxlsx::addWorksheet(x, sheetName = "meta", visible = !isTRUE(hidden))

  # set up worksheet
  ## lock sheet
  openxlsx::protectWorksheet(x, "meta", protect = TRUE,
    lockFormattingCells = FALSE, lockFormattingColumns = FALSE,
    lockInsertingColumns = TRUE, lockDeletingColumns = TRUE,
    lockInsertingRows = TRUE, lockDeletingRows = TRUE)

  # add data
  openxlsx::writeDataTable(x, "meta", x = d, startRow = 1)

  # return result
  x

}
