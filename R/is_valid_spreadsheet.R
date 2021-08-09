#' @include internal.R
NULL

#' Is validate spreadsheet?
#'
#' Verify if an Excel Spreadsheet is in the correct format.
#'
#' @param x `character` file path.
#'
#' @param parameters `list` configuration parameters.
#'
#' @return `logical` indicating if file path contains valid data.
#'
#' @export
is_valid_spreadsheet <- function(x, parameters) {
  # assert arguments are valid
  if (!isTRUE(assertthat::is.string(x) &&
              assertthat::noNA(x) &&
              file.exists(x))) {
    return(FALSE)
 }
  # import data
  w <- openxlsx::loadWorkbook(x, xlsxFile = NULL, isUnzipped = FALSE)
  # verify if the spreadsheet was created by the NCC Data App
  # to achieve this, we will check if the cell contents in B2
  # match the help text from the parameters
  suppressWarnings({
    cell <- openxlsx::read.xlsx(x, sheet = names(w)[[1]], colNames = FALSE,
                                cols = 2, rows = 2)[[1]]
  })
  # see if cell matches expected text
  identical(cell, parameters$site_data_sheet$sub_message)
}
