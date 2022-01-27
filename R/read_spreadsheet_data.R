#' @include internal.R
NULL

#' Read spreadsheet data
#'
#' Read prioritization input data from an Excel Spreadsheet. Note that
#' this function assumes that the data are formatted correctly, following
#' the NCC Data App.
#'
#' @param x `character` file path.
#'
#' @param parameters `list` configuration parameters for
#'
#' @return `list` object.
#'
#' @export
read_spreadsheet_data <- function(x, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    file.exists(x),
    is.list(parameters))
  # load workbook
  w <- openxlsx::loadWorkbook(x, xlsxFile = NULL, isUnzipped = FALSE)

  # extract parameters
  site_data_sheet_name <- parameters$site_data_sheet$sheet_name
  feasibility_data_sheet_name <- parameters$feasibility_data_sheet$sheet_name
  feature_data_sheet_name <- parameters$feature_data_sheet$sheet_name
  metadata_sheet_name <- parameters$metadata_sheet$sheet_name
  consequence_sheet_names <- setdiff(
    names(w),
    c(site_data_sheet_name,
      feasibility_data_sheet_name,
      feature_data_sheet_name,
      metadata_sheet_name
    )
  )

  # import data
  ## site data
  site_data <- suppressWarnings(openxlsx::read.xlsx(
    xlsxFile = x,
    sheet = site_data_sheet_name,
    colNames = TRUE,
    startRow = 3
  ))
  ## feasibility data
  feasibility_data <- suppressWarnings(openxlsx::read.xlsx(
    xlsxFile = x,
    sheet = feasibility_data_sheet_name,
    colNames = TRUE,
    startRow = 3
  ))
  ## feature data
  feature_data <- suppressWarnings(openxlsx::read.xlsx(
    xlsxFile = x,
    sheet = feature_data_sheet_name,
    colNames = TRUE,
    startRow = 3
  ))
  ## consequence data
  consequence_data <- lapply(
    consequence_sheet_names,
    function(y) {
      suppressWarnings(openxlsx::read.xlsx(
        xlsxFile = x, sheet = y, colNames = TRUE, startRow = 3
      ))
    }
  )
  ## meta sheet
  metadata <- suppressWarnings(openxlsx::read.xlsx(
    xlsxFile = x,
    sheet = metadata_sheet_name,
    colNames = TRUE,
    startRow = 1
  ))

  # convert data to tibble format
  site_data <- tibble::as_tibble(site_data)
  feasibility_data <- tibble::as_tibble(feasibility_data)
  feature_data <- tibble::as_tibble(feature_data)
  metadata <- tibble::as_tibble(metadata)
  consequence_data <- lapply(consequence_data, tibble::as_tibble)

  # extract column names
  site_data_names <- unlist(
    openxlsx::read.xlsx(
      w, sheet = site_data_sheet_name, colNames = FALSE,
      cols = seq_len(ncol(site_data)), rows = 3
    ),
    recursive = TRUE, use.names = FALSE
  )
  feasibility_data_names <- unlist(
    openxlsx::read.xlsx(
      w, sheet = feasibility_data_sheet_name, colNames = FALSE,
      cols = seq_len(ncol(site_data)), rows = 3
    ),
    recursive = TRUE, use.names = FALSE
  )
  feature_data_names <- unlist(
    openxlsx::read.xlsx(
      w, sheet = feature_data_sheet_name, colNames = FALSE,
      cols = seq_len(ncol(site_data)), rows = 3
    ),
    recursive = TRUE, use.names = FALSE
  )
  consequence_data_names <- unlist(
    openxlsx::read.xlsx(
      w, sheet = consequence_sheet_names[[1]],
      colNames = FALSE, cols = seq_len(ncol(site_data)), rows = 3
    ),
    recursive = TRUE, use.names = FALSE
  )

  # fix column names
  names(site_data) <- site_data_names
  names(feasibility_data) <- feasibility_data_names
  names(feature_data) <- feature_data_names
  consequence_data <- lapply(consequence_data, function(x) {
    names(x) <- consequence_data_names
    x
  })

  # ensure that correct data types
  ## site data
  site_data[[1]] <- as.character(site_data[[1]])
  for (i in seq(2, ncol(site_data))) {
    if (!identical(
      names(site_data)[[i]],
      parameters$site_data_sheet$status_header)
    ) {
      site_data[[i]] <- as.numeric(site_data[[i]])
    }
  }

  ## site feasibility data
  feasibility_data[[1]] <- as.character(feasibility_data[[1]])
  for (i in seq(2, ncol(feasibility_data))) {
    feasibility_data[[i]] <- as.logical(feasibility_data[[i]])
  }

  ## feature data
  feature_data[[1]] <- as.character(feature_data[[1]])
  for (i in seq(2, ncol(feature_data))) {
    feature_data[[i]] <- as.numeric(feature_data[[i]])
  }

  ## consequence data
  consequence_data <- lapply(
    consequence_data, function(x) {
      x[[1]] <- as.character(x[[1]])
      for (i in seq(2, ncol(x))) {
        x[[i]] <- as.numeric(x[[i]])
      }
      x
    }
  )

  # set names for list
  names(consequence_data) <- paste0(
    "action_", seq_along(consequence_data)
  )

  # return result
  list(
    site_ids = extract_strings(metadata$site_ids),
    feature_ids = extract_strings(metadata$feature_ids),
    action_ids =  extract_strings(metadata$action_ids),
    site_descriptions = extract_strings(metadata$site_descriptions),
    feature_descriptions = extract_strings(metadata$feature_descriptions),
    action_descriptions = extract_strings(metadata$action_descriptions),
    site_data = site_data,
    feasibility_data = feasibility_data,
    feature_data = feature_data,
    consequence_data = consequence_data
  )
}
