#' @include internal.R
NULL

#' Add consequence worksheet
#'
#' This function adds a consequence data worksheet to an Excel Workbook.
#'
#' @inheritParams create_template_workbook
#' @inheritParams add_site_data_sheet
#'
#' @details
#' A consequence worksheet specifies the expected amount of each
#' feature that would be expected to follow if a certain management action
#' were implemented within each site. Note that is it assumed that the
#' amount of each feature expected to occur within each site is independent.
#' This function only adds a single sheet for a particular action,
#' and so it will need to be executed multiple times for each action in
#' a particular conservation planning exercise.
#'
#' @inherit add_site_data_sheet return
#'
#' @noRd
add_consequence_sheet <- function(x, data, comments, action_id,
                                         parameters) {
  # validate arguments
  assertthat::assert_that(
    inherits(x, "Workbook"),
    inherits(data, "data.frame"),
    inherits(comments, "data.frame"),
    identical(ncol(data), ncol(comments)),
    identical(nrow(data), nrow(comments)),
    assertthat::is.string(action_id), assertthat::noNA(action_id),
    is.list(parameters))

  # define parameters
  p <- parameters$consequence_sheet
  start_row <- 3
  n_message_rows <- 5

  # create styles
  header_style <- do.call(openxlsx::createStyle, parameters$header_style)
  label_style <- do.call(openxlsx::createStyle, parameters$label_style)
  data_style <- do.call(openxlsx::createStyle, parameters$data_style)
  main_style <- do.call(openxlsx::createStyle, parameters$main_style)
  sub_style <- do.call(openxlsx::createStyle, parameters$sub_style)

  # create sheet
  p$sheet_name <- as.character(glue::glue(
    p$sheet_name, action_ids = action_id))
  openxlsx::addWorksheet(x, sheetName = p$sheet_name)

  # set up worksheet
  ## lock sheet
  openxlsx::protectWorksheet(x, p$sheet_name, protect = TRUE,
    lockFormattingCells = FALSE, lockFormattingColumns = FALSE,
    lockInsertingColumns = TRUE, lockDeletingColumns = TRUE,
    lockInsertingRows = TRUE, lockDeletingRows = TRUE)

  ## resize column widths
  openxlsx::setColWidths(x,
    sheet = p$sheet_name,
    cols = seq_len(ncol(data)),
    widths = column_widths(data) + p$col_width_padding)
  if (ncol(data) < 5) {
    openxlsx::setColWidths(x, sheet = p$sheet_name,
      cols = seq(ncol(data) + 1, 5),
      widths = rep(max(column_widths(data) + p$col_width_padding),
                   5 - ncol(data)))
  }

  ## resize row heights
  openxlsx::setRowHeights(x, sheet = p$sheet_name,
    rows = 1, heights = p$main_message_height)
  openxlsx::setRowHeights(x, sheet = p$sheet_name,
    rows = 2, heights = p$sub_message_height)

  ## freeze first row and column
  openxlsx::freezePane(x, p$sheet_name,
    firstActiveRow = start_row, firstCol = TRUE)

  # add specific styles for certain cells
  ## add main message
  openxlsx::addStyle(x, p$sheet_name,
    style = main_style,
    rows = 1, cols = seq_len(ncol(data)),
    gridExpand = TRUE)

  ## add sub message
  openxlsx::addStyle(x, p$sheet_name,
    style = sub_style,
    rows = 2, cols = seq_len(ncol(data)),
    gridExpand = TRUE)

  ## style header cells
  openxlsx::addStyle(x, p$sheet_name,
    style = header_style,
    rows = start_row, cols = seq_len(ncol(data)),
    gridExpand = TRUE)

  ## style label cells
  openxlsx::addStyle(x, p$sheet_name,
    style = label_style,
    rows = seq_len(nrow(data)) + start_row, cols = 1,
    gridExpand = TRUE)

  ## style data cells
  openxlsx::addStyle(x, p$sheet_name,
    style = data_style,
    rows = seq_len(nrow(data)) + 3, cols = seq(2, ncol(data)),
    gridExpand = TRUE)

  # add messages
  ## main message
  openxlsx::mergeCells(x, p$sheet_name,
    cols = seq_len(n_message_rows) + 1, rows = 1)
  openxlsx::writeData(x, p$sheet_name,
    x = data.frame(x = as.character(glue::glue(
      p$main_message, action_ids = action_id)), stringsAsFactors = FALSE),
    startCol = 2, startRow = 1, colNames = FALSE, rowNames = FALSE)

  ## add sub message
  openxlsx::mergeCells(x, p$sheet_name,
    cols = seq_len(n_message_rows) + 1, rows = 2)
  openxlsx::writeData(x, p$sheet_name,
    x = data.frame(x = as.character(glue::glue(
      p$sub_message, action_ids = action_id)), stringsAsFactors = FALSE),
    startCol = 2, startRow = 2, colNames = FALSE, rowNames = FALSE)

  # add data
  openxlsx::writeDataTable(x, p$sheet_name, x = data, startRow = 3)

  # add input data validation
  openxlsx::dataValidation(x, p$sheet_name,
    rows = seq_len(nrow(data)) + start_row, cols = seq(2, ncol(data)),
    type = "decimal", operator = "between",
    value = c(0, 1e+4), allowBlank = FALSE,
    showInputMsg = TRUE, showErrorMsg = TRUE)

  # add comments
  ## add comments for header
  for (i in seq_len(ncol(comments))) {
    if (!identical(names(data)[i], names(comments)[i])) {
      openxlsx::writeComment(
        x,
        sheet = p$sheet_name,
        col = i,
        row = start_row,
        comment = openxlsx::createComment(
          comment = names(comments)[i],
          author = "What Template Maker",
          visible = FALSE
        )
      )
    }
  }
  ## add comments to cells
  for (i in seq_len(ncol(comments))) {
    for (j in seq_len(nrow(comments))) {
      if (!is.na(comments[[i]][[j]])) {
        openxlsx::writeComment(
          x,
          sheet = p$sheet_name,
          col = i,
          row = start_row + j,
          comment = openxlsx::createComment(
            comment = comments[[i]][[j]],
            author = "What Template Maker",
            visible = FALSE
          )
        )
      }
    }
  }

  # return result
  x

}
