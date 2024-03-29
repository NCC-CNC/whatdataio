#' @include internal.R
NULL

#' Add feature results worksheet
#'
#' This function adds a feature results worksheet to an Excel Workbook.
#'
#' @inheritParams add_site_results_sheet
#'
#' @details
#' The feature results worksheet is used to summarize the how well
#' features are represented by a prioritization.
#'
#' @inherit add_site_results_sheet return
#'
#' @noRd
add_feature_results_sheet <- function(x, data, comments = NULL, parameters) {
  # validate arguments
  assertthat::assert_that(
    inherits(x, "Workbook"),
    inherits(data, "data.frame"),
    is.list(parameters)
  )
  if (!is.null(comments)) {
    assertthat::assert_that(
      inherits(comments, "data.frame"),
      identical(dim(data), dim(comments))
    )
  }

  # define parameters
  p <- parameters$feature_results_sheet
  start_row <- 3
  n_message_rows <- 2

  # create styles
  header_style <- do.call(openxlsx::createStyle, parameters$header_style)
  label_style <- do.call(openxlsx::createStyle, parameters$label_style)
  data_style <- do.call(openxlsx::createStyle, parameters$data_style)
  main_style <- do.call(openxlsx::createStyle, parameters$main_style)
  sub_style <- do.call(openxlsx::createStyle, parameters$sub_style)

  # create sheet
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
    x = data.frame(x = p$main_message, stringsAsFactors = FALSE),
    startCol = 2, startRow = 1, colNames = FALSE, rowNames = FALSE)

  ## add sub message
  openxlsx::mergeCells(x, p$sheet_name,
    cols = seq_len(n_message_rows) + 1, rows = 2)
  openxlsx::writeData(x, p$sheet_name,
    x = data.frame(x = p$sub_message, stringsAsFactors = FALSE),
    startCol = 2, startRow = 2, colNames = FALSE, rowNames = FALSE)

  # add data
  openxlsx::writeDataTable(x, p$sheet_name, x = data, startRow = 3)

  # add comments
  if (!is.null(comments)) {
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
  }

  # return result
  x
}
