context("add_action_expectation_sheet")

test_that("expected result", {
  # initialization
  f <- tempfile(fileext = ".xlsx")
  w <- openxlsx::createWorkbook()
  parameters <- app_parameters()
  sn <- paste0("s", seq_len(5))
  fn <- paste0("f", seq_len(3))
  an <- paste0("a", seq_len(2))
  dat <- template_action_expectation_data(sn, fn, an[1], parameters)
  # main code
  w <- add_action_expectation_sheet(w, dat, "a1", parameters = parameters)
  # tests
  expect_is(w, "Workbook")
  suppressWarnings(openxlsx::saveWorkbook(w, f, returnValue = FALSE))
  expect_true(file.exists(f))
  # clean up
  unlink(f)
})
