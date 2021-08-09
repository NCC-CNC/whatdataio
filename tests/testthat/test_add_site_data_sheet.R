context("add_site_data_sheet")

test_that("expected result", {
  # initialization
  f <- tempfile(fileext = ".xlsx")
  w <- openxlsx::createWorkbook()
  parameters <- read_data_configuration()
  sn <- paste0("s", seq_len(5))
  fn <- paste0("f", seq_len(3))
  an <- paste0("a", seq_len(2))
  dat <- template_site_data(sn, an, parameters)
  # main code
  w <- add_site_data_sheet(w, dat, parameters = parameters)
  # tests
  expect_is(w, "Workbook")
  suppressWarnings(openxlsx::saveWorkbook(w, f, returnValue = FALSE))
  expect_true(file.exists(f))
  # clean up
  unlink(f)
})
