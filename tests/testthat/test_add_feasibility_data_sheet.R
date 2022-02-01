context("add_feasibility_data_sheet")

test_that("expected result", {
  # initialization
  f <- tempfile(fileext = ".xlsx")
  w <- openxlsx::createWorkbook()
  parameters <- read_data_configuration()
  sn <- paste0("s", seq_len(5))
  sd <- paste0("Locality s", seq_len(5))
  fn <- paste0("f", seq_len(3))
  fd <- paste0("Habitat for s", seq_len(3))
  an <- paste0("a", seq_len(2))
  ad <- paste0("Action a", seq_len(2))
  dat <- template_feasibility_data(sn, an, parameters)
  com <- template_feasibility_comments(sd, ad, parameters)
  # main code
  w <- add_feasibility_data_sheet(w, dat, com, parameters = parameters)
  # tests
  expect_is(w, "Workbook")
  suppressWarnings(openxlsx::saveWorkbook(w, f, returnValue = FALSE))
  expect_true(file.exists(f))
  # clean up
  unlink(f)
})
