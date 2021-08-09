context("create_template_workbook")

test_that("expected result", {
  # initialization
  f <- tempfile(fileext = ".xlsx")
  parameters <- app_parameters()
  # main code
  w <- create_template_workbook(
    site_names = c("s1", "s2", "s3"),
    feature_names = c("spp1", "spp2"),
    action_names = c("a1"),
    parameters = parameters)
  # tests
  expect_is(w, "Workbook")
  openxlsx::saveWorkbook(w, f, returnValue = FALSE)
  expect_true(file.exists(f))
  # clean up
  unlink(f)
})
