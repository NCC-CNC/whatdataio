context("create_template_workbook")

test_that("expected result", {
  # initialization
  f <- tempfile(fileext = ".xlsx")
  parameters <- read_data_configuration()
  # main code
  w <- create_template_workbook(
    site_ids = c("s1", "s2", "s3"),
    site_descriptions = c("Site s1", "Site s2", "Site s3"),
    feature_ids = c("spp1", "spp2"),
    feature_descriptions = c("Species spp1", "Species spp2"),
    action_ids = c("a1"),
    action_descriptions = c("Action a1"),
    parameters = parameters)
  # tests
  expect_is(w, "Workbook")
  openxlsx::saveWorkbook(w, f, returnValue = FALSE)
  expect_true(file.exists(f))
  # clean up
  unlink(f)
})
