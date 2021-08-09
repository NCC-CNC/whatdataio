context("create_export_workbook")

test_that("expected result", {
  # initialization
  f <- tempfile(fileext = ".xlsx")
  parameters <- app_parameters()
  d <- simulate_data(5, 4, 3, parameters)
  r <-
  # main code
  w <- create_export_workbook(
    site_names = d$site_names,
    feature_names = d$feature_names,
    action_names = d$action_names,
    site_data = d$site_data,
    site_status_data = d$site_status_data,
    feature_data = d$feature_data,
    action_expectation_data = d$action_expectation_data,
    summary_results_data = tibble::tibble(
      Status = "No information to show.",
      Message = "Please click the \"Generate prioritization!\" button."
    ),
    site_results_data = tibble::tibble(
      Status = "No information to show.",
      Message = "Please click the \"Generate prioritization!\" button."
    ),
    feature_results_data = tibble::tibble(
      Status = "No information to show.",
      Message = "Please click the \"Generate prioritization!\" button."
    ),
    parameters = parameters)
  # tests
  expect_is(w, "Workbook")
  openxlsx::saveWorkbook(w, f, returnValue = FALSE)
  expect_true(file.exists(f))
  # clean up
  unlink(f)
})
