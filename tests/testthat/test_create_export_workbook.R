context("create_export_workbook")

test_that("expected result", {
  # initialization
  f <- tempfile(fileext = ".xlsx")
  parameters <- read_data_configuration()
  d <- simulate_data(5, 4, 3, parameters)
  # main code
  w <- create_export_workbook(
    ## variables
    site_ids = d$site_ids,
    feature_ids = d$feature_ids,
    action_ids = d$action_ids,
    site_descriptions = d$site_descriptions,
    feature_descriptions = d$feature_descriptions,
    action_descriptions = d$action_descriptions,
    ## data
    site_data = d$site_data,
    site_status_data = d$site_status_data,
    site_feasibility_data = d$site_feasibility_data,
    feature_data = d$feature_data,
    action_expectation_data = d$action_expectation_data,
    ## data comments
    site_comments = d$site_comments,
    site_status_comments = d$site_status_comments,
    site_feasibility_comments = d$site_feasibility_comments,
    feature_comments = d$feature_comments,
    action_expectation_comments = d$action_expectation_comments,
    ## results
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
    ## results comments
    summary_results_comments = tibble::tibble(
      Status = "No further details available",
      Message = "No further information to show"
    ),
    site_results_comments = tibble::tibble(
      Status = "No further details available",
      Message = "No further information to show"
    ),
    feature_results_comments = tibble::tibble(
      Status = "No further details available",
      Message = "No further information to show"
    ),
    ## parameters
    parameters = parameters
  )
  # tests
  expect_is(w, "Workbook")
  openxlsx::saveWorkbook(w, f, returnValue = FALSE)
  expect_true(file.exists(f))
  # clean up
  unlink(f)
})
