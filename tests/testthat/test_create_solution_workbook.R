context("create_solution_workbook")

test_that("expected result", {
  # initialization
  f <- tempfile(fileext = ".xlsx")
  parameters <- read_data_configuration()
  d <- simulate_project_data(5, 4, 3, parameters)
  # main code
  w <- create_solution_workbook(
    ## variables
    site_ids = d$site_ids,
    feature_ids = d$feature_ids,
    action_ids = d$action_ids,
    site_descriptions = d$site_descriptions,
    feature_descriptions = d$feature_descriptions,
    action_descriptions = d$action_descriptions,
    ## data
    site_data = d$site_data,
    feasibility_data = d$feasibility_data,
    feature_data = d$feature_data,
    action_expectation_data = d$action_expectation_data,
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
