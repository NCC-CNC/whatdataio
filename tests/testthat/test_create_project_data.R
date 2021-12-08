context("create_project_data")

test_that("expected result", {
  # initialization
  f <- tempfile(fileext = ".xlsx")
  parameters <- read_data_configuration()
  d <- simulate_project_data(5, 4, 3, parameters)
  # main code
  w <- create_project_workbook(
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
