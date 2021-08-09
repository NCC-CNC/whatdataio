context("simulate_data")

test_that("expected_result", {
  # data
  parameters <- read_data_configuration()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4,  parameters = parameters)
  # tests
  expect_is(d, "list")
  expect_is(d$site_names, "character")
  expect_length(d$site_names, 5)
  expect_is(d$feature_names, "character")
  expect_length(d$feature_names, 3)
  expect_is(d$action_names, "character")
  expect_length(d$action_names, 4)
  expect_is(d$site_data, "data.frame")
  expect_equal(nrow(d$site_data), 5)
  expect_is(d$site_status_data, "data.frame")
  expect_equal(nrow(d$site_status_data), 5)
  expect_is(d$feature_data, "data.frame")
  expect_equal(nrow(d$feature_data), 3)
  expect_is(d$action_expectation_data, "list")
  expect_length(d$action_expectation_data, 4)
})
