context("simulate_project_data")

test_that("expected_result", {
  # data
  parameters <- read_data_configuration()
  d <- simulate_project_data(
    n_sites = 5, n_features = 3, n_actions = 4,  parameters = parameters
  )
  # tests
  ## object
  expect_is(d, "list")
  ## ids
  expect_is(d$site_ids, "character")
  expect_length(d$site_ids, 5)
  expect_is(d$feature_ids, "character")
  expect_length(d$feature_ids, 3)
  expect_is(d$action_ids, "character")
  expect_length(d$action_ids, 4)
  ## descriptions
  expect_is(d$site_descriptions, "character")
  expect_length(d$site_descriptions, 5)
  expect_is(d$feature_descriptions, "character")
  expect_length(d$feature_descriptions, 3)
  expect_is(d$action_descriptions, "character")
  expect_length(d$action_descriptions, 4)
  ## data
  expect_is(d$site_data, "data.frame")
  expect_equal(nrow(d$site_data), 5)
  expect_is(d$feasibility_data, "data.frame")
  expect_equal(nrow(d$feasibility_data), 5)
  expect_is(d$feature_data, "data.frame")
  expect_equal(nrow(d$feature_data), 3)
  expect_is(d$action_expectation_data, "list")
  expect_length(d$action_expectation_data, 4)
})
