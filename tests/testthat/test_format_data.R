context("format_data")

test_that("format_pu_data", {
  # simulate data
  parameters <- read_data_configuration()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4,  parameters = parameters)

  # format data
  f <- format_pu_data(
    d$site_ids, d$feature_ids, d$action_ids,
    d$site_data, d$action_expectation_data, parameters)

  # run tests
  expect_true(inherits(f, "data.frame"))
})

test_that("format_zone_data", {
  # simulate data
  parameters <- read_data_configuration()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4,  parameters = parameters)

  # format data
  f <- format_zone_data(
    d$site_ids, d$feature_ids, d$action_ids, parameters)

  # run tests
  expect_true(inherits(f, "ZonesCharacter"))
})

test_that("format_target_data", {
  # simulate data
  parameters <- read_data_configuration()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4,  parameters = parameters)

  # format data
  f <- format_target_data(
    d$site_ids, d$feature_ids, d$action_ids, d$feature_data, parameters)

  # run tests
  expect_true(inherits(f, "data.frame"))
})

test_that("format_weights_data", {
  # simulate data
  parameters <- read_data_configuration()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4,  parameters = parameters)

  # format data
  f <- format_weights_data(
    d$site_ids, d$feature_ids, d$action_ids, d$feature_data, parameters)

  # run tests
  expect_true(inherits(f, "data.frame"))
})

test_that("format_locked_data", {
  # simulate data
  parameters <- read_data_configuration()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4,  parameters = parameters)

  # format data
  f <- format_locked_data(
      d$site_ids, d$feature_ids, d$action_ids, d$site_status_data,
      parameters)

  # run tests
  expect_true(inherits(f, "data.frame"))
})

test_that("format_results_data", {
  # simulate data
  parameters <- read_data_configuration()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4,  parameters = parameters)

  # format data for prioritization
  pu_data <- format_pu_data(
    d$site_ids, d$feature_ids, d$action_ids,
    d$site_data, d$action_expectation_data, parameters)
  zone_data <- format_zone_data(
    d$site_ids, d$feature_ids, d$action_ids, parameters)
  target_data <- format_target_data(
    d$site_ids, d$feature_ids, d$action_ids, d$feature_data, parameters)

  # create solution data
  solution <- pu_data
  status <- sample(d$action_ids, length(d$site_ids), replace = TRUE)
  for (i in seq_along(d$action_ids)) {
    n <- paste0("solution_1_", d$action_ids[i])
    solution[[n]] <- as.numeric(status == d$action_ids[i])
  }

  # format results data
  out <- format_results_data(
    d$site_ids, d$feature_ids, d$action_ids,
    pu_data, zone_data, target_data, solution, parameters)

  # tests
  expect_is(out, "list")
  expect_is(out$summary_results, "data.frame")
  expect_is(out$feature_results, "data.frame")
  expect_is(out$site_results, "data.frame")
})
