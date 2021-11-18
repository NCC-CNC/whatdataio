context("template_data")

test_that("template_site_data", {
  # data
  p <- read_data_configuration()
  sn <- paste0("s", seq_len(5))
  fn <- paste0("f", seq_len(3))
  an <- paste0("a", seq_len(2))
  d <- template_site_data(sn, an, p)
  # tests
  expect_is(d, "data.frame")
  expect_equal(d[[1]], sn)
  expect_true(all(vapply(an, function(x) any(grepl(x, names(d))), logical(1))))
})

test_that("template_feasibility_data", {
  # data
  p <- read_data_configuration()
  sn <- paste0("s", seq_len(5))
  fn <- paste0("f", seq_len(3))
  an <- paste0("a", seq_len(2))
  d <- template_feasibility_data(sn, an, p)
  # tests
  expect_is(d, "data.frame")
  expect_equal(d[[1]], sn)
  expect_true(all(vapply(an, function(x) any(grepl(x, names(d))), logical(1))))
  expect_equal(d[[2]], rep(1, 5))
  expect_equal(d[[3]], rep(1, 5))
})

test_that("template_feature_data", {
  # data
  p <- read_data_configuration()
  sn <- paste0("s", seq_len(5))
  fn <- paste0("f", seq_len(3))
  an <- paste0("a", seq_len(2))
  d <- template_feature_data(fn, p)
  # tests
  expect_is(d, "data.frame")
  expect_equal(d[[1]], fn)
})

test_that("template_action_expectation_data", {
  # data
  p <- read_data_configuration()
  sn <- paste0("s", seq_len(5))
  fn <- paste0("f", seq_len(3))
  an <- paste0("a", seq_len(2))
  d <- template_action_expectation_data(sn, fn, an[1], p)
  # tests
  expect_is(d, "data.frame")
  expect_equal(d[[1]], sn)
  expect_true(all(vapply(fn, function(x) any(grepl(x, names(d))), logical(1))))
})
