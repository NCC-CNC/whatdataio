context("template_comments")

test_that("template_site_comments", {
  # data
  p <- read_data_configuration()
  sn <- paste0("s", seq_len(5))
  fn <- paste0("f", seq_len(3))
  an <- paste0("a", seq_len(2))
  d <- template_site_comments(sn, an, p)
  # tests
  expect_is(d, "data.frame")
  expect_equal(d[[1]], sn)
  expect_true(all(an %in% names(d)))
})

test_that("template_site_status_comments", {
  # data
  p <- read_data_configuration()
  sn <- paste0("s", seq_len(5))
  fn <- paste0("f", seq_len(3))
  an <- paste0("a", seq_len(2))
  d <- template_site_status_comments(sn, an, p)
  # tests
  expect_is(d, "data.frame")
  expect_equal(d[[1]], sn)
  expect_true(all(an %in% names(d)))
  expect_equal(d[[2]], rep(NA_integer_, 5))
  expect_equal(d[[3]], rep(NA_integer_, 5))
})

test_that("template_site_feasibility_comments", {
  # data
  p <- read_data_configuration()
  sn <- paste0("s", seq_len(5))
  fn <- paste0("f", seq_len(3))
  an <- paste0("a", seq_len(2))
  d <- template_site_feasibility_comments(sn, an, p)
  # tests
  expect_is(d, "data.frame")
  expect_equal(d[[1]], sn)
  expect_true(all(an %in% names(d)))
  expect_equal(d[[2]], rep(NA_integer_, 5))
  expect_equal(d[[3]], rep(NA_integer_, 5))
})

test_that("template_feature_comments", {
  # data
  p <- read_data_configuration()
  sn <- paste0("s", seq_len(5))
  fn <- paste0("f", seq_len(3))
  an <- paste0("a", seq_len(2))
  d <- template_feature_comments(fn, p)
  # tests
  expect_is(d, "data.frame")
  expect_equal(d[[1]], fn)
  expect_equal(d[[2]], rep(NA_character_, 3))
  expect_equal(d[[3]], rep(NA_character_, 3))
})

test_that("template_action_expectation_comments", {
  # data
  p <- read_data_configuration()
  sn <- paste0("s", seq_len(5))
  fn <- paste0("f", seq_len(3))
  an <- paste0("a", seq_len(2))
  d <- template_action_expectation_comments(sn, fn, an[1], p)
  # tests
  expect_is(d, "data.frame")
  expect_true(all(fn %in% names(d)))
  expect_equal(d[[2]], rep(NA_real_, 5))
  expect_equal(d[[3]], rep(NA_real_, 5))
  expect_equal(d[[4]], rep(NA_real_, 5))
})
