context("read_spreadsheet_data")

test_that("expected result", {
  # set parameters
  f <- "testdata/data-template.xlsx"
  p <- app_parameters()
  # read data
  x <- read_spreadsheet_data(f, p)
  # tests
  expect_is(x, "list")
  expect_equal(x$site_names, c("q", "w", "e", "r", "t"))
  expect_equal(x$feature_names, c("a", "s", "d", "f", "g"))
  expect_equal(x$action_names, c("z", "x", "c", "v", "b"))
  expect_is(x$site_data, "tbl_df")
  expect_is(x$site_status_data, "tbl_df")
  expect_is(x$feature_data, "tbl_df")
  expect_is(x$action_expectation_data, "list")
  expect_equal(length(x$action_expectation_data), 5)
  expect_equal(names(x$action_expectation_data),
               paste0("action_", seq_along(x$action_expectation_data)))
  expect_true(all(vapply(x$action_expectation_data,
                         inherits, logical(1), "tbl_df")))
})
