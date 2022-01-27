context("read_spreadsheet_data")

test_that("expected result", {
  # set parameters
  f <- "testdata/data-template.xlsx"
  p <- read_data_configuration()
  # read data
  x <- read_spreadsheet_data(f, p)
  # tests
  expect_is(x, "list")
  expect_equal(x$site_ids, c("q", "w", "e"))
  expect_equal(x$feature_ids, c("a", "s", "d", "f"))
  expect_equal(x$action_ids, c("z", "x", "c", "v", "b"))
  expect_equal(x$site_descriptions, paste0("Site ", x$site_ids))
  expect_equal(x$feature_descriptions, paste0("Feature ", x$feature_ids))
  expect_equal(x$action_descriptions,  paste0("Action ", x$action_ids))
  expect_is(x$site_data, "tbl_df")
  expect_is(x$feasibility_data, "tbl_df")
  expect_is(x$feature_data, "tbl_df")
  expect_is(x$consequence_data, "list")
  expect_equal(length(x$consequence_data), 5)
  expect_equal(names(x$consequence_data),
               paste0("action_", seq_along(x$consequence_data)))
  expect_true(all(vapply(x$consequence_data,
                         inherits, logical(1), "tbl_df")))
})
