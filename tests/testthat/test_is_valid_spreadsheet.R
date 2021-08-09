context("is_valid_spreadsheet")

test_that("valid data", {
  p <- app_parameters()
  expect_true(is_valid_spreadsheet("testdata/data-template.xlsx", p))
})

test_that("invalid data", {
  p <- app_parameters()
  expect_false(is_valid_spreadsheet("nonexistantfile", p))
  expect_false(is_valid_spreadsheet("testdata/invalid-data.xlsx", p))
})
