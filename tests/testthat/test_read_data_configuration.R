context("read_data_configuration")

test_that("works", {
  expect_is(read_data_configuration(), "list")
})
