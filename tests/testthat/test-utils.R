
context("Utilities")

test_that("make_spaces is OK", {

  expect_equal(make_spaces(0), "")
  expect_equal(make_spaces(1), " ")
  expect_equal(make_spaces(2), "  ")

})
