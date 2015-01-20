context("assertions about assertions in assertion.R")

test_that("not_na returns true if not NA", {
  expect_equal(not_na("tree"), TRUE)
  expect_equal(not_na(2.8),    TRUE)
  expect_equal(not_na(8),      TRUE)
})

test_that("not_na returns FALSE if NA", {
  expect_equal(not_na(NA), FALSE)
})

test_that("not_na errors out when appropriate", {
  expect_error(not_na(c(1,2)),       "not_na must be called with single element")
  expect_error(not_na(diag(c(1,2))), "not_na must be called with single element")
  expect_error(not_na(c()),          "not_na must be called with single element")
  expect_error(not_na(),             "argument \"x\" is missing, with no default")
})
