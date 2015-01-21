context("assertions about assertions in assertion.R")


############### not_na ###############
test_that("not_na returns true if not NA", {
  expect_equal(not_na("tree"), TRUE)
  expect_equal(not_na(2.8),    TRUE)
  expect_equal(not_na(8),      TRUE)
})

test_that("not_na returns FALSE if NA", {
  expect_equal(not_na(NA), FALSE)
})

test_that("not_na handles NaNs right", {
  expect_equal(not_na(NaN), FALSE)
  expect_equal(not_na(NaN, allow.NaN=FALSE), FALSE)
  expect_equal(not_na(NaN, allow.NaN=TRUE), TRUE)
})

test_that("not_na errors out when appropriate", {
  expect_error(not_na(c(1,2)),       "not_na must be called with single element")
  expect_error(not_na(diag(c(1,2))), "not_na must be called with single element")
  expect_error(not_na(c()),          "not_na must be called with single element")
  expect_error(not_na(),             "argument \"x\" is missing, with no default")
})
######################################


#####################################
########### within_bounds ###########

test_that("within_bounds fails appropriately", {
  expect_error(within_bounds(), "argument \"lower.bound\" is missing, with no default")
  expect_error(within_bounds(1, "tree"), "bounds must be numeric")
  expect_error(within_bounds(2, 1), "lower bound must be strictly lower than upper bound")
  expect_error(within_bounds(2, 2), "lower bound must be strictly lower than upper bound")
})

test_that("returned predicate works appropriately", {
  expect_equal(within_bounds(3, 4)(pi), TRUE)
  expect_equal(within_bounds(3, 4)(3), TRUE)
  expect_equal(within_bounds(3, 4, include.lower=FALSE)(3), FALSE)
  expect_equal(within_bounds(3, 4)(4), TRUE)
  expect_equal(within_bounds(3, 4, include.upper=FALSE)(4), FALSE)
  expect_equal(within_bounds(3, 4)(10), FALSE)
  expect_equal(within_bounds(3, 4)(as.numeric(NA)), TRUE)
  expect_equal(within_bounds(3, 4, allow.NA=FALSE)(as.numeric(NA)), FALSE)
  expect_equal(within_bounds(0, Inf)(0), TRUE)
  expect_equal(within_bounds(0, Inf, include.lower=FALSE)(0), FALSE)
  expect_equal(within_bounds(0, Inf)(10), TRUE)
  expect_equal(within_bounds(0, Inf)(Inf), TRUE)
  expect_equal(within_bounds(0, Inf, include.upper=FALSE)(Inf), FALSE)
})

test_that("returned predicate fails appropriately", {
  expect_error(within_bounds(0,1)(), "argument \"x\" is missing, with no default")
  expect_error(within_bounds(0,1)("tree"), "bounds must only be checked on numerics")
  expect_error(within_bounds(0,1)(c(1,2)), "bounds must be checked on a single element")
  expect_error(within_bounds(0,1)(c()), "bounds must be checked on a single element")

})


# if(length(x)>1)      stop("bounds must be checked on a single element")
# if(is.null(x))       stop("bounds must be checked on a single element")
# if(!is.numeric(x))   stop("bounds must only be checked on numerics")


#####################################
