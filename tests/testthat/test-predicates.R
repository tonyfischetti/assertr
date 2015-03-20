context("assertions about predicates in predicates.R")

set.seed(1)
test.vect <- rnorm(100, mean=100, sd=20)
test.vect2 <- c(1, NA, 3)



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
  expect_error(not_na(c(1,2)),
               "not_na must be called with single element")
  expect_error(not_na(diag(c(1,2))),
               "not_na must be called with single element")
  expect_error(not_na(c()),
               "not_na must be called with single element")
  expect_error(not_na(),
               "argument \"x\" is missing, with no default")
})
######################################


########### within_bounds ###########
test_that("within_bounds fails appropriately", {
  expect_error(within_bounds(),
               "argument \"lower.bound\" is missing, with no default")
  expect_error(within_bounds(1, "tree"), "bounds must be numeric")
  expect_error(within_bounds(2, 1),
               "lower bound must be strictly lower than upper bound")
  expect_error(within_bounds(2, 2),
               "lower bound must be strictly lower than upper bound")
})

test_that("returned predicate works appropriately", {
  expect_equal(within_bounds(3, 4)(pi), TRUE)
  expect_equal(within_bounds(3, 4)(3), TRUE)
  expect_equal(within_bounds(3, 4, include.lower=FALSE)(3), FALSE)
  expect_equal(within_bounds(3, 4)(4), TRUE)
  expect_equal(within_bounds(3, 4, include.upper=FALSE)(4), FALSE)
  expect_equal(within_bounds(3, 4)(10), FALSE)
  expect_equal(within_bounds(3, 4)(as.numeric(NA)), TRUE)
  expect_equal(within_bounds(3, 4, allow.na=FALSE)(as.numeric(NA)), FALSE)
  expect_equal(within_bounds(0, Inf)(0), TRUE)
  expect_equal(within_bounds(0, Inf, include.lower=FALSE)(0), FALSE)
  expect_equal(within_bounds(0, Inf)(10), TRUE)
  expect_equal(within_bounds(0, Inf)(Inf), TRUE)
  expect_equal(within_bounds(0, Inf, include.upper=FALSE)(Inf), FALSE)
})

test_that("returned predicate fails appropriately", {
  expect_error(within_bounds(0,1)(),
               "argument \"x\" is missing, with no default")
  expect_error(within_bounds(0,1)("tree"),
               "bounds must only be checked on numerics")
  expect_error(within_bounds(0,1)(c(1,2)),
               "bounds must be checked on a single element")
  expect_error(within_bounds(0,1)(c()),
               "bounds must be checked on a single element")
})
#####################################


############### within_n_sds ###############

test_that("returned predicate works appropriately", {
  expect_equal(within_n_sds(1)(test.vect)(84.3), TRUE)
  expect_equal(within_n_sds(1)(test.vect)(84.2), FALSE)
  expect_equal(within_n_sds(2)(test.vect)(138.1), TRUE)
  expect_equal(within_n_sds(2)(test.vect)(138.11), FALSE)
  expect_equal(within_n_sds(2)(test.vect)(test.vect2[2]), TRUE)
  expect_equal(within_n_sds(2, allow.na=FALSE)(test.vect)(test.vect2[2]), FALSE)
})

# the returned predicate (third inner function) will fail appropriately
# given that the above "within_bounds" checks work out

test_that("first inner function fails appropriately", {
  expect_error(within_n_sds(-1),
               "'n' must be a positive number")
  expect_error(within_n_sds(0),
               "'n' must be a positive number")
  expect_error(within_n_sds(NA),
               "'n' must be a positive number")
  expect_error(within_n_sds(c(1,2)),
               "'n' must be a positive number")
  expect_error(within_n_sds(),
               "argument \"n\" is missing, with no default")
})

test_that("second inner function fails appropriately", {
  expect_error(within_n_sds(1)(),
               "argument \"a.vector\" is missing, with no default")
  expect_error(within_n_sds(1)(1),
               "standard deviations of vector is NA")
  expect_error(within_n_sds(1)(c("johnny", "marr")),
               "argument must be a numeric vector")
})
############################################



############### in_set ###############
test_that("in_set fails appropriately", {
  expect_error(in_set(),
               "can not test for membership in empty set")
  expect_error(in_set(,allow.na=FALSE),
               "argument is missing, with no default")
})

test_that("returned predicate works appropriately", {
  expect_equal(in_set(3, 4)(pi), FALSE)
  expect_equal(in_set(3, 4)(4), TRUE)
  expect_equal(in_set(1:10)(2), TRUE)
  expect_equal(in_set(1:10)(11), FALSE)
  expect_equal(in_set(1:10)(NA), TRUE)
  expect_equal(in_set(1:10, allow.na = TRUE)(NA), TRUE)
  expect_equal(in_set(1:10, allow.na = FALSE)(NA), FALSE)
  expect_equal(in_set(1, "tree")("tree"), TRUE)
  expect_equal(in_set(1, "tree")("leaf"), FALSE)
})

test_that("returned predicate fails appropriately", {
  expect_error(in_set(0,1)(),
               "argument \"x\" is missing, with no default")
  expect_error(in_set(0,1)(c(1,2)),
               "bounds must be checked on a single element")
  expect_error(in_set(0,1)(c()),
               "bounds must be checked on a single element")
})
######################################
