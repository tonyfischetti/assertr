context("assertions about predicates in predicates.R")

set.seed(1)
test.vect <- rnorm(100, mean=100, sd=20)
test.vect2 <- c(1, NA, 3)
test.vect3 <- c(rnorm(100, mean=100, 20), 10000)



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

test_that("not_na handles vectors correctly", {
  expect_equal(not_na(c("tree", "arbol")), c(TRUE, TRUE))
  # NaN in character string will convert to non-na string
  expect_equal(not_na(c("tree", "árbol", NA, "δέντρο")),
               c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(not_na(c("tree", "árbol", NA, NaN, "δέντρο")),
               c(TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_equal(not_na(c("tree", "árbol", NA, NaN, "δέντρο")),
               c(TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_equal(not_na(c("tree", "árbol", NA, NaN, "δέντρο"), allow.NaN=FALSE),
               c(TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_equal(not_na(c("tree", "árbol", NA, NaN, "δέντρο"), allow.NaN=TRUE),
               c(TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_equal(not_na(c(1, (1+1), 0/0, (6/2), NA)),
               c(TRUE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(not_na(c(1, (1+1), 0/0, (6/2), NA), allow.NaN=TRUE),
               c(TRUE, TRUE, TRUE, TRUE, FALSE))

})

test_that("not_na errors out when appropriate", {
  expect_error(not_na(c()),
               "not_na must be called on non-null object")
  expect_error(not_na(),
               ".x. is missing")
})

test_that("predicate is tagged for assert function to vectorize", {
  expect_equal(comment(not_na), "assertr/vectorized")
})
######################################


########### within_bounds ###########
test_that("within_bounds fails appropriately", {
  expect_error(within_bounds(),
               ".lower.bound. is missing")
  expect_error(within_bounds(1, "tree"), "bounds must be numeric")
  expect_error(within_bounds(2, 1),
               "lower bound must be strictly lower than upper bound")
  expect_error(within_bounds(2, 2),
               "lower bound must be strictly lower than upper bound")
})

test_that("returned predicate works appropriately on scalars", {
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

test_that("returned predicate works appropriately on vectors", {
  expect_equal(within_bounds(1,3)(c(0,1,2,3,4)),
               c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(within_bounds(1,3, include.lower = FALSE)(c(0,1,2,3,4)),
               c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(within_bounds(1,3, include.lower = FALSE,
                             include.upper = FALSE)(c(0,1,2,3,4)),
               c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(within_bounds(1,3)(c(0,1,2,3,4, NA, 5, NaN)),
               c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(within_bounds(1,3, allow.na=FALSE)(c(0,1,2,3,4, NA, 5, NaN)),
               c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
})

test_that("returned predicate fails appropriately", {
  expect_error(within_bounds(0,1)(),
               ".x. is missing")
  expect_error(within_bounds(0,1)("tree"),
               "bounds must only be checked on numerics")
  expect_error(within_bounds(0,1)(c("tree", 1, 2)),
               "bounds must only be checked on numerics")
  expect_error(within_bounds(0,1)(c()),
               "bounds must be checked on non-null element")
})

test_that("returned predicate is tagged for assert function to vectorize", {
  expect_equal(comment(within_bounds(1,2)), "assertr/vectorized")
})
#####################################


############### within_n_sds ###############

test_that("returned predicate works appropriately", {
  expect_equal(within_n_sds(1)(test.vect)(84.3), TRUE)
  expect_equal(within_n_sds(1)(test.vect)(84.2), FALSE)
  expect_equal(within_n_sds(1)(test.vect)(c(84.3, 84.2)), c(TRUE, FALSE))
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
               ".n. is missing")
})

test_that("second inner function fails appropriately", {
  expect_error(within_n_sds(1)(),
               "argument .a.vector. is missing")
  expect_error(within_n_sds(1)(1),
               "standard deviations of vector is NA")
  expect_error(within_n_sds(1)(c("johnny", "marr")),
               "argument must be a numeric vector")
})
############################################


############### within_n_mads ##############

test_that("returned predicate works appropriately", {
  expect_equal(within_n_mads(1)(test.vect3)(test.vect3[100]), TRUE)
  expect_equal(within_n_mads(1)(test.vect3)(test.vect3[101]), FALSE)
  expect_equal(within_n_mads(1)(test.vect3)(test.vect3[100:101]),
               c(TRUE, FALSE))
  expect_equal(within_n_mads(1)(test.vect)(84.9), TRUE)
  expect_equal(within_n_mads(1)(test.vect)(84.8), FALSE)
  expect_equal(within_n_mads(2)(test.vect)(137), TRUE)
  expect_equal(within_n_mads(2)(test.vect)(137.1), FALSE)
  expect_equal(within_n_mads(2)(test.vect)(test.vect2[2]), TRUE)
  expect_equal(within_n_mads(2, allow.na=FALSE)(test.vect)(test.vect2[2]), FALSE)
})

# the returned predicate (third inner function) will fail appropriately
# given that the above "within_bounds" checks work out

test_that("first inner function fails appropriately", {
  expect_error(within_n_mads(-1),
               "'n' must be a positive number")
  expect_error(within_n_mads(0),
               "'n' must be a positive number")
  expect_error(within_n_mads(NA),
               "'n' must be a positive number")
  expect_error(within_n_mads(c(1,2)),
               "'n' must be a positive number")
  expect_error(within_n_mads(),
               ".n. is missing")
})

test_that("second inner function fails appropriately", {
  expect_error(within_n_mads(1)(),
               "argument .a.vector. is missing")
  expect_error(within_n_mads(1)(1),
               "lower bound must be strictly lower than upper bound")
  expect_error(within_n_mads(1)(c("johnny", "marr")),
               "argument must be a numeric vector")
})
############################################



############### in_set ###############
test_that("in_set fails appropriately", {
  expect_error(in_set(),
               "can not test for membership in empty set")
  expect_error(in_set(,allow.na=FALSE),
               "argument is missing")
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
               ".x. is missing")
  expect_error(in_set(0,1)(c(1,2)),
               "bounds must be checked on a single element")
  expect_error(in_set(0,1)(c()),
               "bounds must be checked on a single element")
})
######################################
