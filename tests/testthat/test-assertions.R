context("assertions about assertions in assertion.R")

# just some set up
a <- 1
alist <- list(a=c(1,2,3), b=c(4,5,6))


############### verify ###############
test_that("verify returns data if verification passes", {
  expect_equal(verify(mtcars, drat > 2), mtcars)
  expect_equal(verify(mtcars, mtcars$drat > 2), mtcars)
  expect_equal(verify(mtcars, mtcars$drat > 2), mtcars)
  expect_equal(verify(mtcars, nrow(mtcars) > 30), mtcars)
  expect_equal(verify(mtcars, am %in% c(0,1,2)), mtcars)
  expect_equal(verify(mtcars, am %in% c(0,1)), mtcars)
  # looks to parent frame scope?
  expect_equal(verify(mtcars, a > 0), mtcars)
  expect_equal(verify(mtcars, nrow(iris) > 140), mtcars)
  expect_equal(verify(alist, length(a) > 0), alist)
  # respects scoping rules?
  expect_equal(verify(alist, length(a) > 2), alist)
  expect_equal(verify(alist, length(a) > 2 && length(b) > 2), alist)
  expect_equal(verify(alist, a >= 1 & b > 2), alist)
  expect_equal(verify(alist, a > 2 | b > 2), alist)
  expect_equal(verify(alist, 3 > 2), alist)
})

test_that("verify raises error if verification fails", {
  expect_error(verify(mtcars, drat > 3), "verification failed!")
  expect_error(verify(mtcars, nrow(mtcars) > 34), "verification failed!")
  expect_error(verify(mtcars, am %in% c(1,2)), "verification failed!")
  # looks to parent frame scope?
  expect_error(verify(mtcars, a < 0), "verification failed!")
  # respects scoping rules?
  expect_error(verify(alist, length(a) == 1), "verification failed!")
  expect_error(verify(alist, length(a) > 4), "verification failed!")
  expect_error(verify(alist, length(a) > 2 && length(b) > 3),
               "verification failed!")
  expect_error(verify(alist, a >= 2 | b > 4), "verification failed!")
  expect_error(verify(alist, 2 > 4), "verification failed!")
})

test_that("verify breaks appropriately", {
  expect_error(verify(4 > 2), "argument \"expr\" is missing, with no default")
  expect_error(verify(mtcars), "argument \"expr\" is missing, with no default")
  expect_error(verify(MTCARS, 2 > 1),
               "object 'MTCARS' not found")
  expect_warning(verify(mtcars, 1),
                 "coercing argument of type 'double' to logical")
  expect_error(suppressWarnings(verify(mtcars, "1")),
               "missing value where TRUE/FALSE needed")
  # why does it not think the errors are the same?
  # expect_error(verify(mtcars, 2 > 1, "tree"), "unused argument (\"tree\")")
  expect_error(verify(mtcars, d > 1), "object 'd' not found")
})
######################################


############### assert ###############
test_that("assert returns data if verification passes", {
  expect_equal(assert(mtcars, in_set(0,1), vs, am), mtcars)
  expect_equal(assert(mtcars, within_bounds(3,5), gear), mtcars)
  expect_equal(assert(mtcars, is.numeric, mpg:carb), mtcars)
  expect_equal(assert(mtcars, not_na, vs), mtcars)
  expect_equal(assert(mtcars, not_na, mpg:carb), mtcars)
  # lambdas
  expect_equal(assert(mtcars, function(x) x%%1==0, cyl, vs, am, gear, carb), mtcars)
  expect_equal(assert(mtcars, function(x) x%%1==0, gear), mtcars)
  expect_equal(assert(iris, function(x) nchar(as.character(x)) > 5, Species),
               iris)
})

# test_that("assert raises error if verification fails", {
#   # why does it not think the errors are the same?
#   expect_error(assert(mtcars, within_bounds(3.5,4.5), gear),
#                "Error: Assertion 'within_bounds' violated at index 4 of vector 'gear' (value: 3)")
#   expect_equal(assert(mtcars, within_bounds(3,5), gear, carb),
#                "Assertion 'within_bounds' violated at index 3 of vector 'carb' (value: 1)")
# })


test_that("assert breaks appropriately", {
  expect_error(assert(in_set(0,1), mtcars$vs),
               "no applicable method for 'select_' applied to an object of class \"function\"")
  expect_error(assert("tree"),
               "no applicable method for 'select_' applied to an object of class \"character\"")
})
######################################
