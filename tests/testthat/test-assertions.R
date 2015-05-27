context("assertions about assertions in assertion.R")

# just some set up
a <- 1
alist <- list(a=c(1,2,3), b=c(4,5,6))

our.iris <- iris
our.iris.2 <- our.iris
our.iris.2[106,1] <- 7
our.iris.3 <- our.iris.2
our.iris.3[c(118, 119, 123, 132, 131, 136), 1] <- 7


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
  expect_error(verify(mtcars, drat > 3), "verification failed! \\(4 failures)")
  expect_error(verify(mtcars, nrow(mtcars) > 34), "verification failed! \\(1 failure)")
  expect_error(verify(mtcars, am %in% c(1,2)), "verification failed! \\(19 failures)")
  # looks to parent frame scope?
  expect_error(verify(mtcars, a < 0), "verification failed! \\(1 failure)")
  # respects scoping rules?
  expect_error(verify(alist, length(a) == 1), "verification failed! \\(1 failure)")
  expect_error(verify(alist, length(a) > 4), "verification failed! \\(1 failure)")
  expect_error(verify(alist, length(a) > 2 && length(b) > 3),
               "verification failed! \\(1 failure)")
  expect_error(verify(alist, a >= 2 | b > 4), "verification failed! \\(1 failure)")
  expect_error(verify(alist, 2 > 4), "verification failed! \\(1 failure)")
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
  expect_error(verify(mtcars, 2 > 1, "tree"), "unused argument \\(\"tree\")")
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

test_that("assert returns data if verification passes (using se)", {
  expect_equal(assert_(mtcars, in_set(0,1), "vs", "am"), mtcars)
  expect_equal(assert_(mtcars, within_bounds(3,5), "gear"), mtcars)
  expect_equal(assert_(mtcars, is.numeric, "mpg:carb"), mtcars)
  expect_equal(assert_(mtcars, not_na, "vs"), mtcars)
  expect_equal(assert_(mtcars, not_na, "mpg:carb"), mtcars)
  # lambdas
  expect_equal(assert_(mtcars, function(x) x%%1==0, "cyl", "vs", "am",
                       "gear", "carb"), mtcars)
  expect_equal(assert_(mtcars, function(x) x%%1==0, "gear"), mtcars)
  expect_equal(assert_(iris, function(x) nchar(as.character(x)) > 5, "Species"),
               iris)
})

test_that("assert raises error if verification fails", {
  expect_error(assert(mtcars, within_bounds(3.5,4.5), gear),
               "Vector 'gear' violates assertion 'within_bounds' 20 times \\(e.g. \\[3\\] at index 4\\)")
  expect_error(assert(mtcars, within_bounds(3,5), gear, carb),
               "Vector 'carb' violates assertion 'within_bounds' 19 times \\(e.g. \\[1\\] at index 3\\)")
  expect_error(assert(mtcars, within_bounds(3.5, 4.5), carb, gear),
               "Vector 'carb' violates assertion 'within_bounds' 22 times \\(e.g. \\[1\\] at index 3\\)\nVector 'gear' violates assertion 'within_bounds' 20 times \\(e.g. \\[3\\] at index 4\\)")
})

test_that("assert raises error if verification fails (using se)", {
  expect_error(assert_(mtcars, within_bounds(3.5,4.5), "gear"),
               "Vector 'gear' violates assertion 'within_bounds' 20 times \\(e.g. \\[3\\] at index 4\\)")
  expect_error(assert_(mtcars, within_bounds(3,5), "gear", "carb"),
               "Vector 'carb' violates assertion 'within_bounds' 19 times \\(e.g. \\[1\\] at index 3\\)")
  expect_error(assert_(mtcars, within_bounds(3.5, 4.5), "carb", "gear"),
               "Vector 'carb' violates assertion 'within_bounds' 22 times \\(e.g. \\[1\\] at index 3\\)\nVector 'gear' violates assertion 'within_bounds' 20 times \\(e.g. \\[3\\] at index 4\\)")
})

test_that("assert breaks appropriately", {
  expect_error(assert(in_set(0,1), mtcars$vs),
               "no applicable method for 'select.?' applied to an object of class \"function\"")
  expect_error(assert("tree"),
               "no applicable method for 'select.?' applied to an object of class \"character\"")
})

test_that("assert breaks appropriately (using se)", {
  expect_error(assert_(in_set(0,1), mtcars$vs),
               "no applicable method for 'select.?' applied to an object of class \"function\"")
  expect_error(assert_("tree"),
               "no applicable method for 'select.?' applied to an object of class \"character\"")
})


######################################



############### insist ###############
test_that("insist returns data if verification passes", {
  expect_equal(insist(our.iris, within_n_sds(3), Sepal.Length), our.iris)
  expect_equal(insist(our.iris.3, within_n_sds(2), Sepal.Length), our.iris.3)
  expect_equal(insist(our.iris, within_n_sds(4), Sepal.Length:Petal.Width),
               our.iris)
})

test_that("insist returns data if verification passes (using se)", {
  expect_equal(insist_(our.iris, within_n_sds(3), "Sepal.Length"), our.iris)
  expect_equal(insist_(our.iris.3, within_n_sds(2), "Sepal.Length"), our.iris.3)
  expect_equal(insist_(our.iris, within_n_sds(4), "Sepal.Length:Petal.Width"),
               our.iris)
})

test_that("insist raises error if verification fails", {
  expect_error(insist(our.iris, within_n_sds(2), Sepal.Length),
               "Vector 'Sepal.Length' violates assertion 'within_n_sds' 6 times \\(e.g. \\[7.6\\] at index 106\\)")
  expect_error(insist(our.iris.2, within_n_sds(2), Sepal.Length),
               "Vector 'Sepal.Length' violates assertion 'within_n_sds' 5 times \\(e.g. \\[7.7\\] at index 118\\)")
  expect_error(insist(our.iris, within_n_sds(3), Sepal.Length:Petal.Width),
               "Vector 'Sepal.Width' violates assertion 'within_n_sds' 1 time \\(value \\[4.4\\] at index 16\\)")
  expect_error(insist(our.iris, within_n_sds(2), Sepal.Length:Petal.Width),
               "Vector 'Sepal.Length' violates assertion 'within_n_sds' 6 times \\(e.g. \\[7.6\\] at index 106\\)\nVector 'Sepal.Width' violates assertion 'within_n_sds' 5 times \\(e.g. \\[4\\] at index 15\\)")
})

test_that("insist raises error if verification fails", {
  expect_error(insist_(our.iris, within_n_sds(2), "Sepal.Length"),
               "Vector 'Sepal.Length' violates assertion 'within_n_sds' 6 times \\(e.g. \\[7.6\\] at index 106\\)")
  expect_error(insist_(our.iris.2, within_n_sds(2), "Sepal.Length"),
               "Vector 'Sepal.Length' violates assertion 'within_n_sds' 5 times \\(e.g. \\[7.7\\] at index 118\\)")
  expect_error(insist_(our.iris, within_n_sds(3), "Sepal.Length:Petal.Width"),
               "Vector 'Sepal.Width' violates assertion 'within_n_sds' 1 time \\(value \\[4.4\\] at index 16\\)")
  expect_error(insist_(our.iris, within_n_sds(2), "Sepal.Length:Petal.Width"),
               "Vector 'Sepal.Length' violates assertion 'within_n_sds' 6 times \\(e.g. \\[7.6\\] at index 106\\)\nVector 'Sepal.Width' violates assertion 'within_n_sds' 5 times \\(e.g. \\[4\\] at index 15\\)")
})

test_that("insist breaks appropriately", {
  expect_error(insist(within_n_sds(5), mtcars$vs),
               "no applicable method for 'select.?' applied to an object of class \"function\"")
  expect_error(insist("tree"),
               "no applicable method for 'select.?' applied to an object of class \"character\"")
  expect_error(insist(iris, within_n_sds(5), Petal.Width:Species),
               "argument must be a numeric vector")
})

test_that("insist breaks appropriately (using se)", {
  expect_error(insist_(within_n_sds(5), "mtcars$vs"),
               "no applicable method for 'select.?' applied to an object of class \"function\"")
  expect_error(insist_("tree"),
               "no applicable method for 'select.?' applied to an object of class \"character\"")
  expect_error(insist_(iris, within_n_sds(5), "Petal.Width:Species"),
               "argument must be a numeric vector")
})
######################################

