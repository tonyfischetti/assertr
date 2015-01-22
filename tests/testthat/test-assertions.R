context("assertions about assertions in assertion.R")




############### verify ###############
test_that("verify returns data if verification passes", {

  # just some set up
  a <- 1
  alist <- list(a=c(1,2,3), b=c(4,5,6))

  expect_equal(verify(mtcars, drat > 2), mtcars)
  expect_equal(verify(mtcars, nrow(mtcars) > 2), mtcars)
  expect_equal(verify(mtcars, am %in% c(0,1)), mtcars)
  expect_equal(verify(mtcars, a > 0), mtcars)
  expect_equal(verify(alist, length(a) > 0), alist)
  # respects scoping rules?
  expect_equal(verify(alist, length(a) > 2), alist)
  expect_equal(verify(alist, length(a) > 2 && length(b) > 2), alist)
  expect_equal(verify(alist, a >= 1 & b > 2), alist)

})

######################################
