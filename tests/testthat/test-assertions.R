context("assertions about assertions in assertion.R")

# just some set up
a <- 1
alist <- list(a=c(1,2,3), b=c(4,5,6))

our.iris <- iris
our.iris.2 <- our.iris
our.iris.2[106,1] <- 7
our.iris.3 <- our.iris.2
our.iris.3[c(118, 119, 123, 132, 131, 136), 1] <- 7

exmpl.data <- data.frame(x=c(8, 9, 6, 5, 9, 5, 6, 7,
                             8, 9, 6, 5, 5, 6, 7),
                         y=c(82, 91, 61, 49, 40, 49, 57, 74,
                             78, 90, 61, 49, 51, 62, 68))

nexmpl.data <- exmpl.data
nexmpl.data[12,2] <- NA

mnexmpl.data <- nexmpl.data
mnexmpl.data[12,1] <- NA

nanmnexmpl.data <- mnexmpl.data
nanmnexmpl.data[10,1] <- 0/0

test.df <- data.frame(x = c(0, 1, 2))
test.df2 <- data.frame(x = c(0, 1, 2),
                       y = c(2, 1.5, 1),
                       z = c(0,NA, -1))

# custom error (or success) messages
not.helpful <- function(message, ...){
  stop("unspecified error", call.=FALSE)
}

give.validation <- function(data, ...){
  return("great job!")
}

just.show.error <- function(err, ...){
  lapply(err, summary)
}

# helper functions for verifying success_append results
success_result <- function(verb, the_call, columns, row_redux_call) {
  row_redux_message <- ""
  if (!is.na(row_redux_call))
    row_redux_message <- paste0(" on ", row_redux_call, " row reduction")
  msg <- paste0("verification [", the_call, "]", row_redux_message, " passed!")
  success <- list(
    verb = verb,
    message = msg,
    call = the_call,
    columns = columns,
    row_redux_call = row_redux_call
  )
  class(success) <- c("assertr_success", "success", "condition")
  list(success)
}
get_assertr_success <- function(assertion) {
  attr(assertion, "assertr_success")
}

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

test_that("verify returns TRUE if verification passes (and we use `success_logical`)", {
  expect_true(verify(mtcars, drat > 2, success_fun=success_logical))
  expect_true(verify(mtcars, mtcars$drat > 2, success_fun=success_logical))
  expect_true(verify(mtcars, mtcars$drat > 2, success_fun=success_logical))
  expect_true(verify(mtcars, nrow(mtcars) > 30, success_fun=success_logical))
  expect_true(verify(mtcars, am %in% c(0,1,2), success_fun=success_logical))
  expect_true(verify(mtcars, am %in% c(0,1), success_fun=success_logical))
  # looks to parent frame scope?
  expect_true(verify(mtcars, a > 0, success_fun=success_logical))
  expect_true(verify(mtcars, nrow(iris) > 140, success_fun=success_logical))
  expect_true(verify(alist, length(a) > 0, success_fun=success_logical))
  # respects scoping rules?
  expect_true(verify(alist, length(a) > 2, success_fun=success_logical))
  expect_true(verify(alist, length(a) > 2 && length(b) > 2, success_fun=success_logical))
  expect_true(verify(alist, a >= 1 & b > 2, success_fun=success_logical))
  expect_true(verify(alist, a > 2 | b > 2, success_fun=success_logical))
  expect_true(verify(alist, 3 > 2, success_fun=success_logical))
})

test_that("verify returns success results if verification passes (and we use `success_append`)", {
  expect_equal(
    get_assertr_success(verify(mtcars, drat > 2, success_fun=success_append)),
    success_result("verify", "drat > 2", NA, NA))
  expect_equal(
    get_assertr_success(verify(mtcars, mtcars$drat > 2, success_fun=success_append)),
    success_result("verify", "mtcars$drat > 2", NA, NA))
  expect_equal(
    get_assertr_success(verify(mtcars, mtcars$drat > 2, success_fun=success_append)),
    success_result("verify", "mtcars$drat > 2", NA, NA))
  expect_equal(
    get_assertr_success(verify(mtcars, nrow(mtcars) > 30, success_fun=success_append)),
    success_result("verify", "nrow(mtcars) > 30", NA, NA))
  expect_equal(
    get_assertr_success(verify(mtcars, am %in% c(0,1,2), success_fun=success_append)),
    success_result("verify", "am %in% c(0, 1, 2)", NA, NA))
  expect_equal(
    get_assertr_success(verify(mtcars, am %in% c(0,1), success_fun=success_append)),
    success_result("verify", "am %in% c(0, 1)", NA, NA))
  # looks to parent frame scope?
  expect_equal(
    get_assertr_success(verify(mtcars, a > 0, success_fun=success_append)),
    success_result("verify", "a > 0", NA, NA))
  expect_equal(
    get_assertr_success(verify(mtcars, nrow(iris) > 140, success_fun=success_append)),
    success_result("verify", "nrow(iris) > 140", NA, NA))
  expect_equal(
    get_assertr_success(verify(alist, length(a) > 0, success_fun=success_append)),
    success_result("verify", "length(a) > 0", NA, NA))
  # respects scoping rules?
  expect_equal(
    get_assertr_success(verify(alist, length(a) > 2, success_fun=success_append)),
    success_result("verify", "length(a) > 2", NA, NA))
  expect_equal(
    get_assertr_success(verify(alist, length(a) > 2 && length(b) > 2, success_fun=success_append)),
    success_result("verify", "length(a) > 2 && length(b) > 2", NA, NA))
  expect_equal(
    get_assertr_success(verify(alist, a >= 1 & b > 2, success_fun=success_append)),
    success_result("verify", "a >= 1 & b > 2", NA, NA))
  expect_equal(
    get_assertr_success(verify(alist, a > 2 | b > 2, success_fun=success_append)),
    success_result("verify", "a > 2 | b > 2", NA, NA))
  expect_equal(
    get_assertr_success(verify(alist, 3 > 2, success_fun=success_append)),
    success_result("verify", "3 > 2", NA, NA))
})

test_that("verify performs custom success function if verification passes", {
  expect_equal(verify(mtcars, drat > 2, success_fun=give.validation),
               "great job!")
  expect_equal(verify(mtcars, mtcars$drat > 2, function(x, ...){return("noice!")}),
               "noice!")
})

test_that("verify raises error if verification fails", {
  expect_equal(verify(mtcars, drat > 3, error_fun = error_logical), FALSE)
  expect_output(verify(mtcars, drat > 3, error_fun = just.show.error),
                "verification \\[drat > 3\\] failed! \\(4 failures\\)")

  expect_equal(verify(mtcars, nrow(mtcars) > 34, error_fun = error_logical), FALSE)
  expect_output(verify(mtcars, nrow(mtcars) > 34, error_fun = just.show.error),
                "verification \\[nrow\\(mtcars\\) > 34\\] failed! \\(1 failure\\)")

  expect_equal(verify(mtcars, am %in% c(1,2), error_fun = error_logical), FALSE)
  expect_output(verify(mtcars, am %in% c(1,2), error_fun = just.show.error),
                "verification \\[am %in% c\\(1, 2\\)\\] failed! \\(19 failures\\)")

  # looks to parent frame scope?
  expect_equal(verify(mtcars, a < 0, error_fun = error_logical), FALSE)
  expect_output(verify(mtcars, a < 0, error_fun = just.show.error),
                "verification \\[a < 0\\] failed! \\(1 failure\\)")

  # respects scoping rules?
  expect_equal(verify(alist, length(a) == 1, error_fun = error_logical), FALSE)
  expect_output(verify(alist, length(a) == 1, error_fun = just.show.error),
                "verification \\[length\\(a\\) == 1\\] failed! \\(1 failure\\)")
  expect_equal(verify(alist, length(a) > 4, error_fun = error_logical), FALSE)
  expect_output(verify(alist, length(a) > 4, error_fun = just.show.error),
                "verification \\[length\\(a\\) > 4\\] failed! \\(1 failure\\)")
  expect_output(verify(alist, length(a) > 2 && length(b) > 3, error_fun = just.show.error),
                "verification \\[length\\(a\\) > 2 && length\\(b\\) > 3\\] failed! \\(1 failure\\)")
  expect_output(verify(alist, a >= 2 | b > 4, error_fun = just.show.error),
                "verification \\[a >= 2 | b > 4\\] failed! \\(1 failure\\)")
  expect_output(verify(alist, 2 > 4, error_fun = just.show.error),
                "verification \\[2 > 4\\] failed! \\(1 failure\\)")
  # NA values don't compare TRUE
  expect_output(verify(test.df2, z > -2, error_fun = just.show.error),
                "verification \\[z > -2\\] failed! \\(1 failure\\)")
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
  expect_error(verify(mtcars, 1 > 0, "tree"), "could not find function \"success_fun\"")
  expect_error(verify(mtcars, d > 1), "object 'd' not found")
})

test_that("verify works within functions", {
  my_verify <- function(data, expr, success_fun) {
    verify(data, !!rlang::enexpr(expr), success_fun=success_fun)
  }

  expect_true(my_verify(mtcars, drat > 2, success_fun=success_logical))
})

test_that("verify works with long predicates (fix #80)", {
  my_data <- data.frame(COMMENT=c("foo", "bar", "baz", "Positive Pre-dose"),
                        USUBJID = "ABCDEFGHIJKLMNOPQRTSU",
                        stringsAsFactors=FALSE)
  expect_output(
    expect_error(
      verify(my_data,
             is.na(COMMENT) | (COMMENT %in% "Positive Pre-dose" & USUBJID %in% "ABCDEFGHIJKLMNOPQRTSU")),
      "assertr stopped execution"
    ),
    regexp='is.na(COMMENT) | (COMMENT %in% "Positive Pre-dose" & USUBJID %in%      "ABCDEFGHIJKLMNOPQRTSU")',
    fixed=TRUE
  )
})

test_that("skip_chain_opts doesn't affect functionality outside chain for verify", {
  expect_equal(verify(mtcars, mtcars$drat > 2, skip_chain_opts=TRUE), mtcars)
  expect_equal(verify(mtcars, nrow(mtcars) > 30, skip_chain_opts=TRUE), mtcars)
  expect_true(verify(mtcars, mtcars$drat > 2, success_fun=success_logical, skip_chain_opts=TRUE))
  expect_true(verify(mtcars, nrow(mtcars) > 30, success_fun=success_logical, skip_chain_opts=TRUE))
  expect_equal(verify(mtcars, drat > 3, error_fun = error_logical, skip_chain_opts=TRUE), FALSE)
  expect_output(verify(mtcars, drat > 3, error_fun = just.show.error, skip_chain_opts=TRUE),
                "verification \\[drat > 3\\] failed! \\(4 failures\\)")
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
  expect_equal(assert(mtcars, function(x) if(x%%1!=0) return(FALSE), gear), mtcars)
  expect_equal(assert(iris, function(x) nchar(as.character(x)) > 5, Species),
               iris)
})

test_that("assert returns TRUE if verification passes (w/ `success_logical`)", {
  expect_true(assert(mtcars, in_set(0,1), vs, am, success_fun=success_logical))
  expect_true(assert(mtcars, within_bounds(3,5), gear, success_fun=success_logical))
  expect_true(assert(mtcars, is.numeric, mpg:carb, success_fun=success_logical))
  expect_true(assert(mtcars, not_na, vs, success_fun=success_logical))
  expect_true(assert(mtcars, not_na, mpg:carb, success_fun=success_logical))
  # lambdas
  expect_true(assert(mtcars, function(x) x%%1==0, cyl, vs, am, gear, carb,
                     success_fun=success_logical))
  expect_true(assert(mtcars, function(x) if(x%%1!=0) return(FALSE), gear,
                     success_fun=success_logical))
  expect_true(assert(iris, function(x) nchar(as.character(x)) > 5, Species,
                     success_fun=success_logical))
})

test_that("assert returns correct result if verification passes (w/ `success_append`)", {
  expect_equal(
    get_assertr_success(assert(mtcars, in_set(0,1), vs, am, success_fun=success_append)),
    success_result("assert", "in_set(0, 1)", c("vs", "am"), NA))
  expect_equal(
    get_assertr_success(assert(mtcars, within_bounds(3,5), gear, success_fun=success_append)),
    success_result("assert", "within_bounds(3, 5)", "gear", NA))
  expect_equal(
    get_assertr_success(assert(mtcars, is.numeric, mpg:carb, success_fun=success_append)),
    success_result("assert", "is.numeric", colnames(mtcars), NA))
  expect_equal(
    get_assertr_success(assert(mtcars, not_na, vs, success_fun=success_append)),
    success_result("assert", "not_na", "vs", NA))
  expect_equal(
    get_assertr_success(assert(mtcars, not_na, mpg:carb, success_fun=success_append)),
    success_result("assert", "not_na", colnames(mtcars), NA))
  expect_equal(
    get_assertr_success(assert(mtcars, function(x) x%%1==0, cyl, vs, am, gear, carb,
                               success_fun=success_append)),
    success_result("assert", "function(x) x%%1 == 0", c("cyl", "vs", "am", "gear", "carb"), NA))
  expect_equal(
    get_assertr_success(assert(mtcars, function(x) if(x%%1!=0) return(FALSE), gear,
                               success_fun=success_append)),
    success_result("assert", "function(x) if (x%%1 != 0) return(FALSE)", "gear", NA))
  expect_equal(
    get_assertr_success(assert(iris, function(x) nchar(as.character(x)) > 5, Species,
                               success_fun=success_append)),
    success_result("assert", "function(x) nchar(as.character(x)) > 5", "Species", NA))
})

test_that("assert performs custom success function if verification passes", {
  expect_equal(assert(mtcars, not_na, mpg:carb, success_fun=give.validation), "great job!")
  expect_equal(assert(mtcars, within_bounds(3,5), gear, success_fun=function(x, ...) {return("noice!")}), "noice!")

})

test_that("assert raises error if verification fails", {
  expect_equal(assert(mtcars, within_bounds(3.5,4.5), gear, error_fun = error_logical), FALSE)
  expect_output(assert(mtcars, within_bounds(3.5,4.5), gear, error_fun = just.show.error),
               "Column 'gear' violates assertion 'within_bounds\\(3.5, 4.5\\)' 20 times.*")
  expect_equal(assert(mtcars, within_bounds(3,5), gear, carb, error_fun = error_logical), FALSE)
  expect_output(assert(mtcars, within_bounds(3,5), gear, carb, error_fun = just.show.error),
                 "Column 'carb' violates assertion 'within_bounds\\(3, 5\\)' 19 times")
  expect_equal(assert(mtcars, within_bounds(3.5, 4.5), carb, gear, error_fun = error_logical), FALSE)
  expect_output(assert(mtcars, within_bounds(3.5, 4.5), carb, gear, error_fun = just.show.error),
               "Column 'carb' violates assertion 'within_bounds\\(3.5, 4.5\\)' 22 times.+Column 'gear' violates assertion 'within_bounds\\(3.5, 4.5\\)' 20 times")
})

test_that("assert raises *custom error* if verification fails", {
  expect_error(assert(mtcars, within_bounds(3.5,4.5), gear, error_fun=not.helpful),
               "unspecified error")
  expect_error(assert(mtcars, within_bounds(3,5), gear, carb, error_fun=not.helpful),
               "unspecified error")
})

test_that("assert breaks appropriately", {
  expect_error(assert(in_set(0,1), mtcars$vs),
               "assert requires columns to be selected. Check number of arguments")
  expect_error(assert(mtcars, in_set(0,1), vs, tree),
               "object 'tree' not found")
  expect_error(assert(mtcars, in_set(0,1), vs, "tree"))
  expect_error(assert("tree"),
               "argument \"predicate\" is missing, with no default")
})

test_that("skip_chain_opts doesn't affect functionality outside chain for assert", {
  expect_equal(assert(mtcars, in_set(0,1), vs, am, skip_chain_opts=TRUE), mtcars)
  expect_equal(assert(mtcars, within_bounds(3,5), gear, skip_chain_opts=TRUE), mtcars)
  expect_true(assert(mtcars, in_set(0,1), vs, am, success_fun=success_logical, skip_chain_opts=TRUE))
  expect_true(assert(mtcars, within_bounds(3,5), gear, success_fun=success_logical, skip_chain_opts=TRUE))
  expect_equal(assert(mtcars, within_bounds(3.5,4.5), gear, error_fun = error_logical, skip_chain_opts=TRUE), FALSE)
  expect_output(assert(mtcars, within_bounds(3.5,4.5), gear, error_fun = just.show.error, skip_chain_opts=TRUE),
                "Column 'gear' violates assertion 'within_bounds\\(3.5, 4.5\\)' 20 times.*")
})

######################################





############### assert_rows ###############
test_that("assert_rows returns data if verification passes", {
  expect_equal(assert_rows(mtcars, rowSums, within_bounds(0,2), vs, am), mtcars)
  expect_equal(assert_rows(mtcars, num_row_NAs, within_bounds(0,.1), dplyr::everything()),
               mtcars)
  expect_equal(assert_rows(mtcars, rowSums, within_bounds(5,16), cyl, carb),
               mtcars)
  expect_equal(assert_rows(mnexmpl.data, num_row_NAs, within_bounds(0,2),
                           dplyr::everything()), mnexmpl.data)
  expect_equal(assert_rows(nexmpl.data, num_row_NAs, function(x) x < 2,
                           dplyr::everything()), nexmpl.data)
  expect_equal(assert_rows(mtcars, rowSums, function(x) if(x>16) return(FALSE), carb, cyl),
               mtcars)
})

test_that("assert_rows returns TRUE if verification passes (w/ `success_logical`", {
  expect_true(assert_rows(mtcars, rowSums, within_bounds(0,2), vs, am, success_fun=success_logical))
  expect_true(assert_rows(mtcars, num_row_NAs, within_bounds(0,.1), dplyr::everything(), success_fun=success_logical))
  expect_true(assert_rows(mtcars, rowSums, within_bounds(5,16), cyl, carb, success_fun=success_logical))
  expect_true(assert_rows(mnexmpl.data, num_row_NAs, within_bounds(0,2),
                           dplyr::everything(), success_fun=success_logical))
  expect_true(assert_rows(nexmpl.data, num_row_NAs, function(x) x < 2,
                           dplyr::everything(), success_fun=success_logical))
  expect_true(assert_rows(mtcars, rowSums, function(x) if(x>16) return(FALSE), carb, cyl, success_fun=success_logical))
})

test_that("assert_rows returns correct result if verification passes (w/ `success_append`", {
  expect_equal(
    get_assertr_success(assert_rows(mtcars, rowSums, within_bounds(0,2), vs, am, success_fun=success_append)),
    success_result("assert_rows", "within_bounds(0, 2)", c("vs", "am"), "rowSums"))
  expect_equal(
    get_assertr_success(assert_rows(mtcars, num_row_NAs, within_bounds(0,.1), dplyr::everything(), success_fun=success_append)),
    success_result("assert_rows", "within_bounds(0, 0.1)", colnames(mtcars), "num_row_NAs"))
  expect_equal(
    get_assertr_success(assert_rows(mtcars, rowSums, within_bounds(5,16), cyl, carb, success_fun=success_append)),
    success_result("assert_rows", "within_bounds(5, 16)", c("cyl", "carb"), "rowSums"))
  expect_equal(
    get_assertr_success(assert_rows(mnexmpl.data, num_row_NAs, within_bounds(0,2),
                                    dplyr::everything(), success_fun=success_append)),
    success_result("assert_rows", "within_bounds(0, 2)", colnames(mnexmpl.data), "num_row_NAs"))
  expect_equal(
    get_assertr_success(assert_rows(nexmpl.data, num_row_NAs, function(x) x < 2,
                                    dplyr::everything(), success_fun=success_append)),
    success_result("assert_rows", "function(x) x < 2", colnames(mnexmpl.data), "num_row_NAs"))
  expect_equal(
    get_assertr_success(assert_rows(mtcars, rowSums, function(x) if(x>16) return(FALSE), carb, cyl, success_fun=success_append)),
    success_result("assert_rows", "function(x) if (x > 16) return(FALSE)", c("carb", "cyl"), "rowSums"))
})

test_that("assert_rows performs custom success function if verification passes", {
  expect_equal(assert_rows(mtcars, rowSums, within_bounds(0,2), vs, am, success_fun=give.validation), "great job!")
  expect_equal(assert_rows(mtcars, rowSums, within_bounds(5,16), cyl, carb, success_fun=function(x, ...) {return("noice!")}), "noice!")

})

test_that("assert_rows raises error if verification fails", {
  expect_output(assert_rows(mtcars, rowSums, within_bounds(1,2), vs, am, error_fun = just.show.error),
               "Data frame row reduction 'rowSums' violates predicate 'within_bounds\\(1, 2\\)' 12 times")
  expect_output(assert_rows(mtcars, num_row_NAs, within_bounds(1,2), dplyr::everything(), error_fun = just.show.error),
                "Data frame row reduction 'num_row_NAs' violates predicate 'within_bounds\\(1, 2\\)' 32 times")
  expect_output(assert_rows(mtcars, rowSums, function(x) if(x==10) return(FALSE), carb, cyl, error_fun = just.show.error),
                "Data frame row reduction 'rowSums' violates predicate 'function\\(x\\) if \\(x == 10\\) return\\(FALSE\\)' 8 times")
  expect_output(assert_rows(mnexmpl.data, num_row_NAs, within_bounds(0,1), dplyr::everything(), error_fun = just.show.error),
                "Data frame row reduction 'num_row_NAs' violates predicate 'within_bounds\\(0, 1\\)' 1 time")
})

test_that("assert_rows raises *custom error* if verification fails", {
  expect_error(assert_rows(mnexmpl.data, num_row_NAs, within_bounds(0,1), dplyr::everything(), error_fun=not.helpful),
               "unspecified error")
})

test_that("assert_rows breaks appropriately", {
  expect_error(assert_rows(in_set(0,1), mtcars$vs),
               "argument \"predicate\" is missing, with no default")
  expect_error(assert_rows(rowSums, in_set(0,1), mtcars$vs),
               "assert_rows requires columns to be selected. Check number of arguments")
  expect_error(assert_rows(mtcars, rowSums, in_set(0,1,2), vs, am, tree),
               "object 'tree' not found")
  expect_error(assert_rows(mtcars, rowSums, in_set(0,1,2), vs, am, "tree"))
  expect_error(assert_rows("tree"),
               "argument \"row_reduction_fn\" is missing, with no default")
})

test_that("skip_chain_opts doesn't affect functionality outside chain for assert_rows", {
  expect_equal(assert_rows(mtcars, rowSums, within_bounds(0,2), vs, am, skip_chain_opts=TRUE), mtcars)
  expect_equal(assert_rows(mtcars, num_row_NAs, within_bounds(0,.1), dplyr::everything(), skip_chain_opts=TRUE),
               mtcars)
  expect_true(assert_rows(mtcars, rowSums, within_bounds(0,2), vs, am, success_fun=success_logical, skip_chain_opts=TRUE))
  expect_true(assert_rows(mtcars, num_row_NAs, within_bounds(0,.1), dplyr::everything(), success_fun=success_logical, skip_chain_opts=TRUE))
  expect_output(assert_rows(mtcars, rowSums, within_bounds(1,2), vs, am, error_fun = just.show.error, skip_chain_opts=TRUE),
                "Data frame row reduction 'rowSums' violates predicate 'within_bounds\\(1, 2\\)' 12 times")
  expect_output(assert_rows(mtcars, num_row_NAs, within_bounds(1,2), dplyr::everything(), error_fun = just.show.error, skip_chain_opts=TRUE),
                "Data frame row reduction 'num_row_NAs' violates predicate 'within_bounds\\(1, 2\\)' 32 times")
})

######################################


############### insist ###############
test_that("insist returns data if verification passes", {
  expect_equal(insist(our.iris, within_n_sds(3), Sepal.Length), our.iris)
  expect_equal(insist(our.iris.3, within_n_sds(2), Sepal.Length), our.iris.3)
  expect_equal(insist(our.iris, within_n_sds(4), Sepal.Length:Petal.Width),
               our.iris)
})

test_that("insist returns TRUE if verification passes (w/ success_logical)", {
  expect_true(insist(our.iris, within_n_sds(3), Sepal.Length, success_fun=success_logical))
  expect_true(insist(our.iris.3, within_n_sds(2), Sepal.Length, success_fun=success_logical))
  expect_true(insist(our.iris, within_n_sds(4), Sepal.Length:Petal.Width, success_fun=success_logical))
})

test_that("insist returns correct result if verification passes (w/ `success_append`", {
  expect_equal(
    get_assertr_success(insist(our.iris, within_n_sds(3), Sepal.Length, success_fun=success_append)),
    success_result("insist", "within_n_sds(3)", "Sepal.Length", NA))
  expect_equal(
    get_assertr_success(insist(our.iris.3, within_n_sds(2), Sepal.Length, success_fun=success_append)),
    success_result("insist", "within_n_sds(2)", "Sepal.Length", NA))
  expect_equal(
    get_assertr_success(insist(our.iris, within_n_sds(4), Sepal.Length:Petal.Width, success_fun=success_append)),
    success_result("insist", "within_n_sds(4)", c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), NA))
})

test_that("insist performs custom success function if verification passes", {
  expect_equal(insist(our.iris, within_n_sds(3), Sepal.Length, success_fun=give.validation), "great job!")
  expect_equal(insist(our.iris.3, within_n_sds(2), Sepal.Length, success_fun=function(x, ...){return("noice!")}), "noice!")
})

test_that("insist raises error if verification fails", {
  expect_output(insist(our.iris, within_n_sds(2), Sepal.Length, error_fun = just.show.error),
                "Column 'Sepal.Length' violates assertion 'within_n_sds\\(2\\)' 6 times")
  expect_output(insist(our.iris.2, within_n_sds(2), Sepal.Length, error_fun = just.show.error),
                "Column 'Sepal.Length' violates assertion 'within_n_sds\\(2\\)' 5 times")
  expect_output(insist(our.iris, within_n_sds(3), Sepal.Length:Petal.Width, error_fun = just.show.error),
                "Column 'Sepal.Width' violates assertion 'within_n_sds\\(3\\)' 1 time")
  expect_output(insist(our.iris, within_n_sds(2), Sepal.Length:Petal.Width, error_fun = just.show.error),
                "Column 'Sepal.Length' violates assertion 'within_n_sds\\(2\\)' 6 times.*Column 'Sepal.Width' violates assertion 'within_n_sds\\(2\\)' 5 times")
})

test_that("insist raises *custom error* if verification fails", {
  expect_error(insist(our.iris, within_n_sds(2), Sepal.Length, error_fun=not.helpful),
               "unspecified error")
  expect_error(insist(our.iris.2, within_n_sds(2), Sepal.Length, error_fun=not.helpful),
               "unspecified error")
})

test_that("insist breaks appropriately", {
  expect_error(insist(within_n_sds(5), mtcars$vs),
               "insist requires columns to be selected. Check number of arguments")
  expect_error(insist(mtcars, within_n_sds(5), "vs:am"))
  expect_error(insist(mtcars, within_n_sds(5), tree),
               "object 'tree' not found")
  expect_error(insist("tree"),
               "argument \"predicate_generator\" is missing, with no default")
  expect_error(insist(iris, within_n_sds(5), Petal.Width:Species),
               "argument must be a numeric vector")
})

test_that("skip_chain_opts doesn't affect functionality outside chain for insist", {
  expect_equal(insist(our.iris, within_n_sds(3), Sepal.Length, skip_chain_opts=TRUE), our.iris)
  expect_equal(insist(our.iris.3, within_n_sds(2), Sepal.Length, skip_chain_opts=TRUE), our.iris.3)
  expect_equal(insist(our.iris, within_n_sds(3), Sepal.Length, success_fun=give.validation, skip_chain_opts=TRUE),
               "great job!")
  expect_equal(insist(our.iris.3, within_n_sds(2), Sepal.Length, success_fun=function(x, ...){return("noice!")}, skip_chain_opts=TRUE),
               "noice!")
  expect_output(insist(our.iris, within_n_sds(2), Sepal.Length, error_fun = just.show.error, skip_chain_opts=TRUE),
                "Column 'Sepal.Length' violates assertion 'within_n_sds\\(2\\)' 6 times")
  expect_output(insist(our.iris.2, within_n_sds(2), Sepal.Length, error_fun = just.show.error, skip_chain_opts=TRUE),
                "Column 'Sepal.Length' violates assertion 'within_n_sds\\(2\\)' 5 times")
})

######################################


############### insist rows ###############
test_that("insist_rows returns data if verification passes", {
  expect_equal(insist_rows(our.iris, maha_dist, within_n_sds(6), dplyr::everything()), our.iris)
  expect_equal(insist_rows(our.iris, maha_dist, within_n_mads(10), Sepal.Length:Species), our.iris)
  expect_equal(insist_rows(our.iris, maha_dist, within_n_mads(11), Sepal.Length:Petal.Width),
               our.iris)
})

test_that("insist_rows returns correct result if verification passes (w/ `success_append`", {
  expect_equal(
    get_assertr_success(insist_rows(our.iris, maha_dist, within_n_sds(6), dplyr::everything(), success_fun=success_append)),
    success_result("insist_rows", "within_n_sds(6)", colnames(our.iris), "maha_dist"))
  expect_equal(
    get_assertr_success(insist_rows(our.iris, maha_dist, within_n_mads(10), Sepal.Length:Species, success_fun=success_append)),
    success_result("insist_rows", "within_n_mads(10)", colnames(our.iris), "maha_dist"))
  expect_equal(
    get_assertr_success(insist_rows(our.iris, maha_dist, within_n_mads(11), Sepal.Length:Petal.Width, success_fun=success_append)),
    success_result("insist_rows", "within_n_mads(11)", c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), "maha_dist"))
})

test_that("insist_rows raises error if verification fails", {
  expect_output(insist_rows(our.iris, maha_dist, within_n_sds(4), dplyr::everything(), error_fun = just.show.error),
                "Data frame row reduction 'maha_dist' violates predicate 'within_n_sds\\(4\\)' 1 time")
  expect_output(insist_rows(our.iris, maha_dist, within_n_sds(2), dplyr::everything(), error_fun = just.show.error),
                "Data frame row reduction 'maha_dist' violates predicate 'within_n_sds\\(2\\)' 8 times")
  expect_output(insist_rows(our.iris, maha_dist, within_n_mads(5), Sepal.Length:Species, error_fun = just.show.error),
                "Data frame row reduction 'maha_dist' violates predicate 'within_n_mads\\(5\\)' 1 time")
  expect_output(insist_rows(our.iris, maha_dist, within_n_mads(5), Sepal.Length:Petal.Width, error_fun = just.show.error),
                "Data frame row reduction 'maha_dist' violates predicate 'within_n_mads\\(5\\)' 4 times")
})

test_that("insist_rows raises *custom error* if verification fails", {
  expect_error(insist_rows(our.iris, maha_dist, within_n_mads(5), Sepal.Length:Petal.Width, error_fun = not.helpful),
               "unspecified error")
})

test_that("insist_rows breaks appropriately", {
  expect_error(insist_rows(within_n_sds(5), mtcars$vs),
               "argument \"predicate_generator\" is missing, with no default")
  expect_error(insist_rows(mtcars, within_n_sds(10), vs),
               "object 'vs' not found")
  expect_error(insist_rows(mtcars, maha_dist, within_n_sds(10), vs),
               "\"data\" needs to have at least two columns")
  expect_error(insist_rows(mtcars, maha_dist, within_bound(0, 10), vs, am),
               "could not find function \"within_bound\"")
  expect_error(insist_rows(), "argument \"row_reduction_fn\" is missing, with no default")
  expect_error(insist_rows(mtcars), "argument \"row_reduction_fn\" is missing, with no default")
  expect_error(insist_rows(mtcars, maha_dist, am, vs),
               "object 'am' not found")
  expect_error(insist_rows(mtcars, maha_dist, am, vs, carb),
               "object 'am' not found")

  expect_error(insist_rows(lm(Petal.Length ~ Petal.Width, data=iris)),
               "argument \"row_reduction_fn\" is missing, with no default")
})

test_that("skip_chain_opts doesn't affect functionality outside chain for insist_rows", {
  expect_equal(insist_rows(our.iris, maha_dist, within_n_sds(6), dplyr::everything(), skip_chain_opts=TRUE),
               our.iris)
  expect_equal(insist_rows(our.iris, maha_dist, within_n_mads(10), Sepal.Length:Species, skip_chain_opts=TRUE),
               our.iris)
  expect_output(insist_rows(our.iris, maha_dist, within_n_sds(4), dplyr::everything(), error_fun = just.show.error, skip_chain_opts=TRUE),
                "Data frame row reduction 'maha_dist' violates predicate 'within_n_sds\\(4\\)' 1 time")
  expect_output(insist_rows(our.iris, maha_dist, within_n_sds(2), dplyr::everything(), error_fun = just.show.error, skip_chain_opts=TRUE),
                "Data frame row reduction 'maha_dist' violates predicate 'within_n_sds\\(2\\)' 8 times")
  expect_error(insist_rows(within_n_sds(5), mtcars$vs, skip_chain_opts=TRUE),
               "argument \"predicate_generator\" is missing, with no default")
  expect_error(insist_rows(mtcars, within_n_sds(10), vs, skip_chain_opts=TRUE),
               "object 'vs' not found")
})

###########################################


########## chaining works ############

# A special error function for these tests, produces the error but no
# standard output.
error_no_output <- function (list_of_errors, data=NULL, ...) {
  stop("assertr stopped execution", call.=FALSE)
}

strip_attributes <- function(d){
  attr(d, "assertr_in_chain_error_fun_override") <- NULL
  attr(d, "assertr_in_chain_success_fun_override") <- NULL
  d
}

ret_num_off_errors <- function(errors, data=NULL, warn=FALSE, ...){
  if(!is.null(data) && !is.null(attr(data, "assertr_errors")))
    errors <- append(attr(data, "assertr_errors"), errors)
  num.of.errors <- length(errors)
  cat(sprintf("There %s %d error%s:\n",
              ifelse(num.of.errors==1,"is", "are"),
              num.of.errors,
              ifelse(num.of.errors==1,"", "s")))
}

success_message <- function(data=NULL, ...) {
  message("Assertion passed")
  return(data)
}

error_message <- function(errors, data=NULL) {
  message("Assertion failed")
  return(data)
}

##### !!! chaining: assert
test_that("assert works with chaining", {

  # only assert with no error
  code_to_test <- function() {
    test.df %>%
      chain_start %>%
      assert(in_set(0,1,2), x) %>%
      chain_end %>% strip_attributes
  }
  expect_equal(code_to_test(), test.df)

  # only assert with no error stores success correctly
  code_to_test <- function() {
    test.df %>%
      chain_start(store_success = TRUE) %>%
      assert(in_set(0,1,2), x) %>%
      chain_end %>% strip_attributes
  }
  expect_equal(
    get_assertr_success(code_to_test()),
    success_result("assert", "in_set(0, 1, 2)", "x", NA))

  # only assert with print state on success in chain
  code_to_test <- function() {
    test.df %>%
      chain_start %>%
      assert(in_set(0,1,2), x, success_fun = success_message, skip_chain_opts = TRUE) %>%
      chain_end %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion passed")

  # only assert with error
  code_to_test <- function() {
    test.df %>%
      chain_start %>%
      assert(in_set(0,1), x) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There is 1 error")

  # only assert with printed state on error in chain
  code_to_test <- function() {
    test.df %>%
      chain_start %>%
      assert(in_set(0,1), x, error_fun = error_message, skip_chain_opts = TRUE) %>%
      chain_end %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion failed")

  # two asserts with no error
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert(in_set(0,1,2), x) %>%
      assert(within_bounds(1,2),y) %>%
      chain_end %>% strip_attributes
  }
  expect_equal(code_to_test(), test.df2)

  # two asserts with no error and storing success results
  code_to_test <- function() {
    test.df2 %>%
      chain_start(store_success = TRUE) %>%
      assert(in_set(0,1,2), x) %>%
      assert(within_bounds(1,2),y) %>%
      chain_end %>% strip_attributes
  }
  expect_equal(
    get_assertr_success(code_to_test()),
    append(
      success_result("assert", "in_set(0, 1, 2)", "x", NA),
      success_result("assert", "within_bounds(1, 2)", "y", NA)
    )
  )

  # only assert with error (1st)
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert(in_set(0,1), x) %>%
      assert(within_bounds(1,2),y) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There is 1 error")

  # two asserts with error (1st) and chain callback overwriten
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert(in_set(0,1), x, error_fun = error_message, skip_chain_opts = TRUE) %>%
      assert(within_bounds(1,2),y) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion failed")

  # only assert with error (2st)
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert(within_bounds(1,2),y) %>%
      assert(in_set(0,1), x) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There is 1 error")

  # only assert with two errors
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert(within_bounds(1,1.5),y) %>%
      assert(in_set(0,1), x) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There are 2 errors")

  # two asserts with two errors and one callback overwritten in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert(within_bounds(1,1.5),y, error_fun = error_message, skip_chain_opts = TRUE) %>%
      assert(in_set(0,1), x) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(expect_message(code_to_test(), "Assertion failed"), "There is 1 error")
})


##### !!! chaining: assert_rows
test_that("assert_rows works with chaining", {

  # only assert_rows with no error
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert_rows(col_concat, is_uniq, x, y) %>%
      chain_end %>% strip_attributes
  }
  expect_equal(code_to_test(), test.df2)

  # only assert_rows with no error and storing success result
  code_to_test <- function() {
    test.df2 %>%
      chain_start(store_success = TRUE) %>%
      assert_rows(col_concat, is_uniq, x, y) %>%
      chain_end %>% strip_attributes
  }
  expect_equal(
    get_assertr_success(code_to_test()),
    success_result("assert_rows", "is_uniq", c("x", "y"), "col_concat")
  )

  # only assert_rows with print state on success in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert_rows(col_concat, is_uniq, x, y, success_fun = success_message, skip_chain_opts = TRUE) %>%
      chain_end %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion passed")

  # only assert_row with error
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert_rows(rowSums, not_na, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There is 1 error")

  # only assert_rows with printed state on error in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert_rows(rowSums, not_na, x, y, z, error_fun = error_message, skip_chain_opts = TRUE) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion failed")

  # two asserts_row with no error
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert_rows(rowSums, is.numeric, x, y, z) %>%
      assert_rows(col_concat, is.character, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_equal(code_to_test(), test.df2)

  # two asserts_row with no error ans storing success
  code_to_test <- function() {
    test.df2 %>%
      chain_start(store_success = TRUE) %>%
      assert_rows(rowSums, is.numeric, x, y, z) %>%
      assert_rows(col_concat, is.character, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_equal(
    get_assertr_success(code_to_test()),
    append(
      success_result("assert_rows", "is.numeric", c("x", "y", "z"), "rowSums"),
      success_result("assert_rows", "is.character", c("x", "y", "z"), "col_concat")
    )
  )

  # only assert_rows with error (1st)
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert_rows(rowSums, is.character, x, y, z) %>%
      assert_rows(col_concat, is.character, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There is 1 error")

  # two assert_rows with error (1st) and callback overwritten in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert_rows(rowSums, is.character, x, y, z, error_fun = error_message, skip_chain_opts = TRUE) %>%
      assert_rows(col_concat, is.character, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion failed")

  # only assert_rows with error (2st)
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert_rows(col_concat, is.character, x, y, z) %>%
      assert_rows(rowSums, is.character, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There is 1 error")

  # only assert_rows with two error
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert_rows(col_concat, is.numeric, x, y, z) %>%
      assert_rows(rowSums, is.character, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There are 2 errors")

  # only assert_rows with two errors and one callback overwritten in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      assert_rows(col_concat, is.numeric, x, y, z, error_fun = error_message, skip_chain_opts = TRUE) %>%
      assert_rows(rowSums, is.character, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(expect_message(code_to_test(), "Assertion failed"))
})




##### !!! chaining: insist
test_that("insist works with chaining", {

  # only insist with no error
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist(within_n_mads(5), x, y, z) %>%
      chain_end %>% strip_attributes
  }
  expect_equal(code_to_test(), test.df2)

  # only insist with no error ans storing success
  code_to_test <- function() {
    test.df2 %>%
      chain_start(store_success = TRUE) %>%
      insist(within_n_mads(5), x, y, z) %>%
      chain_end %>% strip_attributes
  }
  expect_equal(
    get_assertr_success(code_to_test()),
    success_result("insist", "within_n_mads(5)", c("x", "y", "z"), NA)
  )


  # only insist with print state on success in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist(within_n_mads(5), x, y, z, success_fun = success_message, skip_chain_opts = TRUE) %>%
      chain_end %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion passed")

  # only insist with error (3 of them, though)
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist(within_n_mads(.4), x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There are 3 errors")

  # only insist with printed state on error in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist(within_n_mads(.4), x, y, z, error_fun = error_message, skip_chain_opts = TRUE) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion failed")

  # two insists with no error
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist(within_n_mads(5), x, y, z) %>%
      insist(within_n_sds(5), x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_equal(code_to_test(), test.df2)

  # two insists with no error and storing success
  code_to_test <- function() {
    test.df2 %>%
      chain_start(store_success = TRUE) %>%
      insist(within_n_mads(5), x, y, z) %>%
      insist(within_n_sds(5), x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_equal(
    get_assertr_success(code_to_test()),
    append(
      success_result("insist", "within_n_mads(5)", c("x", "y", "z"), NA),
      success_result("insist", "within_n_sds(5)", c("x", "y", "z"), NA)
    )
  )

  # two insists with error (1st)
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist(within_n_mads(.4), x, y, z) %>%
      insist(within_n_sds(5), x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There are 3 errors")

  # two insists with error (1st) and callback overwritten in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist(within_n_mads(.4), x, y, z, error_fun = error_message, skip_chain_opts = TRUE) %>%
      insist(within_n_sds(5), x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion failed")

  # two insists with error (2st)
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist(within_n_sds(5), x, y, z) %>%
      insist(within_n_mads(.4), x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There are 3 errors")

  # two insists with two error
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist(within_n_sds(.4), x, y, z) %>%
      insist(within_n_mads(.4), x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There are 6 errors")

  # two insists with two errors and one callback overwritten in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist(within_n_sds(.4), x, y, z, error_fun = error_message, skip_chain_opts = TRUE) %>%
      insist(within_n_mads(.4), x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(expect_message(code_to_test(), "Assertion failed"))
})


##### !!! chaining: insist_rows
test_that("insist_rows works with chaining", {

  # only insist_rows with no error
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist_rows(maha_dist, function(x){function(...) TRUE}, x, y, z) %>%
      chain_end %>% strip_attributes
  }
  expect_equal(code_to_test(), test.df2)

  # only insist_rows with no error and storing success
  code_to_test <- function() {
    test.df2 %>%
      chain_start(store_success = TRUE) %>%
      insist_rows(maha_dist, function(x){function(...) TRUE}, x, y, z) %>%
      chain_end %>% strip_attributes
  }
  expect_equal(
    get_assertr_success(code_to_test()),
    success_result("insist_rows", as.character(expression(function(x){function(...) TRUE})), c("x", "y", "z"), "maha_dist")
  )

  # only insist_rows with print state on success in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist_rows(maha_dist, function(x){function(...) TRUE}, x, y, z, success_fun = success_message, skip_chain_opts = TRUE) %>%
      chain_end %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion passed")

  # only insist_rows with error (3 of them, though)
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist_rows(maha_dist, function(x){function(...) FALSE}, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There is 1 error")

  # only insist_rows with printed state on error in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist_rows(maha_dist, function(x){function(...) FALSE}, x, y, z, error_fun = error_message, skip_chain_opts = TRUE) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion failed")

  # two insists_rows with no error
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist_rows(maha_dist, function(x){function(...) TRUE}, x, y, z) %>%
      insist_rows(col_concat, function(x){function(...) TRUE}, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_equal(code_to_test(), test.df2)

  # two insists_rows with no error and storing success results
  code_to_test <- function() {
    test.df2 %>%
      chain_start(store_success = TRUE) %>%
      insist_rows(maha_dist, function(x){function(...) TRUE}, x, y, z) %>%
      insist_rows(col_concat, function(x){function(...) TRUE}, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_equal(
    get_assertr_success(code_to_test()),
    append(
      success_result("insist_rows", as.character(expression(function(x){function(...) TRUE})), c("x", "y", "z"), "maha_dist"),
      success_result("insist_rows", as.character(expression(function(x){function(...) TRUE})), c("x", "y", "z"), "col_concat")
    )
  )

  # two insists_rows with error (1st)
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist_rows(maha_dist, function(x){function(...) FALSE}, x, y, z) %>%
      insist_rows(col_concat, function(x){function(...) TRUE}, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There is 1 error")

  # two insists_rows with error (1st) and callback overwritten in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist_rows(maha_dist, function(x){function(...) FALSE}, x, y, z, error_fun = error_message, skip_chain_opts = TRUE) %>%
      insist_rows(col_concat, function(x){function(...) TRUE}, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion failed")

  # two insists with error (2st)
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist_rows(col_concat, function(x){function(...) TRUE}, x, y, z) %>%
      insist_rows(maha_dist, function(x){function(...) FALSE}, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There is 1 error")

  # two insists_rows with two errors
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist_rows(col_concat, function(x){function(...) FALSE}, x, y, z) %>%
      insist_rows(maha_dist, function(x){function(...) FALSE}, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There are 2 errors")

  # two insists_rows with two errors and one callback overwritten in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      insist_rows(col_concat, function(x){function(...) FALSE}, x, y, z, error_fun = error_message, skip_chain_opts = TRUE) %>%
      insist_rows(maha_dist, function(x){function(...) FALSE}, x, y, z) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(expect_message(code_to_test(), "Assertion failed"), "There is 1 error")
})


##### !!! chaining: verify
test_that("verify works with chaining", {

  # only verify with no error
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      verify(x >= 0) %>%
      chain_end %>% strip_attributes
  }
  expect_equal(code_to_test(), test.df2)

  # only verify with no error ans storing success results
  code_to_test <- function() {
    test.df2 %>%
      chain_start(store_success = TRUE) %>%
      verify(x >= 0) %>%
      chain_end %>% strip_attributes
  }
  expect_equal(
    get_assertr_success(code_to_test()),
    success_result("verify", "x >= 0", NA, NA)
  )

  # only verify with print state on success in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      verify(x >= 0, success_fun = success_message, skip_chain_opts = TRUE) %>%
      chain_end %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion passed")

  # only verify with error
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      verify(x > 0) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There is 1 error")

  # only verify with printed state on error in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      verify(x > 0, error_fun = error_message, skip_chain_opts = TRUE) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion failed")

  # two verify with no error
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      verify(x >= 0) %>%
      verify(y > 0) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_equal(code_to_test(), test.df2)

  # two verify with no error and storing success results
  code_to_test <- function() {
    test.df2 %>%
      chain_start(store_success = TRUE) %>%
      verify(x >= 0) %>%
      verify(y > 0) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_equal(
    get_assertr_success(code_to_test()),
    append(
      success_result("verify", "x >= 0", NA, NA),
      success_result("verify", "y > 0", NA, NA)
    )
  )

  # two verify with error (1st)
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      verify(x > 0) %>%
      verify(y > 0) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There is 1 error")

  # two verify with error (1st) and callback overwritten in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      verify(x > 0, error_fun = error_message, skip_chain_opts = TRUE) %>%
      verify(y > 0) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_message(code_to_test(), "Assertion failed")

  # two verify with error (2st)
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      verify(y > 0) %>%
      verify(x > 0) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There is 1 error")

  # two verify with two errors
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      verify(y > 2) %>%
      verify(x > 0) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(code_to_test(), "There are 2 errors")

  # two verify with two errors and one callback overwritten in chain
  code_to_test <- function() {
    test.df2 %>%
      chain_start %>%
      verify(y > 2, error_fun = error_message, skip_chain_opts = TRUE) %>%
      verify(x > 0) %>%
      chain_end(error_fun=ret_num_off_errors) %>% strip_attributes
  }
  expect_output(expect_message(code_to_test(), "Assertion failed"), "There is 1 error")
})
###################################


##### !!! rlang .data and unquoting
test_that("all assertions work with .data pronoun without chains", {
  # Define some data we might accidentally reference outside the test.df frame
  y <- 0:2

  ## verify() ##
  # Cases where the name exists:
  # Also test the logical versions here to make sure nothing too weird is happening.
  expect_equal(verify(test.df, .data$x <= 2), test.df)  # expect success
  expect_true(verify(test.df, .data$x <= 2, success_fun = success_logical))
  expect_output(verify(test.df, .data$x > 2, error_fun = just.show.error),
                "verification [.data$x > 2] failed! (3 failures)", fixed = TRUE)
  expect_false(verify(test.df, .data$x > 2, error_fun = error_logical))

  # Cases where the name doesn't exist:
  expect_error(verify(test.df, .data$y <= 2, error_fun = just.show.error))
  # expect success from y defined above
  expect_equal(verify(test.df, y <= 2), test.df)

  ## assert() ##
  expect_equal(assert(test.df, within_bounds(-Inf, 2), .data$x), test.df)
  expect_error(assert(test.df, within_bounds(2, Inf), .data$x))
  # Cases where the name doesn't exist:
  expect_error(assert(test.df, within_bounds(-Inf, 2), .data$y))
  # Note that assert(test.df, within_bounds(-Inf, 2), y) would not work because
  # assert relies on dplyr::select. Use !! varname

  ## insist() ##
  expect_equal(insist(test.df, within_n_sds(1), .data$x), test.df)
  expect_output(insist(test.df, within_n_sds(0.1), .data$x,
    error_fun = just.show.error),
    "Column 'x' violates assertion 'within_n_sds(0.1)' 2 times", fixed = TRUE)

  # Cases where the name doesn't exist:
  expect_error(insist(test.df, within_n_sds(1), .data$y))
  # Note that insist(test.df, within_n_sds(1), y) would not work because
  # insist relies on dplyr::select. Use !! y instead.
})

test_that("all assertions work with .data pronoun in chains", {
  # Define some data we might accidentally reference outside the test.df frame
  y <- 0:2

  ## verify() ##
  # Cases where the name exists:
  # Also test the logical versions here to make sure nothing too weird is happening.
  expect_equal(test.df %>% verify(.data$x <= 2), test.df)
  expect_true(test.df %>% verify(.data$x <= 2, success_fun = success_logical))
  expect_output(test.df %>% verify(.data$x > 2, error_fun = just.show.error),
                "verification [.data$x > 2] failed! (3 failures)", fixed = TRUE)
  expect_false(test.df %>% verify(.data$x > 2, error_fun = error_logical))

  # Cases where the name doesn't exist:
  expect_error(test.df %>% verify(.data$y <= 2, error_fun = just.show.error))
  expect_equal(test.df %>% verify(y <= 2), test.df)

  ## assert() ##
  expect_equal(test.df %>% assert(within_bounds(-Inf, 2), .data$x), test.df)
  expect_output(test.df %>% assert(within_bounds(2, Inf), .data$x,
    error_fun = just.show.error),
    "Column 'x' violates assertion 'within_bounds(2, Inf)' 2 times", fixed = TRUE)
  # Cases where the name doesn't exist:
  expect_error(test.df %>% assert(within_bounds(-Inf, 2), .data$y))
  # Note that test.df %>% assert(within_bounds(-Inf, 2), y) would not work because
  # assert relies on dplyr::select.

  ## insist() ##
  expect_equal(test.df %>% insist(within_n_sds(1), .data$x), test.df)
  expect_output(test.df %>% insist(within_n_sds(0.1), .data$x,
    error_fun = just.show.error),
    "Column 'x' violates assertion 'within_n_sds(0.1)' 2 times", fixed = TRUE)

  # Cases where the name doesn't exist:
  expect_error(test.df %>% insist(within_n_sds(1), .data$y))
  # Note that test.df %>% insist(within_n_sds(1), y) would not work because
  # insist relies on dplyr::select.
})



test_that("all assertions work with !! unquoting", {
  x <- 2:4
  y <- 0:2
  z <- 3
  varname <- rlang::quo(x)

  ## verify() ##
  expect_equal(verify(test.df, !! x > 1), test.df)        # 2:4 > 1
  expect_equal(verify(test.df, !! x > .data$x), test.df)  # 2:4 > .data$x
  expect_equal(verify(test.df, !! y == .data$x), test.df) # 0:2 == .data$x
  expect_equal(verify(test.df, !! varname < 3), test.df)  # x < 3

  expect_output(verify(test.df, !! x < 1, error_fun = just.show.error),
    "verification [2:4 < 1] failed! (3 failures)", fixed = TRUE)
  expect_output(verify(test.df, !! x < x, error_fun = just.show.error),
    "verification [2:4 < x] failed! (3 failures)", fixed = TRUE)
  expect_output(verify(test.df, !! y != x, error_fun = just.show.error),
    "verification [0:2 != x] failed! (3 failures)", fixed = TRUE)
  expect_output(verify(test.df, !! varname > 3, error_fun = just.show.error),
    # this is a weird error message, but it's fine I guess
    "verification [(~x) > 3] failed! (3 failures)", fixed = TRUE)

  ## assert() ##
  # Note that !!min(x) becomes min(2:4), so this works:
  expect_equal(assert(test.df, within_bounds(-Inf, !!min(x)), x), test.df)
  expect_equal(assert(test.df, within_bounds(-Inf, 2), !! varname), test.df)

  expect_output(assert(test.df, within_bounds(2, Inf), !! varname,
    error_fun = just.show.error),
    "Column 'x' violates assertion 'within_bounds(2, Inf)' 2 times",
    fixed = TRUE)
  expect_output(assert(test.df, within_bounds(!!z-1, Inf), x,
    error_fun = just.show.error),
    "Column 'x' violates assertion 'within_bounds(3 - 1, Inf)' 2 times",
    fixed = TRUE)

  ## insist() ##
  expect_equal(test.df %>% insist(within_n_sds(!! z), !! varname), test.df)
  expect_output(test.df %>% insist(within_n_sds(!! z/10), !! varname,
    error_fun = just.show.error),
    "Column 'x' violates assertion 'within_n_sds(3/10)' 2 times", fixed = TRUE)
})

test_that("verify works with variable-argument-length is_uniq", {
  # These x, y, z should not be used by the is_uniq below because the
  # predicates use a data mask
  x <- 2:4
  y <- 0:2
  z <- 3
  varname <- rlang::quo(x)

  # test.df2 <- data.frame(x = c(0, 1, 2),
  #                        y = c(2, 1.5, 1),
  #                        z = c(0,NA, -1))
  expect_equal(verify(test.df2, is_uniq(x)), test.df2)
  expect_equal(verify(test.df2, is_uniq(x, y)), test.df2)
  expect_equal(verify(test.df2, is_uniq(x, y, z, allow.na=TRUE)), test.df2)
  expect_equal(verify(test.df2, is_uniq(!!varname)), test.df2)

  df_dups <- data.frame(x = c(0, 0, 1, 2),
                             y = c(1, 2, 2, NA),
                             z = c(1, 1, 2, 3))
  expect_output(verify(df_dups, is_uniq(x, z), error_fun = just.show.error),
    "verification [is_uniq(x, z)] failed! (2 failures)", fixed = TRUE)
  expect_equal(verify(df_dups, is_uniq(x, y, allow.na = TRUE)), df_dups)
  expect_output(verify(df_dups, is_uniq(x, y), error_fun = just.show.error),
    "verification [is_uniq(x, y)] failed! (1 failure)", fixed = TRUE)



})
