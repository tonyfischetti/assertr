context("assertions about assertions in assertion.R")

# just some set up
a <- 1
alist <- list(a=c(1,2,3), b=c(4,5,6))

our.iris <- iris
our.iris.2 <- our.iris
our.iris.2[106,1] <- 7
our.iris.3 <- our.iris.2
our.iris.3[c(118, 119, 123, 132, 131, 136), 1] <- 7

exmpl.data <- data.frame(x=c(8, 9, 6, 5, 9, 5, 6, 7, 8, 9, 6, 5, 5, 6, 7),
                         y=c(82, 91, 61, 49, 40, 49, 57, 74, 78, 90, 61, 49, 51, 62, 68))

nexmpl.data <- exmpl.data
nexmpl.data[12,2] <- NA

mnexmpl.data <- nexmpl.data
mnexmpl.data[12,1] <- NA

nanmnexmpl.data <- mnexmpl.data
nanmnexmpl.data[10,1] <- 0/0


# custom error messages
yell <- function(message){
  stop(toupper(message), call.=FALSE)
}

not.helpful <- function(message){
  stop("unspecified error", call.=FALSE)
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
  expect_error(verify(mtcars, 0 > 1, "tree"), "could not find function \"error_fun\"")
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
  expect_equal(assert(mtcars, function(x) if(x%%1!=0) return(FALSE), gear), mtcars)
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
  expect_equal(assert_(mtcars, function(x) if(x%%1!=0) return(FALSE), "gear"), mtcars)
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

test_that("assert raises *custom error* if verification fails", {
  expect_error(assert(mtcars, within_bounds(3.5,4.5), gear, error_fun=yell),
               "VECTOR 'GEAR' VIOLATES ASSERTION 'WITHIN_BOUNDS' 20 TIMES \\(E.G. \\[3\\] AT INDEX 4\\)")
  expect_error(assert(mtcars, within_bounds(3,5), gear, carb, error_fun=yell),
               "VECTOR 'CARB' VIOLATES ASSERTION 'WITHIN_BOUNDS' 19 TIMES \\(E.G. \\[1\\] AT INDEX 3\\)")
  expect_error(assert(mtcars, within_bounds(3.5, 4.5), carb, gear, error_fun=yell),
               "VECTOR 'CARB' VIOLATES ASSERTION 'WITHIN_BOUNDS' 22 TIMES \\(E.G. \\[1\\] AT INDEX 3\\)\nVECTOR 'GEAR' VIOLATES ASSERTION 'WITHIN_BOUNDS' 20 TIMES \\(E.G. \\[3\\] AT INDEX 4\\)")
  expect_error(assert(mtcars, within_bounds(3.5,4.5), gear, error_fun=not.helpful),
               "unspecified error")
  expect_error(assert(mtcars, within_bounds(3,5), gear, carb, error_fun=not.helpful),
               "unspecified error")
})

test_that("assert raises *custom error* if verification fails (using se)", {
  expect_error(assert_(mtcars, within_bounds(3.5,4.5), "gear", error_fun=yell),
               "VECTOR 'GEAR' VIOLATES ASSERTION 'WITHIN_BOUNDS' 20 TIMES \\(E.G. \\[3\\] AT INDEX 4\\)")
  expect_error(assert_(mtcars, within_bounds(3,5), "gear", "carb", error_fun=yell),
               "VECTOR 'CARB' VIOLATES ASSERTION 'WITHIN_BOUNDS' 19 TIMES \\(E.G. \\[1\\] AT INDEX 3\\)")
  expect_error(assert_(mtcars, within_bounds(3.5, 4.5), "carb", "gear", error_fun=yell),
               "VECTOR 'CARB' VIOLATES ASSERTION 'WITHIN_BOUNDS' 22 TIMES \\(E.G. \\[1\\] AT INDEX 3\\)\nVECTOR 'GEAR' VIOLATES ASSERTION 'WITHIN_BOUNDS' 20 TIMES \\(E.G. \\[3\\] AT INDEX 4\\)")
  expect_error(assert_(mtcars, within_bounds(3.5,4.5), "gear", error_fun=not.helpful),
               "unspecified error")
  expect_error(assert_(mtcars, within_bounds(3,5), "gear", "carb", error_fun=not.helpful),
               "unspecified error")
})

test_that("assert breaks appropriately", {
  expect_error(assert(in_set(0,1), mtcars$vs),
               "no applicable method for 'select.?' applied to an object of class \"function\"")
  expect_error(assert(mtcars, in_set(0,1), vs, tree),
               "object 'tree' not found")
  expect_error(assert(mtcars, in_set(0,1), vs, "tree"),
               "All select\\(\\) inputs must resolve to integer column positions")
  expect_error(assert("tree"),
               "no applicable method for 'select.?' applied to an object of class \"character\"")
})

test_that("assert breaks appropriately (using se)", {
  expect_error(assert_(in_set(0,1), mtcars$vs),
               "no applicable method for 'select.?' applied to an object of class \"function\"")
  expect_error(assert_(mtcars, in_set(0,1), vs),
               "object 'vs' not found")
  expect_error(assert_(mtcars, in_set(0,1), "vs", "tree"),
               "object 'tree' not found")
  expect_error(assert_("tree"),
               "no applicable method for 'select.?' applied to an object of class \"character\"")
})

######################################





############### assert_rows ###############
test_that("assert_rows returns data if verification passes", {
  expect_equal(assert_rows(mtcars, rowSums, within_bounds(0,2), vs, am), mtcars)
  expect_equal(assert_rows(mtcars, num_row_NAs, within_bounds(0,.1), everything()),
               mtcars)
  expect_equal(assert_rows(mtcars, rowSums, within_bounds(5,16), cyl, carb),
               mtcars)
  expect_equal(assert_rows(mnexmpl.data, num_row_NAs, within_bounds(0,2),
                           everything()), mnexmpl.data)
  # newest version of R broke this
  # expect_equal(assert_rows(mtcars, `|`, in_set(0,1), vs, am), mtcars)
  expect_equal(assert_rows(nexmpl.data, num_row_NAs, function(x) x < 2,
                           everything()), nexmpl.data)
  expect_equal(assert_rows(mtcars, rowSums, function(x) if(x>16) return(FALSE), carb, cyl),
               mtcars)
})

test_that("assert_rows returns data if verification passes (using se)", {
  expect_equal(assert_rows_(mtcars, rowSums, within_bounds(0,2), "vs", "am"), mtcars)
  expect_equal(assert_rows_(mtcars, rowSums, within_bounds(0,2), "vs:am"), mtcars)
  expect_equal(assert_rows_(mtcars, rowSums, within_bounds(5,16), "cyl", "carb"),
               mtcars)
  # newest version of R broke this
  # expect_equal(assert_rows_(mtcars, `|`, in_set(0,1), "vs", "am"), mtcars)
  expect_equal(assert_rows_(mtcars, rowSums, function(x) if(x>16) return(FALSE), "carb", "cyl"),
               mtcars)
})

test_that("assert_rows raises error if verification fails", {
  expect_error(assert_rows(mtcars, rowSums, within_bounds(1,2), vs, am),
               "Data frame row reduction violates predicate 'within_bounds' 12 times \\(e.g. at row number 5\\)")
  expect_error(assert_rows(mtcars, num_row_NAs, within_bounds(1,2), everything()),
               "Data frame row reduction violates predicate 'within_bounds' 32 times \\(e.g. at row number 1\\)")
  expect_error(assert_rows(mtcars, rowSums, function(x) if(x==10) return(FALSE), carb, cyl),
               "Data frame row reduction violates predicate 'function' 8 times \\(e.g. at row number 1\\)")
  expect_error(assert_rows(mnexmpl.data, num_row_NAs, within_bounds(0,1), everything()),
               "Data frame row reduction violates predicate 'within_bounds' 1 time \\(at row number 12\\)")
})

test_that("assert_rows raises error if verification fails (using se)", {
  expect_error(assert_rows_(mtcars, rowSums, within_bounds(1,2), "vs", "am"),
               "Data frame row reduction violates predicate 'within_bounds' 12 times \\(e.g. at row number 5\\)")
  expect_error(assert_rows_(mtcars, rowSums, function(x) if(x==10) return(FALSE), "carb", "cyl"),
               "Data frame row reduction violates predicate 'function' 8 times \\(e.g. at row number 1\\)")
  expect_error(assert_rows_(mnexmpl.data, num_row_NAs, within_bounds(0,1), "x", "y"),
               "Data frame row reduction violates predicate 'within_bounds' 1 time \\(at row number 12\\)")
})

test_that("assert_rows raises *custom error* if verification fails", {
  expect_error(assert_rows(mtcars, rowSums, within_bounds(1,2), vs, am, error_fun=yell),
               "DATA FRAME ROW REDUCTION VIOLATES PREDICATE 'WITHIN_BOUNDS' 12 TIMES \\(E.G. AT ROW NUMBER 5\\)")
  expect_error(assert_rows(mtcars, num_row_NAs, within_bounds(1,2), everything(), error_fun=yell),
               "DATA FRAME ROW REDUCTION VIOLATES PREDICATE 'WITHIN_BOUNDS' 32 TIMES \\(E.G. AT ROW NUMBER 1\\)")
  expect_error(assert_rows(mtcars, rowSums, function(x) if(x==10) return(FALSE), carb, cyl, error_fun=yell),
               "DATA FRAME ROW REDUCTION VIOLATES PREDICATE 'FUNCTION' 8 TIMES \\(E.G. AT ROW NUMBER 1\\)")
  expect_error(assert_rows(mnexmpl.data, num_row_NAs, within_bounds(0,1), everything(), error_fun=not.helpful),
               "unspecified error")
})

test_that("assert_rows raises *custom error* if verification fails (using se)", {
  expect_error(assert_rows_(mtcars, rowSums, within_bounds(1,2), "vs", "am", error_fun=yell),
               "DATA FRAME ROW REDUCTION VIOLATES PREDICATE 'WITHIN_BOUNDS' 12 TIMES \\(E.G. AT ROW NUMBER 5\\)")
  expect_error(assert_rows_(mtcars, rowSums, function(x) if(x==10) return(FALSE), "carb", "cyl", error_fun=yell),
               "DATA FRAME ROW REDUCTION VIOLATES PREDICATE 'FUNCTION' 8 TIMES \\(E.G. AT ROW NUMBER 1\\)")
  expect_error(assert_rows_(mnexmpl.data, num_row_NAs, within_bounds(0,1), "x", "y", error_fun=not.helpful),
               "unspecified error")
})

test_that("assert_rows breaks appropriately", {
  expect_error(assert_rows(in_set(0,1), mtcars$vs),
               "no applicable method for 'select.?' applied to an object of class \"function\"")
  expect_error(assert_rows(rowSums, in_set(0,1), mtcars$vs),
               "no applicable method for 'select.?' applied to an object of class \"function\"")
  expect_error(assert_rows(mtcars, rowSums, in_set(0,1,2), vs, am, tree),
               "object 'tree' not found")
  expect_error(assert_rows(mtcars, rowSums, in_set(0,1,2), vs, am, "tree"),
               "All select\\(\\) inputs must resolve to integer column positions")
  expect_error(assert_rows("tree"),
               "no applicable method for 'select.?' applied to an object of class \"character\"")
})

test_that("assert_rows breaks appropriately (using se)", {
  expect_error(assert_rows_(in_set(0,1), "mtcars$vs"),
               "no applicable method for 'select.?' applied to an object of class \"function\"")
  expect_error(assert_rows_(rowSums, in_set(0,1), "mtcars$vs"),
               "no applicable method for 'select.?' applied to an object of class \"function\"")
  expect_error(assert_rows_(mtcars, rowSums, in_set(0,1,2), vs, am, tree),
               "object 'vs' not found")
  expect_error(assert_rows_(mtcars, rowSums, in_set(0,1,2), vs, am, "tree"),
               "object 'vs' not found")
  expect_error(assert_rows_("tree"),
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

test_that("insist raises error if verification fails (using se)", {
  expect_error(insist_(our.iris, within_n_sds(2), "Sepal.Length"),
               "Vector 'Sepal.Length' violates assertion 'within_n_sds' 6 times \\(e.g. \\[7.6\\] at index 106\\)")
  expect_error(insist_(our.iris.2, within_n_sds(2), "Sepal.Length"),
               "Vector 'Sepal.Length' violates assertion 'within_n_sds' 5 times \\(e.g. \\[7.7\\] at index 118\\)")
  expect_error(insist_(our.iris, within_n_sds(3), "Sepal.Length:Petal.Width"),
               "Vector 'Sepal.Width' violates assertion 'within_n_sds' 1 time \\(value \\[4.4\\] at index 16\\)")
  expect_error(insist_(our.iris, within_n_sds(2), "Sepal.Length:Petal.Width"),
               "Vector 'Sepal.Length' violates assertion 'within_n_sds' 6 times \\(e.g. \\[7.6\\] at index 106\\)\nVector 'Sepal.Width' violates assertion 'within_n_sds' 5 times \\(e.g. \\[4\\] at index 15\\)")
})

test_that("insist raises *custom error* if verification fails", {
  expect_error(insist(our.iris, within_n_sds(2), Sepal.Length, error_fun=yell),
               toupper("Vector 'Sepal.Length' violates assertion 'within_n_sds' 6 times \\(e.g. \\[7.6\\] at index 106\\)"))
  expect_error(insist(our.iris.2, within_n_sds(2), Sepal.Length, error_fun=yell),
               toupper("Vector 'Sepal.Length' violates assertion 'within_n_sds' 5 times \\(e.g. \\[7.7\\] at index 118\\)"))
  expect_error(insist(our.iris, within_n_sds(3), Sepal.Length:Petal.Width, error_fun=yell),
               toupper("Vector 'Sepal.Width' violates assertion 'within_n_sds' 1 time \\(value \\[4.4\\] at index 16\\)"))
  expect_error(insist(our.iris, within_n_sds(2), Sepal.Length:Petal.Width, error_fun=yell),
               toupper("Vector 'Sepal.Length' violates assertion 'within_n_sds' 6 times \\(e.g. \\[7.6\\] at index 106\\)\nVector 'Sepal.Width' violates assertion 'within_n_sds' 5 times \\(e.g. \\[4\\] at index 15\\)"))
  expect_error(insist(our.iris, within_n_sds(2), Sepal.Length, error_fun=not.helpful),
               "unspecified error")
  expect_error(insist(our.iris.2, within_n_sds(2), Sepal.Length, error_fun=not.helpful),
               "unspecified error")
})

test_that("insist raises *custom error* if verification fails (using se)", {
  expect_error(insist_(our.iris, within_n_sds(2), "Sepal.Length", error_fun=yell),
               toupper("Vector 'Sepal.Length' violates assertion 'within_n_sds' 6 times \\(e.g. \\[7.6\\] at index 106\\)"))
  expect_error(insist_(our.iris.2, within_n_sds(2), "Sepal.Length", error_fun=yell),
               toupper("Vector 'Sepal.Length' violates assertion 'within_n_sds' 5 times \\(e.g. \\[7.7\\] at index 118\\)"))
  expect_error(insist_(our.iris, within_n_sds(3), "Sepal.Length:Petal.Width", error_fun=yell),
               toupper("Vector 'Sepal.Width' violates assertion 'within_n_sds' 1 time \\(value \\[4.4\\] at index 16\\)"))
  expect_error(insist_(our.iris, within_n_sds(2), "Sepal.Length:Petal.Width", error_fun=yell),
               toupper("Vector 'Sepal.Length' violates assertion 'within_n_sds' 6 times \\(e.g. \\[7.6\\] at index 106\\)\nVector 'Sepal.Width' violates assertion 'within_n_sds' 5 times \\(e.g. \\[4\\] at index 15\\)"))
  expect_error(insist_(our.iris, within_n_sds(2), "Sepal.Length", error_fun=not.helpful),
               "unspecified error")
  expect_error(insist_(our.iris.2, within_n_sds(2), "Sepal.Length", error_fun=not.helpful),
               "unspecified error")
})

test_that("insist breaks appropriately", {
  expect_error(insist(within_n_sds(5), mtcars$vs),
               "no applicable method for 'select.?' applied to an object of class \"function\"")
  expect_error(insist(mtcars, within_n_sds(5), "vs"),
               "All select\\(\\) inputs must resolve to integer column positions")
  expect_error(insist(mtcars, within_n_sds(5), tree),
               "object 'tree' not found")
  expect_error(insist("tree"),
               "no applicable method for 'select.?' applied to an object of class \"character\"")
  expect_error(insist(iris, within_n_sds(5), Petal.Width:Species),
               "argument must be a numeric vector")
})

test_that("insist breaks appropriately (using se)", {
  expect_error(insist_(within_n_sds(5), "mtcars$vs"),
               "no applicable method for 'select.?' applied to an object of class \"function\"")
  expect_error(insist_(mtcars, within_n_sds(5), tree),
               "object 'tree' not found")
  expect_error(insist_("tree"),
               "no applicable method for 'select.?' applied to an object of class \"character\"")
  expect_error(insist_(iris, within_n_sds(5), "Petal.Width:Species"),
               "argument must be a numeric vector")
})
######################################


############### insist rows ###############
test_that("insist_rows returns data if verification passes", {
  expect_equal(insist_rows(our.iris, maha_dist, within_n_sds(6), everything()), our.iris)
  expect_equal(insist_rows(our.iris, maha_dist, within_n_mads(10), Sepal.Length:Species), our.iris)
  expect_equal(insist_rows(our.iris, maha_dist, within_n_mads(11), Sepal.Length:Petal.Width),
               our.iris)
})

test_that("insist_rows returns data if verification passes (using se)", {
  expect_equal(insist_rows_(our.iris, maha_dist, within_n_sds(6), "Sepal.Length:Species"), our.iris)
  expect_equal(insist_rows_(our.iris, maha_dist, within_n_mads(10), "Sepal.Length:Species"), our.iris)
  expect_equal(insist_rows_(our.iris, maha_dist, within_n_mads(11), "Sepal.Length:Petal.Width"),
               our.iris)
})

test_that("insist_rows raises error if verification fails", {
  expect_error(insist_rows(our.iris, maha_dist, within_n_sds(4), everything()),
               "Data frame row reduction violates predicate 'within_n_sds' 1 time \\(at row number 135\\)")
  expect_error(insist_rows(our.iris, maha_dist, within_n_sds(2), everything()),
               "Data frame row reduction violates predicate 'within_n_sds' 8 times \\(e.g. at row number 42\\)")
  expect_error(insist_rows(our.iris, maha_dist, within_n_mads(5), Sepal.Length:Species),
               "Data frame row reduction violates predicate 'within_n_mads' 1 time \\(at row number 135\\)")
  expect_error(insist_rows(our.iris, maha_dist, within_n_mads(5), Sepal.Length:Petal.Width),
               "Data frame row reduction violates predicate 'within_n_mads' 4 times \\(e.g. at row number 118\\)")
})

test_that("insist_rows raises error if verification fails (using se)", {
  expect_error(insist_rows_(our.iris, maha_dist, within_n_sds(4), "Sepal.Length:Species"),
               "Data frame row reduction violates predicate 'within_n_sds' 1 time \\(at row number 135\\)")
  expect_error(insist_rows_(our.iris, maha_dist, within_n_sds(2), "Sepal.Length:Species"),
               "Data frame row reduction violates predicate 'within_n_sds' 8 times \\(e.g. at row number 42\\)")
  expect_error(insist_rows_(our.iris, maha_dist, within_n_mads(5), "Sepal.Length:Species"),
               "Data frame row reduction violates predicate 'within_n_mads' 1 time \\(at row number 135\\)")
  expect_error(insist_rows_(our.iris, maha_dist, within_n_mads(5), "Sepal.Length:Petal.Width"),
               "Data frame row reduction violates predicate 'within_n_mads' 4 times \\(e.g. at row number 118\\)")
})


test_that("insist_rows raises *custom error* if verification fails", {
  expect_error(insist_rows(our.iris, maha_dist, within_n_sds(4), everything(), error_fun = yell),
               toupper("Data frame row reduction violates predicate 'within_n_sds' 1 time \\(at row number 135\\)"))
  expect_error(insist_rows(our.iris, maha_dist, within_n_sds(2), everything(), error_fun = yell),
               toupper("Data frame row reduction violates predicate 'within_n_sds' 8 times \\(e.g. at row number 42\\)"))
  expect_error(insist_rows(our.iris, maha_dist, within_n_mads(5), Sepal.Length:Species, error_fun = yell),
               toupper("Data frame row reduction violates predicate 'within_n_mads' 1 time \\(at row number 135\\)"))
  expect_error(insist_rows(our.iris, maha_dist, within_n_mads(5), Sepal.Length:Petal.Width, error_fun = not.helpful),
               "unspecified error")
})

test_that("insist_rows raises *custom error* if verification fails (using se)", {
  expect_error(insist_rows_(our.iris, maha_dist, within_n_sds(4), "Sepal.Length:Species", error_fun = yell),
               toupper("Data frame row reduction violates predicate 'within_n_sds' 1 time \\(at row number 135\\)"))
  expect_error(insist_rows_(our.iris, maha_dist, within_n_sds(2), "Sepal.Length:Species", error_fun = yell),
               toupper("Data frame row reduction violates predicate 'within_n_sds' 8 times \\(e.g. at row number 42\\)"))
  expect_error(insist_rows_(our.iris, maha_dist, within_n_mads(5), "Sepal.Length:Species", error_fun = yell),
               toupper("Data frame row reduction violates predicate 'within_n_mads' 1 time \\(at row number 135\\)"))
  expect_error(insist_rows_(our.iris, maha_dist, within_n_mads(5), "Sepal.Length:Petal.Width", error_fun = not.helpful),
               "unspecified")
})



test_that("insist_rows breaks appropriately", {
  expect_error(insist_rows(within_n_sds(5), mtcars$vs),
               "no applicable method for 'select.?' applied to an object of class \"function\"")
  expect_error(insist_rows(mtcars, within_n_sds(10), vs),
               "argument must be a numeric vector")
  expect_error(insist_rows(mtcars, maha_dist, within_n_sds(10), vs),
               "\"data\" needs to have at least two columns")
  expect_error(insist_rows(mtcars, maha_dist, within_bound(0, 10), vs, am),
               "could not find function \"within_bound\"")
  expect_error(insist_rows(), "argument \"data\" is missing, with no default")
  expect_error(insist_rows(mtcars), "argument \"row_reduction_fn\" is missing, with no default")
  expect_error(insist_rows(mtcars, maha_dist, am, vs),
               "\"data\" needs to have at least two columns")
  expect_error(insist_rows(mtcars, maha_dist, am, vs, carb),
               "object 'am' not found")

  expect_error(insist_rows(lm(Petal.Length ~ Petal.Width, data=iris)),
               "no applicable method for 'select.?' applied to an object of class \"lm\"")
})

test_that("insist_rows breaks appropriately (using se)", {
  expect_error(insist_rows_(within_n_sds(5), "mtcars$vs"),
               "no applicable method for 'select.?' applied to an object of class \"function\"")
  expect_error(insist_rows_(mtcars, within_n_sds(10), "vs"),
               "argument must be a numeric vector")
  expect_error(insist_rows_(mtcars, maha_dist, within_n_sds(10), "vs"),
               "\"data\" needs to have at least two columns")
  expect_error(insist_rows_(mtcars, maha_dist, within_bound(0, 10), "vs", "am"),
               "could not find function \"within_bound\"")
  expect_error(insist_rows_(), "argument \"data\" is missing, with no default")
  expect_error(insist_rows_(mtcars), "argument \"row_reduction_fn\" is missing, with no default")
  expect_error(insist_rows_(mtcars, maha_dist, "am", "vs"),
               "\"data\" needs to have at least two columns")
  expect_error(insist_rows_(mtcars, maha_dist, "am", "vs", "carb"),
               "could not find function \"predicate_generator\"")

  expect_error(insist_rows_(lm(Petal.Length ~ Petal.Width, data=iris)),
               "no applicable method for 'select.?' applied to an object of class \"lm\"")
})
###########################################


