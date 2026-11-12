set.seed(1)
exmpl.data <- data.frame(x=c(8, 9, 6, 5, 9, 5, 6, 7, 8, 9, 6, 5, 5, 6, 7),
                y=c(82, 91, 61, 49, 40, 49, 57, 74, 78, 90, 61, 49, 51, 62, 68))

nexmpl.data <- exmpl.data
nexmpl.data[12,2] <- NA

mnexmpl.data <- nexmpl.data
mnexmpl.data[12,1] <- NA

nanmnexmpl.data <- mnexmpl.data
nanmnexmpl.data[10,1] <- 0/0


manswers.mtcars <- c(8.95, 8.29, 8.94, 6.10, 5.43, 8.88, 9.14, 10.03, 22.59,
                     12.39, 11.06, 9.48, 5.59, 6.03, 11.20, 8.67, 12.26,
                     9.08, 14.95, 10.30, 13.43, 6.23, 5.79, 11.68, 6.72,
                     3.65, 18.36, 14.00, 21.57, 11.15, 19.19, 9.89)

manswers.iris <- c(2.28, 2.88, 2.13, 2.46, 2.58, 3.96, 2.89, 1.88, 3.39,
                   2.52, 3.50, 2.77, 2.79, 3.83, 9.84, 9.75, 5.78, 2.33,
                   4.52, 3.44, 2.68, 2.99, 3.93, 2.91, 5.41, 2.45, 1.98,
                   2.30, 2.68, 2.47, 1.99, 4.61, 8.88, 7.74, 1.99, 3.65,
                   5.82, 3.79, 3.16, 1.96, 2.57, 11.53, 3.32, 4.73, 4.91,
                   3.00, 4.48, 2.36, 3.18, 2.01, 5.33, 2.36, 5.27, 4.35,
                   4.13, 4.74, 4.68, 4.50, 3.40, 4.41, 7.72, 2.11, 7.61,
                   3.68, 1.11, 3.42, 6.60, 3.34, 9.97, 2.19, 10.77, 1.57,
                   6.08, 5.34, 1.97, 3.08, 5.69, 6.68, 2.82, 2.57, 2.82,
                   3.24, 0.92, 8.94, 9.72, 5.92, 3.74, 7.31, 2.28, 2.68,
                   5.85, 2.96, 1.50, 4.79, 2.38, 2.99, 1.92, 1.15, 5.17,
                   1.22, 10.22, 4.28, 2.66, 4.98, 2.35, 6.06, 13.52, 8.53,
                   4.57, 7.86, 3.99, 2.78, 2.96, 5.53, 11.47, 5.92, 3.99,
                   12.86, 8.03, 10.10, 4.04, 6.34, 8.83, 5.39, 3.16, 7.55,
                   5.32, 4.55, 1.81, 11.19, 6.56, 13.71, 2.55, 8.90, 17.55,
                   9.66, 8.37, 5.18, 5.09, 4.31, 6.04, 12.88, 4.28, 3.16,
                   7.83, 9.25, 6.20, 3.14, 7.68, 5.83)

manswers.exmpl <- c(1.28, 3.11, 0.25, 1.36, 12.82, 1.36, 0.26, 0.48, 0.88,
                    2.96, 0.25, 1.36, 1.29, 0.28, 0.06)

manswers.nexmpl <- c(1.17, 3.01, 0.23, 1.45, 12.04, 1.45, 0.31, 0.35, 0.83,
                     2.87, 0.23, NA, 1.34, 0.24, 0.04)

manswers.nexmpl.no.na <- manswers.nexmpl
manswers.nexmpl.no.na[12] <- 2.03

manswers.mnexmpl <- c(1.13, 2.91, 0.33, 1.62, 11.84, 1.62, 0.37, 0.40, 0.75,
                      2.76, 0.33, NA, 1.54, 0.36, 0.03)

manswers.mnexmpl.no.na <- manswers.mnexmpl
manswers.mnexmpl.no.na[12] <- 0


############### maha_dist ###############
test_that("regular (non-robust) one works correctly", {
  expect_equal(round(maha_dist(mtcars), 2), manswers.mtcars)
  expect_equal(round(maha_dist(iris), 2), manswers.iris)
  expect_equal(round(maha_dist(exmpl.data), 2), manswers.exmpl)
})

# robust estimation of covariance matrix is stochastic and hard to test
test_that("robust one works correctly", {
  expect_equal(which.max(maha_dist(exmpl.data, robust=TRUE)), 5)
})

test_that("regular one works correctly with NAs", {
  expect_equal(round(maha_dist(mtcars, keep.NA=FALSE), 2), manswers.mtcars)
  expect_equal(round(maha_dist(nexmpl.data), 2), manswers.nexmpl)
  expect_equal(round(maha_dist(nexmpl.data, keep.NA=FALSE), 2),
               manswers.nexmpl.no.na)
  expect_equal(round(maha_dist(mnexmpl.data), 2), manswers.mnexmpl)
  expect_equal(round(maha_dist(mnexmpl.data, keep.NA=FALSE), 2),
               manswers.mnexmpl.no.na)
})

test_that("maha_dist breaks like it is supposed to", {
  expect_error(maha_dist(), "argument \"data\" is missing, with no default")
  expect_error(maha_dist(lm(mpg ~ am, data=mtcars)),
               "\"data\" must be a data.frame \\(or matrix\\)")
  expect_error(maha_dist("William, it was really nothing"),
               "\"data\" must be a data.frame \\(or matrix\\)")
  expect_error(maha_dist(exmpl.data[,1, drop=FALSE]),
               "\"data\" needs to have at least two columns")
  expect_error(maha_dist(nexmpl.data, robust=TRUE),
               "cannot use robust maha_dist with missing values")
})
######################################



############### num_row_NAs ###############
# test NaN
test_that("num_row_NAs works correctly", {
  expect_equal(num_row_NAs(iris), rep(0, 150))
  expect_equal(num_row_NAs(exmpl.data), rep(0, 15))
  expect_equal(num_row_NAs(nexmpl.data), c(rep(0, 11), 1, rep(0,3)))
  expect_equal(num_row_NAs(mnexmpl.data), c(rep(0, 11), 2, rep(0,3)))
  expect_equal(num_row_NAs(nanmnexmpl.data), c(rep(0, 11), 2, rep(0,3)))
  expect_equal(num_row_NAs(nanmnexmpl.data, allow.NaN=TRUE),
               c(rep(0, 9), 1, 0, 2, rep(0,3)))
})

test_that("num_row_NAs breaks correctly", {
  expect_error(num_row_NAs(), "argument \"data\" is missing, with no default")
  expect_error(num_row_NAs(exmpl.data[1,1]),
               "\"data\" must be a data.frame \\(or matrix\\)")

})
###########################################

this <- c("882")
names(this)[1] <- "1"

unname <- function(this){
  names(this) <- NULL
  this
}

############### col_concat ###############
# test NaN
test_that("col_concat works correctly", {

  expect_equal(unname(col_concat(exmpl.data[1,])), "8, 82")
  expect_equal(unname(col_concat(head(exmpl.data))), c("8, 82", "9, 91", "6, 61", "5, 49", "9, 40", "5, 49"))
  expect_equal(unname(col_concat(head(exmpl.data), sep="<>")), c("8<>82", "9<>91", "6<>61", "5<>49", "9<>40", "5<>49"))
  expect_equal(unname(col_concat(head(exmpl.data), sep="")), c("882", "991", "661", "549", "940", "549"))
  expect_equal(unname(col_concat(tail(nexmpl.data))), c("9, 90", "6, 61", "5, NA", "5, 51", "6, 62", "7, 68"))
  expect_equal(unname(col_concat(head(iris, n=2))), c("5.1, 3.5, 1.4, 0.2, setosa", "4.9, 3, 1.4, 0.2, setosa"))
})

test_that("col_concat separates the columns by default, so rows that differ stay different", {
  key.data <- data.frame(id=c(1, 11, 2), sub=c(11, 1, 5))
  expect_equal(unname(col_concat(key.data)), c("1, 11", "11, 1", "2, 5"))
  expect_equal(assert_rows(key.data, col_concat, is_uniq, id, sub, error_fun=error_logical), key.data)
  # real joint duplicates are still found
  key.data[2,] <- key.data[1,]
  expect_equal(assert_rows(key.data, col_concat, is_uniq, id, sub, error_fun=error_logical), FALSE)
})

test_that("col_concat does not round or pad numbers next to non-numeric columns", {
  close.data <- data.frame(x=c(0.12345678, 0.12345679), g=c("a", "a"), stringsAsFactors=FALSE)
  expect_equal(unname(col_concat(close.data)), c("0.12345678, a", "0.12345679, a"))
  wide.data <- data.frame(n=c(1, 10, 100), g=c("a", "b", "c"), stringsAsFactors=FALSE)
  expect_equal(unname(col_concat(wide.data)), c("1, a", "10, b", "100, c"))
  # a row gives the same string whatever other rows come with it
  expect_equal(unname(col_concat(head(iris, n=2)))[2], unname(col_concat(iris[2,])))
  # so rows that differ are not reported as duplicates
  expect_equal(assert_rows(close.data, col_concat, is_uniq, x, g, error_fun=error_logical), close.data)
})

test_that("col_concat breaks correctly", {
  expect_error(col_concat(), "argument \"data\" is missing, with no default")
  expect_error(col_concat(exmpl.data[1,1]),
               "\"data\" must be a data.frame \\(or matrix\\)")
})
###########################################



############### duplicates_across_cols ###############
test_that("duplicates_across_cols works correctly", {
  dup.data <- data.frame(v1=c(1, 1, 2, 3), v2=c(4, 5, 5, 6))
  expect_equal(duplicates_across_cols(dup.data), c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(duplicates_across_cols(dup.data[, "v1", drop=FALSE]), c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(duplicates_across_cols(as.matrix(dup.data)), c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(duplicates_across_cols(tibble::as_tibble(dup.data)), c(TRUE, TRUE, TRUE, FALSE))
  # every row that shares a value is flagged, not only the repeats
  expect_equal(duplicates_across_cols(data.frame(a=c(7, 7, 7, 8), b=c(1, 2, 3, 4))), c(TRUE, TRUE, TRUE, FALSE))
  # a value that appears in two different columns is not a duplicate
  expect_equal(duplicates_across_cols(data.frame(a=c(1, 2), b=c(2, 1))), c(FALSE, FALSE))
  # no duplicates in any column
  expect_equal(duplicates_across_cols(exmpl.data[1:3,]), c(FALSE, FALSE, FALSE))
  # character and factor columns
  key.data <- data.frame(id=1:5, sub_id=letters[1:5], age=c(10, 20, 20, 15, 30), stringsAsFactors=FALSE)
  expect_equal(duplicates_across_cols(key.data), c(FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(duplicates_across_cols(key.data[, c("id", "sub_id")]), rep(FALSE, 5))
  expect_equal(duplicates_across_cols(data.frame(f=factor(c("u", "u", "v")), n=c(1, 2, 3))), c(TRUE, TRUE, FALSE))
  # row names are not carried over to the result
  expect_equal(duplicates_across_cols(mtcars[1:3, c("mpg", "hp")]), c(TRUE, TRUE, FALSE))
})

test_that("duplicates_across_cols works correctly with NAs", {
  na.data <- data.frame(a=c(1, NA, 3), b=c("x", "y", "z"), stringsAsFactors=FALSE)
  # by default a row with an NA cannot be called free of duplicates
  expect_equal(duplicates_across_cols(na.data), c(FALSE, NA, FALSE))
  expect_equal(duplicates_across_cols(na.data, allow.na=TRUE), c(FALSE, FALSE, FALSE))
  # a duplicate in another column still flags the row
  na.data$b <- c("x", "x", "z")
  expect_equal(duplicates_across_cols(na.data), c(TRUE, TRUE, FALSE))
  # two NAs in a column are duplicates of each other when NAs are allowed
  expect_equal(duplicates_across_cols(data.frame(a=c(NA, NA, 3), b=c(4, 5, 6)), allow.na=TRUE), c(TRUE, TRUE, FALSE))
  expect_equal(duplicates_across_cols(data.frame(a=c(NA, NA, 3), b=c(4, 5, 6))), c(NA, NA, FALSE))
})

test_that("duplicates_across_cols works inside assert_rows", {
  key.data <- data.frame(id=1:5, sub_id=letters[1:5], age=c(10, 20, 20, 15, 30), stringsAsFactors=FALSE)
  expect_equal(assert_rows(key.data, duplicates_across_cols, in_set(FALSE), id, sub_id), key.data)
  expect_equal(assert_rows(key.data, duplicates_across_cols, in_set(FALSE), id, sub_id, age, error_fun=error_logical), FALSE)
})

test_that("duplicates_across_cols checks each column as it is", {
  close.data <- data.frame(x=c(0.12345678, 0.12345679), g=c("a", "b"), stringsAsFactors=FALSE)
  # numbers next to a non-numeric column are not rounded before they are compared
  expect_equal(duplicates_across_cols(close.data), c(FALSE, FALSE))
  expect_equal(duplicates_across_cols(data.frame(n=c(1, 1.0000001), g=c("a", "b"))), c(FALSE, FALSE))
  expect_equal(assert_rows(close.data, duplicates_across_cols, in_set(FALSE), x, g, error_fun=error_logical), close.data)
  # a single row, or no rows, is not an error
  expect_equal(duplicates_across_cols(exmpl.data[1,]), FALSE)
  expect_equal(duplicates_across_cols(exmpl.data[0,]), logical(0))
  expect_equal(assert_rows(exmpl.data[1,], duplicates_across_cols, in_set(FALSE), x, y), exmpl.data[1,])
  expect_equal(assert_rows(exmpl.data[0,], duplicates_across_cols, in_set(FALSE), x, y), exmpl.data[0,])
})

test_that("duplicates_across_cols breaks correctly", {
  expect_error(duplicates_across_cols(), "argument \"data\" is missing, with no default")
  expect_error(duplicates_across_cols(exmpl.data[1,1]),
               "\"data\" must be a data.frame \\(or matrix\\)")
})
###########################################
