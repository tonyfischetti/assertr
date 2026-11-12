# Setup
just.show.error <- function(err, ...){
  lapply(err, summary)
}

### has_all_names ###

test_that("has_all_names works with verify", {
  # code borrowed from the has_all_names examples
  expect_equal(verify(mtcars, has_all_names("mpg", "wt", "qsec")), mtcars)
  expect_equal(mtcars %>% verify(has_all_names("mpg", "wt", "qsec")), mtcars)

  mpgg <- "something"
  expect_equal(verify(mtcars, exists("mpgg")), mtcars)  # passes but big mistake

  expect_output(
    mtcars %>% verify(has_all_names("mpgg"), error_fun = just.show.error),
    "verification [has_all_names(\"mpgg\")] failed! (1 failure)",
    fixed = TRUE)

  # Same tests, but using variables to hold varnames
  # code borrowed from the has_all_names examples
  mpg_var <- "mpg"
  wt_var <- "wt"
  expect_equal(verify(mtcars, has_all_names(mpg_var, wt_var)), mtcars)
  expect_equal(mtcars %>% verify(has_all_names(mpg_var, wt_var)), mtcars)
})

test_that("has_all_names accepts character vectors of names", {
  test_data <- data.frame(A=1, B=2, C=3)

  expect_equal(verify(test_data, has_all_names(c("A", "B")), error_fun = error_logical), test_data)
  expect_equal(verify(test_data, has_all_names(c("A", "B"), "C"), error_fun = error_logical), test_data)
  wanted <- c("A", "C")
  expect_equal(verify(test_data, has_all_names(wanted), error_fun = error_logical), test_data)
  # a missing name is still noticed, wherever it is given
  expect_equal(verify(test_data, has_all_names(c("A", "Z")), error_fun = error_logical), FALSE)
  expect_equal(verify(test_data, has_all_names("A", c("B", "Z")), error_fun = error_logical), FALSE)
})

### has_only_names ###

test_that("has_only_names works with verify", {
  test_data <- data.frame(A=1, B=2)

  expect_equal(verify(test_data, has_only_names(c("B", "A"))), test_data)
  # Order doesn't matter
  expect_equal(verify(test_data, has_only_names(c("A", "B"))), test_data)
  # Multiple arguments are okay and are collated
  expect_equal(verify(test_data, has_only_names("A", "B")), test_data)
  expect_output(
    expect_error(verify(test_data, has_only_names("A"))),
    regexp='verification [has_only_names("A")] failed',
    fixed=TRUE
  )
  expect_error(
    verify(test_data, has_only_names(1)),
    regexp="Arguments to 'has_only_names()' must be character strings.",
    fixed=TRUE
  )
})

test_that("has_only_names is not misled by a name that is given twice", {
  test_data <- data.frame(A=1, B=2)

  # B is not listed, so there is a name other than the ones given
  expect_equal(verify(test_data, has_only_names("A", "A"), error_fun = error_logical), FALSE)
  # every column is listed, one of them twice
  expect_equal(verify(test_data, has_only_names("A", "B", "B"), error_fun = error_logical), test_data)
  expect_equal(verify(test_data, has_only_names(c("A", "B"), "A"), error_fun = error_logical), test_data)
  # a name that is not a column
  expect_equal(verify(test_data, has_only_names("A", "B", "Z"), error_fun = error_logical), FALSE)
})

### has_class ###

test_that("has_class works with verify", {

  expect_equal(verify(mtcars, has_class("mpg", class = "numeric")), mtcars)
  expect_equal(
    mtcars %>% verify(has_class("mpg", "wt", class = "numeric")), mtcars)

  expect_output(
    mtcars %>% verify(has_class("mpg", class = "character"), error_fun = just.show.error),
    "verification [has_class(\"mpg\", class = \"character\")] failed! (1 failure)",
    fixed = TRUE
  )

  mpg_var <- "mpg"
  wt_var <- "wt"
  expect_equal(verify(mtcars, has_class(mpg_var, class = "numeric")), mtcars)
  expect_equal(mtcars %>% verify(has_class(mpg_var, wt_var, class = "numeric")), mtcars)
})

test_that("has_class works on columns with more than one class", {
  test_data <- data.frame(when=as.POSIXct(c("2020-01-01", "2020-01-02"), tz="UTC"),
                          size=factor(c("s", "l"), levels=c("s", "l"), ordered=TRUE),
                          n=1:2)

  expect_equal(verify(test_data, has_class("when", class = "POSIXct"), error_fun = error_logical), test_data)
  expect_equal(verify(test_data, has_class("when", class = "POSIXt"), error_fun = error_logical), test_data)
  expect_equal(verify(test_data, has_class("size", class = "factor"), error_fun = error_logical), test_data)
  expect_equal(verify(test_data, has_class("size", "when", class = c("factor", "POSIXct")), error_fun = error_logical),
               test_data)
  # a class the column does not have
  expect_equal(verify(test_data, has_class("when", class = "Date"), error_fun = error_logical), FALSE)
  # the class is the one that class() reports: an integer column is not "numeric"
  expect_equal(verify(test_data, has_class("n", class = "numeric"), error_fun = error_logical), FALSE)
})

test_that("has_class accepts character vectors of column names", {
  expect_equal(verify(mtcars, has_class(c("mpg", "wt"), class = "numeric")), mtcars)
  expect_equal(verify(mtcars, has_class(c("mpg", "wt"), "qsec", class = "numeric")), mtcars)
  these <- c("mpg", "wt")
  expect_equal(verify(mtcars, has_class(these, class = "numeric")), mtcars)
  expect_equal(verify(mtcars, has_class(c("mpg", "wt"), class = "character"), error_fun = error_logical), FALSE)
})
