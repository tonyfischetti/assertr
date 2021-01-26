context("assertions about utils in utils.R")



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
