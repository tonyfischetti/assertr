assertr
===

![assertr logo](http://statethatiamin.onlythisrose.com/assertrlogo.png)

[![Build Status](https://travis-ci.org/tonyfischetti/assertr.svg?branch=master)](https://travis-ci.org/tonyfischetti/assertr)

### What is it?
The assertr package supplies a suite of functions designed to verify
assumptions about data early in an dplyr/magrittr analysis pipeline so that
data errors are spotted early and can be addressed quickly.

### Installation

    install.packages("devtools")
    devtools::install_github("tonyfischetti/assertr")

I also plan to make this package available on CRAN, too, in the
near future.

### What does it look like?
This package offers two assertion functions, `assert` and `verify`, that
are designed to be used shortly after data-loading in a dplyr/magrittr
pipeline...

Let’s say, for example, that the R’s built-in car dataset, mtcars, was not 
built-in but rather procured from an external source that was known for making
errors in data entry or coding. Pretend we wanted to find the average
miles per gallon for each number of engine cylinders. We might want to first,
confirm
- that the dataset contains more than 10 observations
- that the column for 'miles per gallon' (mpg) is a positive number
- that the column for ‘miles per gallon’ (mpg) does not contain a datum
that is outside 4 standard deviations from its mean, and
- that the am and vs columns (automatic/manual and v/straight engine,
respectively) contain 0s and 1s only

This could be written using `assertr` like this:


    mtcars %>%
      verify(nrow(mtcars) > 10) %>%
      verify(mpg > 0) %>%
      insist(within_n_sds(4), mpg) %>%
      assert(in_set(0,1), am, vs) %>%
      group_by(cyl) %>%
      summarise(avg.mpg=mean(mpg))


If any of these assertions were violated, an error would have been raised
and the pipeline would have been terminated early.

### What does `assertr` give me?

- `verify` - takes a data frame (its first argument is provided by
the `%>%` operator above), and a logical (boolean) expression. Then, `verify`
evaluates that expression using the scope of the provided data frame. If any
of the logical values of the expression's result are `FALSE`, `verify` will
raise an error that terminates any further processing of the pipeline.
- `assert` - takes a data frame, a predicate function, and an arbitrary
number of columns to apply the predicate function to. The predicate function
(a function that returns a logical/boolean value) is then applied to every
element of the columns selected, and will raise an error when it finds the
first violation.  Internally, the `assert` function uses `dplyr`'s
`select` function to extract the columns to test the predicate function on. 

- `insist` - takes a data frame, a predicate-generating function, and an
arbitrary number of columns.

`assertr` also offers three (so far) predicate functions designed to be used
with the `assert` function:

- `not_na` - that checks if an element is not NA
- `within_bounds` - that returns a predicate function that checks if a numeric
value falls within the bounds supplied, and
- `in_set` - that returns a predicate function that checks if an element is
a member of the set supplied.

and a predicate generator designed to be used with the `insist` function:

- `within_n_sds` - used to dynamically create bounds to check vector elements with

### More info

For more info, check out the `assertr` vignette

    > vignette("assertr")

Or [read it here](http://www.onthelambda.com/wp-content/uploads/2015/03/assertr.html)
