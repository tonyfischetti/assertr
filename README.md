assertr
===

[![Build Status](https://travis-ci.org/tonyfischetti/assertr.svg?branch=master)](https://travis-ci.org/tonyfischetti/assertr)

#### What is it?
The assertr package supplies a suite of functions designed to verify
assumptions about data early in an dplyr/magrittr analysis pipeline so that
data errors are spotted early and address quickly.

#### Installation

    install.packages("devtools")
    devtools::install_github("tonyfischetti/assertr")

I also plan to make this package available on CRAN, too, in the
near future.

#### What does it look like?
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
- that the am and vs columns (automatic/manual and v/straight engine,
respectively) contains 0s and 1s only

This could be written using `assertr` like this:

    mtcars %>%
      verify(nrow(mtcars) > 2) %>%
      verify(mpg > 0) %>%
      assert(in_set(0,1), am, vs) %>%
      group_by(cyl) %>%
      summarise(avg.mpg=mean(mpg))

If any of these assertions were violated, an error would have been raised
and the pipeline would have been terminated early.

#### What does `assertr` give me?

`assertr` also offers three (so far) predicate functions designed to be used

#### More info

For more info, check out the `assertr` vignette

    > vignette("assertr")

Or [read it here](http://www.onthelambda.com/wp-content/uploads/2015/01/assertr.html)
