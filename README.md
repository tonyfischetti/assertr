assertr
===

![assertr logo](http://statethatiamin.onlythisrose.com/assertrlogo.png)

[![Build Status](https://travis-ci.org/ropensci/assertr.svg?branch=master)](https://travis-ci.org/ropensci/assertr)
[![](http://www.r-pkg.org/badges/version/assertr)](https://cran.r-project.org/package=assertr)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/assertr)](https://cran.r-project.org/package=assertr)

### What is it?
The assertr package supplies a suite of functions designed to verify
assumptions about data early in an analysis pipeline so that
data errors are spotted early and can be addressed quickly.

This package does not need to be used with the magrittr/dplyr piping
mechanism but the examples in this README use them for clarity.

### Installation

You can install the latest version on CRAN like this
  
    install.packages("assertr")

or you can install the bleeding-edge development version like this:
```{r}
    install.packages("devtools")
    devtools::install_github("ropensci/assertr")
```
### What does it look like?
This package offers five assertion functions, `assert`, `verify`,
`insist`, `assert_rows`, and `insist_rows`, that are designed to be used
shortly after data-loading in an analysis pipeline...

Let’s say, for example, that the R’s built-in car dataset, `mtcars`, was not 
built-in but rather procured from an external source that was known for making
errors in data entry or coding. Pretend we wanted to find the average
miles per gallon for each number of engine cylinders. We might want to first,
confirm
- that it has the columns "mpg", "vs", and "am"
- that the dataset contains more than 10 observations
- that the column for 'miles per gallon' (mpg) is a positive number
- that the column for ‘miles per gallon’ (mpg) does not contain a datum
that is outside 4 standard deviations from its mean, and
- that the am and vs columns (automatic/manual and v/straight engine,
respectively) contain 0s and 1s only
- each row contains at most 2 NAs
- each row's mahalanobis distance is within 10 median absolute deviations of
all the distances (for outlier detection)


This could be written (in order) using `assertr` like this:

```{r}
    mtcars %>%
      verify(has_all_names("mpg", "vs", "am")) %>%
      verify(nrow(.) > 10) %>%
      verify(mpg > 0) %>%
      insist(within_n_sds(4), mpg) %>%
      assert(in_set(0,1), am, vs) %>%
      assert_rows(num_row_NAs, within_bounds(0,2), everything()) %>%
      insist_rows(maha_dist, within_n_mads(10), everything()) %>%
      group_by(cyl) %>%
      summarise(avg.mpg=mean(mpg))
```

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
element of the columns selected, and will raise an error if it finds any
violations. Internally, the `assert` function uses `dplyr`'s
`select` function to extract the columns to test the predicate function on.

- `insist` - takes a data frame, a predicate-generating function, and an
arbitrary number of columns. For each column, the the predicate-generating
function is applied, returning a predicate. The predicate is then applied to
every element of the columns selected, and will raise an error if it finds any
violations. The reason for using a predicate-generating function to return a
predicate to use against each value in each of the selected rows is so
that, for example, bounds can be dynamically generated based on what the data
look like; this the only way to, say, create bounds that check if each datum is
within x z-scores, since the standard deviation isn't known a priori.
Internally, the `insist` function uses `dplyr`'s `select` function to extract
the columns to test the predicate function on.

- `assert_rows` - takes a data frame, a row reduction function, a predicate
function, and an arbitrary number of columns to apply the predicate function
to. The row reduction function is applied to the data frame, and returns a value
for each row. The predicate function is then applied to every element of vector
returned from the row reduction function, and will raise an error if it finds
any violations. This functionality is useful, for example, in conjunction with
the `num_row_NAs()` function to ensure that there is below a certain number of
missing values in each row. Internally, the `assert_rows` function uses
`dplyr`'s`select` function to extract the columns to test the predicate
function on.

- `insist_rows` - takes a data frame, a row reduction function, a
predicate-generating
function, and an arbitrary number of columns to apply the predicate function
to. The row reduction function is applied to the data frame, and returns a value
for each row. The predicate-generating function is then applied to the vector
returned from the row reduction function and the resultant predicate is
applied to each element of that vector. It will raise an error if it finds any
violations. This functionality is useful, for example, in conjunction with
the `maha_dist()` function to ensure that there are no flagrant outliers.
Internally, the `assert_rows` function uses `dplyr`'s`select` function to
extract the columns to test the predicate function on.


`assertr` also offers four (so far) predicate functions designed to be used
with the `assert` and `assert_rows` functions:

- `not_na` - that checks if an element is not NA
- `within_bounds` - that returns a predicate function that checks if a numeric
value falls within the bounds supplied, and
- `in_set` - that returns a predicate function that checks if an element is
a member of the set supplied.
- `is_uniq` - that checks to see if each element appears only once

and predicate generators designed to be used with the `insist` and `insist_rows`
functions:

- `within_n_sds` - used to dynamically create bounds to check vector elements with
based on standard z-scores
- `within_n_mads` - better method for dynamically creating bounds to check vector
elements with based on 'robust' z-scores (using median absolute deviation)

and the following row reduction functions designed to be used with `assert_rows`
and `insist_rows`:

- `num_row_NAs` - counts number of missing values in each row
- `maha_dist` - computes the mahalanobis distance of each row (for outlier
detection). It will coerce categorical variables into numerics if it needs to.

Finally, each assertion function has a counterpart that using standard
evaluation. The counterpart functions are postfixed by "_" (an underscore).

### More info

For more info, check out the `assertr` vignette
```{r}
    > vignette("assertr")
```
Or [read it here](http://cran.r-project.org/web/packages/assertr/vignettes/assertr.html)

[![ropensci\_footer](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
