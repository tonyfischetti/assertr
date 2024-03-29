% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicates.R
\name{within_bounds}
\alias{within_bounds}
\title{Creates bounds checking predicate}
\usage{
within_bounds(
  lower.bound,
  upper.bound,
  include.lower = TRUE,
  include.upper = TRUE,
  allow.na = TRUE,
  check.class = TRUE
)
}
\arguments{
\item{lower.bound}{The lowest permitted value}

\item{upper.bound}{The upper permitted value}

\item{include.lower}{A logical indicating whether lower bound
should be inclusive (default TRUE)}

\item{include.upper}{A logical indicating whether upprt bound
should be inclusive (default TRUE)}

\item{allow.na}{A logical indicating whether NAs (including NaNs)
should be permitted (default TRUE)}

\item{check.class}{Should the class of the \code{lower.bound},
\code{upper_bound}, and the input to the returned function be checked
to be numeric or of the same class?  If \code{FALSE}, the comparison
may have unexpected results.}
}
\value{
A function that takes numeric value or numeric vactor and returns
        TRUE if the value(s) is/are within the bounds defined by the
        arguments supplied by \code{within_bounds} and FALSE
        otherwise
}
\description{
This function returns a predicate function that will take a numeric value
or vector and return TRUE if the value(s) is/are within the bounds set.
This does not actually check the bounds of anything--it only returns
a function that actually does the checking when called with a number.
This is a convenience function meant to return a predicate function to
be used in an \code{\link{assertr}} assertion.
}
\examples{
predicate <- within_bounds(3,4)
predicate(pi)

## is equivalent to

within_bounds(3,4)(pi)

# a correlation coefficient must always be between 0 and 1
coeff <- cor.test(c(1,2,3), c(.5, 2.4, 4))[["estimate"]]
within_bounds(0,1)(coeff)

## check for positive number
positivep <- within_bounds(0, Inf, include.lower=FALSE)

## this is meant to be used as a predicate in an assert statement
assert(mtcars, within_bounds(4,8), cyl)

## or in a pipeline

library(magrittr)

mtcars \%>\%
  assert(within_bounds(4,8), cyl)

}
