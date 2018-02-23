
##
##  defines some useful predicates for use with assert functions in assertr                      ##
##


#' Returns TRUE if value is not NA
#'
#' This is the inverse of \code{\link[base]{is.na}}. This is a convenience
#' function meant to be used as a predicate in an \code{\link{assertr}}
#' assertion.
#'
#' @param x A R object that supports \link{is.na} an \link{is.nan}
#' @param allow.NaN A logical indicating whether NaNs should be allowed
#'        (default FALSE)
#' @return A vector of the same length that is TRUE when the element is
#' not NA and FALSE otherwise
#' @seealso \code{\link{is.na}} \code{\link{is.nan}}
#' @examples
#' not_na(NA)
#' not_na(2.8)
#' not_na("tree")
#' not_na(c(1, 2, NA, 4))
#'
#' @export
not_na <- function(x, allow.NaN=FALSE){
  if(is.null(x))    stop("not_na must be called on non-null object")
  if(allow.NaN)     return((!is.na(x)) | is.nan(x))
  return(!is.na(x))
}
# so assert function knows to vectorize the function for
# substantial speed increase
attr(not_na, "assertr_vectorized") <- TRUE
attr(not_na, "call") <- "not_na"

#' Creates bounds checking predicate
#'
#' This function returns a predicate function that will take a numeric value
#' or vector and return TRUE if the value(s) is/are within the bounds set.
#' This does not actually check the bounds of anything--it only returns
#' a function that actually does the checking when called with a number.
#' This is a convenience function meant to return a predicate function to
#' be used in an \code{\link{assertr}} assertion.
#'
#' @param lower.bound The lowest permitted value
#' @param upper.bound The upper permitted value
#' @param include.lower A logical indicating whether lower bound
#'        should be inclusive (default TRUE)
#' @param include.upper A logical indicating whether upprt bound
#'        should be inclusive (default TRUE)
#' @param allow.na A logical indicating whether NAs (including NaNs)
#'        should be permitted (default TRUE)
#'
#' @return A function that takes numeric value or numeric vactor and returns
#'         TRUE if the value(s) is/are within the bounds defined by the
#'         arguments supplied by \code{within_bounds} and FALSE
#'         otherwise
#'
#' @examples
#' predicate <- within_bounds(3,4)
#' predicate(pi)
#'
#' ## is equivalent to
#'
#' within_bounds(3,4)(pi)
#'
#' # a correlation coefficient must always be between 0 and 1
#' coeff <- cor.test(c(1,2,3), c(.5, 2.4, 4))[["estimate"]]
#' within_bounds(0,1)(coeff)
#'
#' ## check for positive number
#' positivep <- within_bounds(0, Inf, include.lower=FALSE)
#'
#' ## this is meant to be used as a predicate in an assert statement
#' assert(mtcars, within_bounds(4,8), cyl)
#'
#' ## or in a pipeline
#'
#' library(magrittr)
#'
#' mtcars %>%
#'   assert(within_bounds(4,8), cyl)
#'
#' @export
within_bounds <- function(lower.bound, upper.bound,
                          include.lower=TRUE, include.upper=TRUE,
                          allow.na=TRUE){
  the_call <- paste(deparse(sys.call()), collapse = " ")
  if(!(is.numeric(lower.bound) && is.numeric(upper.bound)))
    stop("bounds must be numeric")
  if(lower.bound >= upper.bound)
    stop("lower bound must be strictly lower than upper bound")
  fun <- function(x){
    if(is.null(x))       stop("bounds must be checked on non-null element")
    if(!is.numeric(x))   stop("bounds must only be checked on numerics")
    lower.operator <- if(!include.lower) `>` else `>=`
    upper.operator <- if(!include.upper) `<` else `<=`
    if(allow.na){
      return((lower.operator(x, lower.bound) &
                upper.operator(x, upper.bound)) | is.na(x))
    }
    return((lower.operator(x, lower.bound) &
              upper.operator(x, upper.bound)) & !(is.na(x)))
  }
  attr(fun, "assertr_vectorized") <- TRUE
  attr(fun, "call") <- the_call
  return(fun)
}
# so, this function returns a function to be used as argument to another
# function



#' Returns TRUE if value in set
#'
#' This function returns a predicate function that will take a single
#' value and return TRUE if the value is a member of the set of objects
#' supplied. This doesn't actually check the membership of anything--it
#' only returns a function that actually does the checking when called
#' with a value. This is a convenience function meant to return a
#' predicate function to be used in an \code{\link{assertr}} assertion.
#'
#' @param ... objects that make up the set
#' @param allow.na A logical indicating whether NAs (including NaNs)
#'        should be permitted (default TRUE)
#' @return A function that takes one value and returns TRUE
#'         if the value is in the set defined by the
#'         arguments supplied by \code{in_set} and FALSE
#'         otherwise
#' @seealso \code{\link{\%in\%}}
#' @examples
#' predicate <- in_set(3,4)
#' predicate(4)
#'
#' ## is equivalent to
#'
#' in_set(3,4)(3)
#'
#' # the remainder of division by 2 is always 0 or 1
#' rem <- 10 %% 2
#' in_set(0,1)(rem)
#'
#' ## this is meant to be used as a predicate in an assert statement
#' assert(mtcars, in_set(3,4,5), gear)
#'
#' ## or in a pipeline, like this was meant for
#'
#' library(magrittr)
#'
#' mtcars %>%
#'   assert(in_set(3,4,5), gear) %>%
#'   assert(in_set(0,1), vs, am)
#'
#' @export
in_set <- function(..., allow.na=TRUE){
  the_call <- paste(deparse(sys.call()), collapse = " ")
  set <- c(...)
  if(!length(set)) stop("can not test for membership in empty set")
  fun <- function(x){
    if(is.null(x))       stop("nothing to check set membership to")

    raw_result <- x %in% set
    if(allow.na){
      these_are_NAs <- is.na(x)
      raw_result[these_are_NAs] <- TRUE
    }
    return(raw_result)
  }
  attr(fun, "assertr_vectorized") <- TRUE
  attr(fun, "call") <- the_call
  return(fun)
}


#' Return a function to create z-score checking predicate
#'
#' This function takes one argument, the number of standard deviations
#' within which to accept a particular data point.
#'
#' As an example, if '2' is passed into this function, this will return
#' a function that takes a vector and figures out the bounds of two
#' standard deviations from the mean. That function will then return
#' a \code{\link{within_bounds}} function that can then be applied
#' to a single datum. If the datum is within two standard deviations of
#' the mean of the vector given to the function returned by this function,
#' it will return TRUE. If not, FALSE.
#'
#' This function isn't meant to be used on its own, although it can. Rather,
#' this function is meant to be used with the \code{\link{insist}} function to
#' search for potentially erroneous data points in a data set.
#'
#' @param n The number of standard deviations from the mean
#'        within which to accept a datum
#' @param ... Additional arguments to be passed to \code{\link{within_bounds}}
#'
#' @return A function that takes a vector and returns a
#'         \code{\link{within_bounds}} predicate based on the standard deviation
#'         of that vector.
#' @seealso \code{\link{within_n_mads}}
#' @examples
#' test.vector <- rnorm(100, mean=100, sd=20)
#'
#' within.one.sd <- within_n_sds(1)
#' custom.bounds.checker <- within.one.sd(test.vector)
#' custom.bounds.checker(105)     # returns TRUE
#' custom.bounds.checker(40)      # returns FALSE
#'
#' # same as
#' within_n_sds(1)(test.vector)(40)    # returns FALSE
#'
#' within_n_sds(2)(test.vector)(as.numeric(NA))  # returns TRUE
#' # because, by default, within_bounds() will accept
#' # NA values. If we want to reject NAs, we have to
#' # provide extra arguments to this function
#' within_n_sds(2, allow.na=FALSE)(test.vector)(as.numeric(NA))  # returns FALSE
#'
#' # or in a pipeline, like this was meant for
#'
#' library(magrittr)
#'
#' iris %>%
#'   insist(within_n_sds(5), Sepal.Length)
#'
#' @export
within_n_sds <- function(n, ...){
  the_call <- paste(deparse(sys.call()), collapse = " ")
  if(!is.numeric(n) || length(n)!=1 || n<=0){
    stop("'n' must be a positive number")
  }
  fun <- function(a.vector){
    if(!is.vector(a.vector) || !is.numeric(a.vector))
      stop("argument must be a numeric vector")
    mu <- mean(a.vector, na.rm=TRUE)
    stdev <- stats::sd(a.vector, na.rm=TRUE)
    if(is.na(mu)) stop("mean of vector is NA")
    if(is.na(stdev)) stop("standard deviations of vector is NA")
    if(stdev==0) stop("standard deviation of vector is 0")
    within_bounds((mu-(n*stdev)), (mu+(n*stdev)), ...)
  }
  attr(fun, "call") <- the_call
  return(fun)

}


#' Return a function to create robust z-score checking predicate
#'
#' This function takes one argument, the number of median absolute
#' deviations within which to accept a particular data point. This is
#' generally more useful than its sister function \code{\link{within_n_sds}}
#' because it is more robust to the presence of outliers. It is therefore
#' better suited to identify potentially erroneous data points.
#'
#' As an example, if '2' is passed into this function, this will return
#' a function that takes a vector and figures out the bounds of two
#' median absolute deviations (MADs) from the median. That function will then
#' return a \code{\link{within_bounds}} function that can then be applied
#' to a single datum. If the datum is within two MADs of the median of the
#' vector given to the function returned by this function, it will return TRUE.
#' If not, FALSE.
#'
#' This function isn't meant to be used on its own, although it can. Rather,
#' this function is meant to be used with the \code{\link{insist}} function to
#' search for potentially erroneous data points in a data set.
#'
#' @param n The number of median absolute deviations from the median
#'        within which to accept a datum
#' @param ... Additional arguments to be passed to \code{\link{within_bounds}}
#'
#' @return A function that takes a vector and returns a
#'         \code{\link{within_bounds}} predicate based on the MAD
#'         of that vector.
#' @seealso \code{\link{within_n_sds}}
#' @examples
#' test.vector <- rnorm(100, mean=100, sd=20)
#'
#' within.one.mad <- within_n_mads(1)
#' custom.bounds.checker <- within.one.mad(test.vector)
#' custom.bounds.checker(105)     # returns TRUE
#' custom.bounds.checker(40)      # returns FALSE
#'
#' # same as
#' within_n_mads(1)(test.vector)(40)    # returns FALSE
#'
#' within_n_mads(2)(test.vector)(as.numeric(NA))  # returns TRUE
#' # because, by default, within_bounds() will accept
#' # NA values. If we want to reject NAs, we have to
#' # provide extra arguments to this function
#' within_n_mads(2, allow.na=FALSE)(test.vector)(as.numeric(NA))  # returns FALSE
#'
#' # or in a pipeline, like this was meant for
#'
#' library(magrittr)
#'
#' iris %>%
#'   insist(within_n_mads(5), Sepal.Length)
#'
#' @export
within_n_mads <- function(n, ...){
  the_call <- paste(deparse(sys.call()), collapse = " ")
  if(!is.numeric(n) || length(n)!=1 || n<=0){
    stop("'n' must be a positive number")
  }
  fun <- function(a.vector){
    if(!is.vector(a.vector) || !is.numeric(a.vector))
      stop("argument must be a numeric vector")
    dmad <- stats::mad(a.vector, na.rm=TRUE)
    dmed <- stats::median(a.vector, na.rm=TRUE)
    if(is.na(dmad)) stop("MAD of vector is NA")
    if(dmad==0) stop("MAD of vector is 0")
    if(is.na(dmed)) stop("median of vector is NA")
    within_bounds((dmed-(n*dmad)), (dmed+(n*dmad)), ...)
  }
  attr(fun, "call") <- the_call
  return(fun)
}


#' Returns TRUE where no elements appear more than once
#'
#' This function is meant to take only a vector. It relies heavily on
#' the \code{\link{duplicated}} function where it can be thought of as
#' the inverse. Where this function differs, though--besides being only
#' meant for one vector or column--is that it marks the first occurrence
#' of a duplicated value as "non unique", as well.
#'
#' @param x A vector to check for unique elements in
#' @param allow.na A logical indicating whether NAs should be preserved
#'                 as missing values in the return value (FALSE) or
#'                 if they should be treated just like any other value
#'                 (TRUE) (default is FALSE)
#'
#' @return A vector of the same length where the corresponding element
#'         is TRUE if the element only appears once in the vector and
#'         FALSE otherwise
#' @seealso \code{\link{duplicated}}
#' @examples
#'
#' is_uniq(1:10)
#'
#' \dontrun{
#' # returns FALSE where a "5" appears
#' is_uniq(c(1:10, 5))
#' }
#'
#' library(magrittr)
#'
#' \dontrun{
#' # this fails 4 times
#' mtcars %>% assert(is_uniq, qsec)
#' }
#'
#' @export
is_uniq <- function(x, allow.na=FALSE){
  if(is.null(x))    stop("is_uniq must be called on non-null object")
  raw_result <- !duplicated(x)
  repeats <- x[!raw_result]
  raw_result[x %in% repeats] <- FALSE
  if(!allow.na){
    these_are_NAs <- is.na(x)
    raw_result[these_are_NAs] <- NA
  }
  return(raw_result)
}
attr(is_uniq, "call") <- "is_uniq"
attr(is_uniq, "assertr_vectorized") <- TRUE

