
###########################################################
##                                                       ##
##   predicates.R                                        ##
##                                                       ##
##      defines some useful predicates for use with      ##
##      assert functions in assertr                      ##
##                                                       ##
##                Author: Tony Fischetti                 ##
##                        tony.fischetti@gmail.com       ##
##                                                       ##
###########################################################




# PREDICATES (for vectors)

#' Returns TRUE if element is not NA
#'
#' This is the inverse of \code{\link[base]{is.na}} if it is
#' used on a atomic element. This is a convenience function meant
#' to be used as a predicate in an \code{\link{assertr}} assertion.
#'
#' @param x A single atomic value
#' @param allow.NaN A logical indicating whether NaNs should be allowed
#'        (default FALSE)
#' @return TRUE if x is not NA, FALSE otherwise
#' @seealso \code{\link{is.na}}
#' @examples
#' not_na(NA)
#' not_na(2.8)
#' not_na("tree")
#'
#' @export
not_na <- function(x, allow.NaN=FALSE){
  if(length(x)>1)            stop("not_na must be called with single element")
  if(is.null(x))             stop("not_na must be called with single element")
  if(allow.NaN && is.nan(x)) return(TRUE)
  if(is.na(x))               return(FALSE)
  return(TRUE)
}







#' Creates bounds checking predicate
#'
#' This function returns a predicate function that will take a single
#' numeric value and return TRUE if the value is within the bounds set.
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
#' @param allow.NA A logical indicating whether NAs (including NaNs)
#'        should be permitted (default TRUE)
#'
#' @return A function that takes one numeric and returns TRUE
#'         if the value is within the bounds defined by the
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
#' assert(mtcars, cyl, within_bounds(4,8))
#'
#' ## or in a pipeline, like this was meant for
#'
#' library(magrittr)
#'
#' mtcars %>%
#'   assert(cyl, within_bounds(4,8))
#'
#' @export
within_bounds <- function(lower.bound, upper.bound,
                          include.lower=TRUE, include.upper=TRUE,
                          allow.NA=TRUE){
  if(!(is.numeric(lower.bound) && is.numeric(upper.bound)))
    stop("bounds must be numeric")
  if(lower.bound >= upper.bound)
    stop("lower bound must be strictly lower than upper bound")
  function(x){
    if(length(x)>1)      stop("bounds must be checked on a single element")
    if(is.null(x))       stop("bounds must be checked on a single element")
    if(!is.numeric(x))   stop("bounds must only be checked on numerics")
    if(is.na(x)){
      if(allow.NA)    return(TRUE)
      if(!allow.NA)   return(FALSE)
    }
    lower.operator <- `>=`
    if(!include.lower) lower.operator <- `>`
    upper.operator <- `<=`
    if(!include.upper) upper.operator <- `<`
    if(lower.operator(x, lower.bound) && upper.operator(x, upper.bound))
      return(TRUE)
    return(FALSE)
  }

}
# so, this function returns a function to be used as argument to another
# function


# dummy
divby5 <- function(x){
  if(x %% 5 != 0)
    return(FALSE)
}



