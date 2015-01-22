
###########################################################
##                                                       ##
##   assertions.R                                        ##
##                                                       ##
##      provides different assertion functions for       ##
##      different use cases and one master assert()      ##
##      function that delegates which specific one       ##
##      to use                                           ##
##                                                       ##
##                                                       ##
##                Author: Tony Fischetti                 ##
##                        tony.fischetti@gmail.com       ##
##                                                       ##
###########################################################



# assert just returns what it's given



#############################################################################
### assert(data.frame, column_from_data.frame, predicate_function) ###
#############################################################################
# uses predicate provided on every element of a vector
# and either errors out (with helpful message)
# or returns the data.frame it was called with

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
assert <- function(the.frame, column, predicate){
  #assert <- function(data, attrbs, predicate,
  name.of.column <- deparse(substitute(column))
  if(!(name.of.column %in% names(the.frame))){
    stop(paste0("column '", name.of.column, "' not in data.frame '",
                stringify(substitute(the.frame)), "'"))
  }
  name.of.predicate <- get.name.of.function(deparse(substitute(predicate)))
  the.vector <- the.frame[[name.of.column]]
  full.predicate <- make.predicate.proper(predicate)
  log.vect <- vapply(the.vector, full.predicate, logical(1))
  violations <- which(!(log.vect))
  if(length(violations)){
    first.violation <- violations[1]
    offending.element <- the.vector[first.violation]
    error.message <- make.error.message(name.of.predicate, first.violation,
                                        name.of.column, offending.element)
    stop(error.message, call.=FALSE)
  }
  return(the.frame)
}
# REWRITE THIS SO IT STOPS AT THE FIRST VIOLATION






#' Raises error if expression is FALSE anywhere
#'
#' Meant for use in a data analysis pipeline, this function will
#' just return the data it's supplied if all the logicals in the
#' expression supplied are TRUE. If at least one is FALSE, this
#' function will raise a error, effectively terminating the pipeline
#' early
#'
#' @param data A data frame, list, or environment
#' @param expr A logical expression
#' @param character.only a logical indicating whether attributes
#'        from \code{data} in \code{expr} can be assumed to be
#'        character strings (default FALSE)
#'
#' @return data if verification passes. errors if not
#' @note See \code{vignette("assertr")} for how to use this in context
#' @seealso \code{\link{assert}}
#' @examples
#'
#' verify(mtcars, drat > 2)     # returns mtcars
#' verify(mtcars, drat > 3)     # produces error
#'
#' library(magrittr)            # for piping operator
#'
#' mtcars %>%
#'   verify(drat > 3) %>%
#'   # anything here will not run
#'
#' mtcars %>%
#'  verify(nrow(mtcars)>2)
#'  # nothing here will run
#'
#' alist <- list(a=c(1,2,3), b=c(4,5,6))
#' verify(alist, length(a) > 2)
#' verify(alist, length(a) > 2 && length(b) > 2)
#' verify(alist, a > 0 & b > 2)
#'
#' alist %>%
#'   verify(alist, length(a) > 5)
#'   # nothing here will run
#'
#'
#' @export
verify <- function(data, expr, character.only=FALSE){
  if(!character.only)
    expr <- substitute(expr)
  # conform to terminology from subset
  envir <- data
  enclos <- parent.frame()
  logical.results <- eval(expr, envir, enclos)
  if(all(logical.results))
    return(data)
  stop("verification failed!")
}







