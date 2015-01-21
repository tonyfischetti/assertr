
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
### assert_vector(data.frame, column_from_data.frame, predicate_function) ###
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
assert_vector <- function(the.frame, column, predicate){
  name.of.column <- stringify(substitute(column))
  if(!(name.of.column %in% names(the.frame))){
    stop(paste0("column '", name.of.column, "' not in data.frame '",
                stringify(substitute(the.frame)), "'"))
  }
  name.of.predicate <- get.name.of.function(stringify(substitute(predicate)))
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


#' @export
assert <- assert_vector


