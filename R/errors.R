
##
##  defines some error object constructors, error functions, and
##  error chaining functions
##


#######################################
#   assert error object and methods   #
#######################################
# used by "assert" and "insist"
make.assertr.assert.error <- function(name.of.predicate,
                                      column,
                                      num.violations,
                                      index.of.violations,
                                      offending.elements){
  time.or.times <- ifelse(num.violations==1, "time", "times")
  msg <- paste0("Column '", column, "' violates assertion '",
                name.of.predicate,"' ", num.violations, " ", time.or.times)

  this_error <- list()

  this_error$error_df <- data.frame(index=unname(index.of.violations),
                                    value=unname(offending.elements))
  this_error$message <- msg
  this_error$num.violations <- num.violations
  this_error$call <- name.of.predicate

  class(this_error) <- c("assertr_assert_error", "assertr_error",
                         "error", "condition")
  return(this_error)
}

# used by "assert_rows" and "insist_rows"
make.assertr.assert_rows.error <- function(name.of.rowredux.fn,
                                           name.of.predicate,
                                           num.violations,
                                           loc.violations){
  time.or.times <- ifelse(num.violations==1, "time", "times")
  msg <- paste0("Data frame row reduction '", name.of.rowredux.fn,
                "' violates predicate '", name.of.predicate,
                "' ", num.violations, " ", time.or.times)
  this_error <- list()

  this_error$error_df <- data.frame(rownumber=loc.violations)

  this_error$message <- msg
  this_error$num.violations <- num.violations
  this_error$call <- name.of.predicate

  class(this_error) <- c("assertr_assert_error", "assertr_error",
                         "error", "condition")
  return(this_error)
}


#' Printing assertr's assert errors
#'
#' `print` method for class "assertr_assert_error"
#' This prints the error message and the entire two-column
#' `data.frame` holding the indexes and values of the offending
#' data.
#'
#' @param x An assertr_assert_error object
#' @param ... Further arguments passed to or from other methods
#' @seealso \code{\link{summary.assertr_assert_error}}
#'
#' @export
print.assertr_assert_error <- function(x, ...){
  cat(x$message)
  cat("\n")
  print(x$error_df)
}

#' Summarizing assertr's assert errors
#'
#' `summary` method for class "assertr_assert_error"
#' This prints the error message and the first five
#' rows of the two-column `data.frame` holding the
#' indexes and values of the offending data.
#'
#' @param object An assertr_assert_error object
#' @param ... Additional arguments affecting the summary produced
#' @seealso \code{\link{print.assertr_assert_error}}
#'
#' @export
summary.assertr_assert_error <- function(object, ...){
  cat(object$message)
  cat("\n")
  numrows <- nrow(object$error_df)
  print(utils::head(object$error_df, n=5))
  if(numrows > 5)
    cat(paste0("  [omitted ", numrows-5, " rows]\n\n"))
}


#####################
#   verify errors   #
#####################
# used by "verify"

make.assertr.verify.error <- function(num.violations, the_call){
  sing.plur <- ifelse(num.violations==1, " failure)", " failures)")
  msg <- paste0("verification [", the_call, "] failed! (", num.violations, sing.plur)
  this_error <- list()
  this_error$message <- msg
  this_error$num.violations <- num.violations
  this_error$call <- the_call
  class(this_error) <- c("assertr_verify_error", "assertr_error",
                         "error", "condition")
  return(this_error)
}

#' Printing assertr's verify errors
#'
#' `summary` method for class "assertr_verify_error"
#'
#' @param x An assertr_verify_error object.
#' @param ... Further arguments passed to or from other methods
#' @seealso \code{\link{summary.assertr_verify_error}}
#'
#' @export
print.assertr_verify_error <- function(x, ...){
  cat(x$message)
  cat("\n\n")
}

#' Summarizing assertr's verify errors
#'
#' `summary` method for class "assertr_verify_error"
#'
#' @param object An assertr_verify_error object
#' @param ... Additional arguments affecting the summary produced
#' @seealso \code{\link{print.assertr_verify_error}}
#'
#' @export
summary.assertr_verify_error <- function(object, ...){ print(object) }





#' Success and error functions
#'
#' The behavior of functions like \code{assert}, \code{assert_rows},
#' \code{insist}, \code{insist_rows}, \code{verify} when the assertion
#' passes or fails is configurable via the \code{success_fun}
#' and \code{error_fun} parameters, respectively.
#' The \code{success_fun} parameter takes a function that takes
#' the data passed to the assertion function as a parameter. You can
#' write your own success handler function, but there are two
#' provided by this package:
#' \itemize{
#'   \item \code{success_continue} - just returns the data that was
#'                                    passed into the assertion function
#'   \item \code{success_logical} - returns TRUE
#' }
#' The \code{error_fun} parameter takes a function that takes
#' the data passed to the assertion function as a parameter. You can
#' write your own error handler function, but there are a few
#' provided by this package:
#' \itemize{
#'   \item \code{error_stop} - Prints a summary of the errors and
#'                             halts execution.
#'   \item \code{error_report} - Prints all the information available
#'                               about the errors and halts execution.
#'   \item \code{error_append} - Attaches the errors to a special
#'    attribute of \code{data} and returns the data. This is chiefly
#'    to allow assertr errors to be accumulated in a pipeline so that
#'    all assertions can have a chance to be checked and so that all
#'    the errors can be displayed at the end of the chain.
#'   \item \code{error_logical} - returns FALSE
#'   \item \code{just_warn} - Prints a summary of the errors but does
#'    not halt execution, it just issues a warning.
#'   \item \code{warn_report} - Prints all the information available
#'   about the errors but does not halt execution, it just issues a warning.
#'  }
#' @name success_and_error_functions
NULL



#########################
#   success functions   #
#########################

#' @export
#' @rdname success_and_error_functions
#' @param data A data frame
success_logical <- function(data, ...){ return(TRUE) }

#' @export
#' @rdname success_and_error_functions
success_continue <- function(data, ...){ return(data) }


#######################
#   error functions   #
#######################

#' @export
#' @rdname success_and_error_functions
#' @param errors A list of objects of class \code{assertr_errors}
#' @param warn If TRUE, assertr will issue a warning instead of an error
error_stop <- function(errors, data=NULL, warn=FALSE, ...){
  if(!is.null(data) && !is.null(attr(data, "assertr_errors")))
    errors <- append(attr(data, "assertr_errors"), errors)
  lapply(errors, summary)
  if(!warn)
    stop("assertr stopped execution", call.=FALSE)
  warning("assertr encountered errors", call.=FALSE)
  return(data)
}
# for backwards compatibility
assertr_stop <- error_stop

#' @export
#' @rdname success_and_error_functions
just_warn <- function(errors, data=NULL){
  error_stop(errors, data, warn=TRUE)
}

#' @export
#' @rdname success_and_error_functions
error_report <- function(errors, data=NULL, warn=FALSE, ...){
  if(!is.null(data) && !is.null(attr(data, "assertr_errors")))
    errors <- append(attr(data, "assertr_errors"), errors)
  num.of.errors <- length(errors)
  cat(sprintf("There %s %d error%s:\n",
              ifelse(num.of.errors==1,"is", "are"),
              num.of.errors,
              ifelse(num.of.errors==1,"", "s")))
  lapply(errors, function(x){cat("\n- "); print(x)})
  if(!warn)
    stop("assertr stopped execution", call.=FALSE)
  warning("assertr encountered errors", call.=FALSE)
  return(data)
}

#' @export
#' @rdname success_and_error_functions
warn_report <- function(errors, data=NULL){
  error_report(errors, data, warn=TRUE)
}

#' @export
#' @rdname success_and_error_functions
error_append <- function(errors, data=NULL){
  if(is.null(attr(data, "assertr_errors")))
    attr(data, "assertr_errors") <- list()
  attr(data, "assertr_errors") <- append(attr(data, "assertr_errors"), errors)
  return(data)
}

#' @export
#' @rdname success_and_error_functions
error_return <- function(errors, data=NULL){
  if(!is.null(data) && !is.null(attr(data, "assertr_errors")))
    errors <- append(attr(data, "assertr_errors"), errors)
  return(errors)
}

#' @export
#' @rdname success_and_error_functions
#' @param ... Further arguments passed to or from other methods
error_logical <- function(errors, data=NULL, ...){
  return(FALSE)
}


##########################
#   chaining functions   #
##########################

#' Chaining functions
#'
#' These functions are for starting and ending a sequence of assertr
#' assertions and overriding the default behavior of assertr halting
#' execution on the first error.
#'
#' For more information, read the relevant section in this package's
#' vignette using, \code{vignette("assertr")}
#'
#' For examples of possible choices for the \code{success_fun} and
#' \code{error_fun} parameters, run \code{help("success_and_error_functions")}
#'
#' @name chaining_functions
#'
#' @examples
#' library(magrittr)
#'
#' mtcars %>%
#'   chain_start() %>%
#'   verify(nrow(mtcars) > 10) %>%
#'   verify(mpg > 0) %>%
#'   insist(within_n_sds(4), mpg) %>%
#'   assert(in_set(0,1), am, vs) %>%
#'   chain_end()
NULL

#' @export
#' @rdname chaining_functions
chain_start <- function(data){
  attr(data, "assertr_in_chain_success_fun_override") <- success_continue
  attr(data, "assertr_in_chain_error_fun_override") <- error_append
  return(data)
}

#' @param data A data frame
#' @param success_fun Function to call if assertion passes. Defaults to
#'                    returning \code{data}.
#' @param error_fun Function to call if assertion fails. Defaults to printing
#'                  a summary of all errors.
#' @export
#' @rdname chaining_functions
chain_end <- function(data, success_fun=success_continue,
                      error_fun=error_report){
  list_of_errors <- attr(data, "assertr_errors")
  attr(data, "assertr_errors") <- NULL
  if(is.null(list_of_errors))
    return(success_fun(data))
  error_fun(list_of_errors, data=data)
}


