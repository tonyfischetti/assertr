
##
##  defines some error object constructors, error functions, and
##  error chaining functions
##


#######################################
#   assert error object and methods   #
#######################################
# used by "assert" and "insist"
make.assertr.assert.error <- function(verb,
                                      name.of.predicate,
                                      column,
                                      num.violations,
                                      index.of.violations,
                                      offending.elements,
                                      description,
                                      assertion.id){
  time.or.times <- if (num.violations==1) "time" else "times"
  msg <- paste0("Column '", column, "' violates assertion '",
                name.of.predicate,"' ", num.violations, " ", time.or.times)

  this_error <- list()

  this_error$error_df <- data.frame(verb=verb,
                                    redux_fn=NA,
                                    predicate=name.of.predicate,
                                    column=column,
                                    index=unname(index.of.violations),
                                    value=unname(offending.elements))
  this_error$message <- msg
  this_error$num.violations <- num.violations
  this_error$call <- name.of.predicate
  this_error$description <- description
  this_error$assertion.id <- assertion.id


  class(this_error) <- c("assertr_assert_error", "assertr_error",
                         "error", "condition")
  return(this_error)
}

# used by "assert_rows" and "insist_rows"
make.assertr.assert_rows.error <- function(verb,
                                           name.of.rowredux.fn,
                                           name.of.predicate,
                                           column,
                                           num.violations,
                                           loc.violations,
                                           offending.elements,
                                           description,
                                           assertion.id){
  time.or.times <- if (num.violations==1) "time" else "times"
  msg <- paste0("Data frame row reduction '", name.of.rowredux.fn,
                "' violates predicate '", name.of.predicate,
                "' ", num.violations, " ", time.or.times)
  error_df <- data.frame(verb=verb,
                         redux_fn=name.of.rowredux.fn,
                         predicate=name.of.predicate,
                         column=column,
                         index=loc.violations,
                         value=offending.elements)
  rownames(error_df) <- NULL
  this_error <- list(error_df = error_df,
                     message = msg,
                     num.violations = num.violations,
                     call = name.of.predicate,
                     description = description,
                     assertion.id = assertion.id)

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
  if (!is.na(x$description))
    cat(x$description, "\n")
  cat(x$message)
  cat("\n")
  print(x$error_df)
}

#' Printing assertr's success
#'
#' `print` method for class "assertr_success"
#' This prints the success message along with columns that were checked.
#'
#' @param x An assertr_success object
#' @param ... Further arguments passed to or from other methods
#'
#' @export
print.assertr_success <- function(x, ...){
  if (!is.na(x$description))
    cat(x$description, "\n")
  cat(paste0(x$verb, ":"), x$message)
  if ((length(x$columns) > 2) || !identical(x$columns, NA)) {
    cat(paste(" Verified columns:", paste0(x$columns, collapse = " ")), "\n")
  } else {
    cat("\n")
  }
}

#' Printing assertr's defect
#'
#' `print` method for class "assertr_defect"
#' This prints the defect message along with columns that were checked.
#'
#' @param x An assertr_defect object
#' @param ... Further arguments passed to or from other methods
#'
#' @export
print.assertr_defect <- function(x, ...){
  if (!is.na(x$description))
    cat(x$description, "\n")
  cat(paste0(x$verb, ":"), x$message)
  if ((length(x$columns) > 2) || !identical(x$columns, NA)) {
    cat(paste(" Columns passed to assertion:", paste0(x$columns, collapse = " ")), "\n")
  } else {
    cat("\n")
  }
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
    cat("  [omitted ", numrows-5, " rows]\n\n", sep = "")
}

#####################
#   verify errors   #
#####################
# used by "verify"

make.assertr.verify.error <- function(verb, num.violations, the_call,
                                      logical.results, description, assertion.id){
  sing.plur <- if (num.violations==1) " failure)" else " failures)"
  msg <- paste0("verification [", the_call, "] failed! (", num.violations, sing.plur)

  error_df <- data.frame(verb = verb,
                         redux_fn = NA,
                         predicate = paste(the_call, collapse=" "),
                         column = NA,
                         index = logical.results,
                         value = NA)

  this_error <- list(error_df = error_df,
                     message = msg,
                     num.violations = num.violations,
                     call = the_call,
                     description = description,
                     assertion.id = assertion.id)
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
  if (!is.na(x$description))
    cat(x$description, "\n")
  cat(x$message)
  cat("\n\n")
  print(x$error_df)
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
#' write your own success handler function, but there are a few
#' provided by this package:
#' \itemize{
#'   \item \code{success_continue} - just returns the data that was
#'                                    passed into the assertion function
#'   \item \code{success_logical} - returns TRUE
#'   \item \code{success_append} - returns the data that was
#'                                 passed into the assertion function
#'                                 but also stores basic information about
#'                                 verification result
#'   \item \code{success_report} - When success results are stored, and each
#'                                 verification ended up with success prints
#'                                 summary of all successful validations
#'   \item \code{success_df_return} - When success results are stored, and each
#'                                    verification ended up with success prints
#'                                    data.frame with verification results
#' }
#' The \code{error_fun} parameter takes a function that takes
#' the data passed to the assertion function as a parameter. You can
#' write your own error handler function, but there are a few
#' provided by this package:
#' \itemize{
#'   \item \code{error_stop} - Prints a summary of the errors and
#'                             halts execution.
#'   \item \code{error_report} - Prints all the information available
#'                               about the errors in a "tidy"
#'                               \code{data.frame} (including information
#'                               such as the name of the predicate used,
#'                               the offending value, etc...) and halts
#'                               execution.
#'   \item \code{error_append} - Attaches the errors to a special
#'    attribute of \code{data} and returns the data. This is chiefly
#'    to allow assertr errors to be accumulated in a pipeline so that
#'    all assertions can have a chance to be checked and so that all
#'    the errors can be displayed at the end of the chain.
#'   \item \code{error_return} - Returns the raw object containing all
#'     the errors
#'   \item \code{error_df_return} - Returns a "tidy" \code{data.frame}
#'     containing all the errors, including informations such as
#'     the name of the predicate used, the offending value, etc...
#'   \item \code{error_logical} - returns FALSE
#'   \item \code{just_warn} - Prints a summary of the errors but does
#'    not halt execution, it just issues a warning.
#'   \item \code{warn_report} - Prints all the information available
#'   about the errors but does not halt execution, it just issues a warning.
#'   \item \code{defect_report} - For single rule and defective data it displays
#'   short info about skipping current assertion. For \code{chain_end} sums
#'   up all skipped rules for defective data.
#'   \item \code{defect_df_return} - For single rule and defective data it returns
#'   info data.frame about skipping current assertion. For \code{chain_end}
#'   returns all skipped rules info data.frame for defective data.
#'  }
#' You may find the third type of data verification result. In a scenario
#' when validation rule was obligatory (obligatory = TRUE) in order to execute the
#' following ones we may want to skip them and register that fact.
#' In order to do this there are three callbacks reacting to defective
#' data:
#'  \itemize{
#'   \item \code{defect_report} - For single rule and defective data it displays
#'   short info about skipping current assertion.
#'   \item \code{defect_df_return} - For single rule and defective data it returns
#'   info data.frame about skipping current assertion.
#'   \item \code{defect_append} - Appends info about skipped rule due to data
#'   defect into one of data attributes. Rules skipped on defective data, or its summary, can
#'   be returned with proper error_fun callback in \code{chain_end}.
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

#' @export
#' @rdname success_and_error_functions
success_append <- function(data, ...){
  verb <- ..1
  the_call <- ..2
  columns <- ..3
  row_redux_call <- ..4
  description <- ..5
  row_redux_message <- ""
  if (!is.na(row_redux_call))
    row_redux_message <- paste0(" on ", row_redux_call, " row reduction")
  msg <- paste0("verification [", the_call, "]", row_redux_message, " passed!")
  this_success <- list(
    verb = verb,
    message = msg,
    call = paste0(the_call, collapse = " "),
    columns = columns,
    row_redux_call = row_redux_call,
    description = description
  )
  class(this_success) <- c("assertr_success", "success", "condition")
  if(is.null(attr(data, "assertr_success")))
    attr(data, "assertr_success") <- list()
  attr(data, "assertr_success") <- append(attr(data, "assertr_success"), list(this_success))
  return(data)
}

#' @export
#' @rdname success_and_error_functions
success_report <- function(data, ...){
  n_assertions <- length(attr(data, "assertr_success"))
  if(is.null(attr(data, "assertr_in_chain_success_fun_override"))) {
    # single verification case
    data <- success_append(data, ...)
    print(attr(data, "assertr_success")[[1]])
  } else {
    # chain_end case
    if (n_assertions==0) {
      cat("No success results stored.")
      return(data)
    }
    result <- "result"
    if (n_assertions>1)
      result <- "results"
    cat(n_assertions, result, "verified:", "\n")
    lapply(attr(data, "assertr_success"), print)
    attr(data, "assertr_in_chain_success_fun_override") <- NULL
  }
  attr(data, "assertr_success") <- NULL
  return(invisible(data))
}

#' @export
#' @rdname success_and_error_functions
success_df_return <- function(data, ...){
  n_assertions <- length(attr(data, "assertr_success"))
  success_to_df <- function(success) {
    if (!identical(success$columns, NA))
      success$columns <- paste0(success$columns, collapse = ", ")
    data.frame(
      verb = success$verb,
      message = success$message,
      call = success$call,
      columns = success$columns,
      row_redux_call = success$row_redux_call,
      description = success$description,
      stringsAsFactors = FALSE
    )
  }
  if(is.null(attr(data, "assertr_in_chain_success_fun_override"))) {
    # single verification case
    data <- success_append(data, ...)
    success_df <- success_to_df(attr(data, "assertr_success")[[1]])
  } else {
    # chain_end case
    if (n_assertions==0) {
      stop("No success results stored.")
    }
    success_df <- do.call(rbind, lapply(attr(data, "assertr_success"), success_to_df))
    attr(data, "assertr_in_chain_success_fun_override") <- NULL
  }
  attr(data, "assertr_success") <- NULL
  return(success_df)
}

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
  lapply(errors, function(x){ summary(x); cat("\n")})
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

  total.num.failures <- sum(unlist(lapply(errors, function(x) x$num.violations)))
  big.error.frame <- do.call(rbind, lapply(errors, function(x) x$error_df))
  num.of.errors <- length(errors)
  cat(sprintf("There %s %d error%s across %d verb%s:\n",
              if (total.num.failures==1) "is" else "are",
              total.num.failures,
              if (total.num.failures==1) "" else "s",
              num.of.errors,
              if (num.of.errors==1) "" else "s"))
  cat("- \n")
  print(big.error.frame)
  cat("\n")

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
warning_append <- function(errors, data=NULL){
  attr(errors[[1]], "warning") <- TRUE
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
error_df_return <- function(errors, data=NULL){
  if(!is.null(data) && !is.null(attr(data, "assertr_errors")))
    errors <- append(attr(data, "assertr_errors"), errors)
  big.error.frame <- do.call(rbind, lapply(errors, function(x) x$error_df))
  return(big.error.frame)
}

#' @export
#' @rdname success_and_error_functions
#' @param ... Further arguments passed to or from other methods
error_logical <- function(errors, data=NULL, ...){
  return(FALSE)
}


##########################
#   defect functions   #
##########################

#' @export
#' @rdname success_and_error_functions
defect_append <- function(errors, data, ...){
  verb <- ..1
  the_call <- ..2
  columns <- ..3
  row_redux_call <- ..4
  description <- ..5
  row_redux_message <- ""
  if (!is.na(row_redux_call))
    row_redux_message <- paste0(" on ", row_redux_call, " row reduction")
  msg <- paste0("verification [", the_call, "]", row_redux_message, " omitted due to data defect!")
  this_defect <- list(
    verb = verb,
    message = msg,
    call = paste0(the_call, collapse = " "),
    columns = columns,
    row_redux_call = row_redux_call,
    description = description
  )
  class(this_defect) <- c("assertr_defect", "defect", "condition")
  if(is.null(attr(data, "assertr_defect")))
    attr(data, "assertr_defect") <- list()
  attr(data, "assertr_defect") <- append(attr(data, "assertr_defect"), list(this_defect))
  return(data)
}

#' @export
#' @rdname success_and_error_functions
defect_report <- function(errors, data, ...){
  n_assertions <- length(attr(data, "assertr_defect"))
  if(is.null(attr(data, "assertr_in_chain_success_fun_override"))) {
    # single verification case
    data <- defect_append(errors, data, ...)
    print(attr(data, "assertr_defect")[[1]])
  } else {
    # chain_end case
    if (n_assertions==0) {
      cat("No rules run on defective data.")
      return(data)
    }
    result <- "assertion"
    if (n_assertions>1)
      result <- "assertions"
    cat(n_assertions, result, "omitted:", "\n")
    lapply(attr(data, "assertr_defect"), print)
    attr(data, "assertr_in_chain_success_fun_override") <- NULL
  }
  attr(data, "assertr_defect") <- NULL
  return(invisible(data))
}

#' @export
#' @rdname success_and_error_functions
defect_df_return <- function(errors, data, ...){
  n_assertions <- length(attr(data, "assertr_defect"))
  defect_to_df <- function(defect) {
    if (!identical(defect$columns, NA))
      defect$columns <- paste0(defect$columns, collapse = ", ")
    data.frame(
      verb = defect$verb,
      message = defect$message,
      call = defect$call,
      columns = defect$columns,
      row_redux_call = defect$row_redux_call,
      description = defect$description,
      stringsAsFactors = FALSE
    )
  }
  if(is.null(attr(data, "assertr_in_chain_success_fun_override"))) {
    # single verification case
    data <- defect_append(errors, data, ...)
    defect_df <- defect_to_df(attr(data, "assertr_defect")[[1]])
  } else {
    # chain_end case
    if (n_assertions==0) {
      stop("No rules run on defective data.")
    }
    defect_df <- do.call(rbind, lapply(attr(data, "assertr_defect"), defect_to_df))
    attr(data, "assertr_in_chain_success_fun_override") <- NULL
  }
  return(defect_df)
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
chain_start <- function(data, store_success = FALSE){
  attr(data, "assertr_in_chain_success_fun_override") <- success_continue
  if (store_success)
    attr(data, "assertr_in_chain_success_fun_override") <- success_append
  attr(data, "assertr_in_chain_error_fun_override") <- error_append
  return(data)
}

#' @param data A data frame
#' @param store_success If TRUE each successful assertion is stored in chain.
#' @param success_fun Function to call if assertion passes. Defaults to
#'                    returning \code{data}.
#' @param error_fun Function to call if assertion fails. Defaults to printing
#'                  a summary of all errors.
#' @export
#' @rdname chaining_functions
chain_end <- function(data, success_fun=success_continue,
                      error_fun=error_report){
  list_of_errors <- attr(data, "assertr_errors")
  attr(data, "assertr_in_chain_error_fun_override") <- NULL
  attr(data, "assertr_errors") <- NULL
  if(is.null(list_of_errors))
    return(success_fun(data))
  error_fun(list_of_errors, data=data)
}
