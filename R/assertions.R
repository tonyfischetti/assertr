
##
##  provides different assertion functions
##


#' Raises error if predicate is FALSE in any columns selected
#'
#' Meant for use in a data analysis pipeline, this function will
#' just return the data it's supplied if there are no FALSEs
#' when the predicate is applied to every element of the columns
#' indicated. If any element in any of the columns, when applied
#' to the predicate, is FALSE, then this function will raise an
#' error, effectively terminating the pipeline early.
#'
#' @param data A data frame
#' @param predicate A function that returns FALSE when violated
#' @param ... Comma separated list of unquoted expressions.
#'            Uses dplyr's \code{select} to select
#'            columns from data.
#' @param .dots Use assert_() to select columns using standard evaluation.
#' @param success_fun Function to call if assertion passes. Defaults to
#'                    returning \code{data}.
#' @param error_fun Function to call if assertion fails. Defaults to printing
#'                  a summary of all errors.
#'
#' @details The behavior of this function when the assertion passes or fails
#'          is configurable via the \code{success_fun} and \code{error_fun}
#'          parameters, respectively.
#'          The \code{success_fun} parameter takes a function that takes
#'          the data passed to the assertion function as a parameter. You can
#'          write your own success handler function, but there are two
#'          provided by this package:
#'          \enumerate{
#'            \item \code{success_continue} - just returns the data that was
#'                  passed into the assertion function
#'            \item \code{success_logical} - returns TRUE
#'          }
#'          The \code{error_fun} parameter takes a function that takes
#'          the data passed to the assertion function as a parameter. You can
#'          write your own success handler function, but there are two
#'          provided by this package:
#'          \enumerate{
#'            \item \code{error_stop} - Prints a summary of the errors and
#'            halts execution.
#'            \item \code{error_report} - Prints all the information available
#'            about the errors and halts execution.
#'            \item \code{error_append} - Attaches the errors to a special
#'            attribute of \code{data} and returns the data. This is chiefly
#'            to allow assertr errors to be accumulated in a pipeline so that
#'            all assertions can have a chance to be checked and so that all
#'            the errors can be displayed at the end of the chain.
#'            \item \code{error_logical} - returns FALSE
#'          }
#'
#' @return By default, the \code{data} is returned if predicate assertion
#'         is TRUE and and error is thrown if not. If a non-default
#'         \code{success_fun} or \code{error_fun} is used, the return
#'         values of these function will be returned.
#' @note See \code{vignette("assertr")} for how to use this in context
#' @seealso \code{\link{verify}} \code{\link{insist}}
#'          \code{\link{assert_rows}} \code{\link{insist_rows}}
#' @examples
#'
#' # returns mtcars
#' assert(mtcars, not_na, vs)
#'
#' # equivalent statements using standard evaluation
#' assert_(mtcars, not_na, "vs")
#' var <- "vs"
#' assert_(mtcars, not_na, var)
#'
#' # return mtcars
#' assert(mtcars, not_na, mpg:carb)
#'
#' # equivalent using standard evaluation
#' assert_(mtcars, not_na, "mpg:carb")
#'
#'
#' library(magrittr)                    # for piping operator
#'
#' mtcars %>%
#'   assert(in_set(c(0,1)), vs)
#'   # anything here will run
#'
#' \dontrun{
#' mtcars %>%
#'   assert(in_set(c(1, 2, 3, 4, 6)), carb)
#'   # the assertion is untrue so
#'   # nothing here will run}
#'
#' @export
assert <- function(data, predicate, ..., success_fun=success_continue,
                   error_fun=error_stop){
  assert_(data, predicate, .dots = lazyeval::lazy_dots(...),
          success_fun=success_fun,
          error_fun = error_fun)
}

#' @export
#' @rdname assert
assert_ <- function(data, predicate, ..., .dots, success_fun=success_continue,
                      error_fun=error_stop){
  sub.frame <- dplyr::select_(data, ..., .dots = .dots)
  if(!is.null(attr(predicate, "call"))){
    name.of.predicate <- attr(predicate, "call")
  }
  else {
    name.of.predicate <- deparse(substitute(predicate))
    if(length(name.of.predicate)>1)
      name.of.predicate <- gsub("\\s{2,}", " ",
                                paste0(name.of.predicate, collapse=""))
  }

  if(!is.vectorized.predicate(predicate))
    predicate <- make.predicate.proper(predicate)


  log.mat <- sapply(names(sub.frame),
                    function(column){
                      this.vector <- sub.frame[[column]]
                      return(apply.predicate.to.vector(this.vector,
                                                       predicate))})

  # if all checks pass *and* there are no leftover errors
  if(all(log.mat) && is.null(attr(data, "assertr_errors")))
    return(success_fun(data))

  messages <- sapply(colnames(log.mat), function(col.name){
    col <- log.mat[, col.name]
    num.violations <- sum(!col)
    if(num.violations==0)
      return("")
    index.of.first.violation <- which(!col)[1]
    offending.element <- sub.frame[[col.name]][index.of.first.violation]
    make.assert.error.message(name.of.predicate, col.name, num.violations,
                              index.of.first.violation, offending.element)
  })

  messages <- paste0(messages[messages!=""], collapse = '')
  error_fun(messages)
}



#' Raises error if predicate is FALSE for any row after applying
#' row reduction function
#'
#' Meant for use in a data analysis pipeline, this function applies a
#' function to a data frame that reduces each row to a single value. Then,
#' a predicate function is applied to each of the row reduction values. If
#' any of these predicate applications yield FALSE, this function will raise
#' an error, effectively terminating the pipeline early. If there are no
#' FALSEs, this function will just return the data that it was supplied for
#' further use in later parts of the pipeline.
#'
#' @param data A data frame
#' @param row_reduction_fn A function that returns a value for each row of
#'                          the provided data frame
#' @param predicate A function that returns FALSE when violated
#' @param ... Comma separated list of unquoted expressions.
#'            Uses dplyr's \code{select} to select
#'            columns from data.
#' @param .dots Use assert_rows_() to select columns using standard evaluation.
#' @param success_fun Function to call if assertion passes. Defaults to
#'                    returning \code{data}.
#' @param error_fun Function to call if assertion fails. Defaults to printing
#'                  a summary of all errors.
#'
#' @details The behavior of this function when the assertion passes or fails
#'          is configurable via the \code{success_fun} and \code{error_fun}
#'          parameters, respectively.
#'          The \code{success_fun} parameter takes a function that takes
#'          the data passed to the assertion function as a parameter. You can
#'          write your own success handler function, but there are two
#'          provided by this package:
#'          \enumerate{
#'            \item \code{success_continue} - just returns the data that was
#'                  passed into the assertion function
#'            \item \code{success_logical} - returns TRUE
#'          }
#'          The \code{error_fun} parameter takes a function that takes
#'          the data passed to the assertion function as a parameter. You can
#'          write your own success handler function, but there are two
#'          provided by this package:
#'          \enumerate{
#'            \item \code{error_stop} - Prints a summary of the errors and
#'            halts execution.
#'            \item \code{error_report} - Prints all the information available
#'            about the errors and halts execution.
#'            \item \code{error_append} - Attaches the errors to a special
#'            attribute of \code{data} and returns the data. This is chiefly
#'            to allow assertr errors to be accumulated in a pipeline so that
#'            all assertions can have a chance to be checked and so that all
#'            the errors can be displayed at the end of the chain.
#'            \item \code{error_logical} - returns FALSE
#'          }
#'
#' @return By default, the \code{data} is returned if predicate assertion
#'         is TRUE and and error is thrown if not. If a non-default
#'         \code{success_fun} or \code{error_fun} is used, the return
#'         values of these function will be returned.
#' @note See \code{vignette("assertr")} for how to use this in context
#' @seealso \code{\link{insist_rows}} \code{\link{assert}}
#'          \code{\link{verify}} \code{\link{insist}}
#'
#' @examples
#'
#' # returns mtcars
#' assert_rows(mtcars, num_row_NAs, within_bounds(0,2), mpg:carb)
#'
#' # equivalent using standard evaluation
#' assert_rows_(mtcars, num_row_NAs, within_bounds(0,2), "mpg:carb")
#'
#'
#' library(magrittr)                    # for piping operator
#'
#' mtcars %>%
#'   assert_rows(rowSums, within_bounds(0,2), vs:am)
#'   # anything here will run
#'
#' \dontrun{
#' mtcars %>%
#'   assert_rows(rowSums, within_bounds(0,1), vs:am)
#'   # the assertion is untrue so
#'   # nothing here will run}
#'
#' @export
#'
assert_rows <- function(data, row_reduction_fn, predicate, ...,
                        success_fun=success_continue,
                        error_fun=error_stop){
  assert_rows_(data, row_reduction_fn, predicate,
               .dots = lazyeval::lazy_dots(...),
               success_fun = success_fun,
               error_fun = error_fun)
}

#' @export
#' @rdname assert_rows
assert_rows_ <- function(data, row_reduction_fn, predicate, ..., .dots,
                         success_fun=success_continue,
                         error_fun=error_stop){
  sub.frame <- dplyr::select_(data, ..., .dots = .dots)
  if(!is.null(attr(predicate, "call"))){
    name.of.predicate <- attr(predicate, "call")
  }
  else {
    name.of.predicate <- deparse(substitute(predicate))
    if(length(name.of.predicate)>1)
      name.of.predicate <- gsub("\\s{2,}", " ",
                                paste0(name.of.predicate, collapse=""))
  }

  if(!is.vectorized.predicate(predicate))
    predicate <- make.predicate.proper(predicate)

  redux <- row_reduction_fn(sub.frame)

  log.vec <- apply.predicate.to.vector(redux, predicate)

  # if all checks pass *and* there are no leftover errors
  if(all(log.vec) && is.null(attr(data, "assertr_errors")))
    return(success_fun(data))

  num.violations <- sum(!log.vec)
  if(num.violations==0)
    return("")
  loc.violations <- which(!log.vec)

  message <- make.assert_rows.error.message(name.of.predicate, num.violations,
                                            loc.violations)
  error_fun(message)
}



#' Raises error if dynamically created predicate is FALSE in any columns selected
#'
#' Meant for use in a data analysis pipeline, this function applies a predicate
#' generating function to each of the columns indicated. It will then use these
#' predicates to check every element of those columns. If any of these
#' predicate applications yield FALSE, this function will raise an error,
#' effectively terminating the pipeline early. If there are no FALSES, this
#' function will just return the data that it was supplied for further use in
#' later parts of the pipeline.
#'
#' @param data A data frame
#' @param predicate_generator A function that is applied
#'        to each of the column vectors selected. This will produce,
#'        for every column, a true predicate function to be applied to
#'        every element in the column vectors selected
#' @param ... Comma separated list of unquoted expressions.
#'            Uses dplyr's \code{select} to select
#'            columns from data.
#' @param .dots Use insist_() to select columns using standard evaluation.
#' @param success_fun Function to call if assertion passes. Defaults to
#'                    returning \code{data}.
#' @param error_fun Function to call if assertion fails. Defaults to printing
#'                  a summary of all errors.
#'
#' @details The behavior of this function when the assertion passes or fails
#'          is configurable via the \code{success_fun} and \code{error_fun}
#'          parameters, respectively.
#'          The \code{success_fun} parameter takes a function that takes
#'          the data passed to the assertion function as a parameter. You can
#'          write your own success handler function, but there are two
#'          provided by this package:
#'          \enumerate{
#'            \item \code{success_continue} - just returns the data that was
#'                  passed into the assertion function
#'            \item \code{success_logical} - returns TRUE
#'          }
#'          The \code{error_fun} parameter takes a function that takes
#'          the data passed to the assertion function as a parameter. You can
#'          write your own success handler function, but there are two
#'          provided by this package:
#'          \enumerate{
#'            \item \code{error_stop} - Prints a summary of the errors and
#'            halts execution.
#'            \item \code{error_report} - Prints all the information available
#'            about the errors and halts execution.
#'            \item \code{error_append} - Attaches the errors to a special
#'            attribute of \code{data} and returns the data. This is chiefly
#'            to allow assertr errors to be accumulated in a pipeline so that
#'            all assertions can have a chance to be checked and so that all
#'            the errors can be displayed at the end of the chain.
#'            \item \code{error_logical} - returns FALSE
#'          }
#'
#' @return By default, the \code{data} is returned if dynamically created
#'         predicate assertion is TRUE and and error is thrown if not. If a
#'         non-default \code{success_fun} or \code{error_fun} is used, the
#'         return values of these function will be returned.
#' @note See \code{vignette("assertr")} for how to use this in context
#' @seealso \code{\link{assert}} \code{\link{verify}} \code{\link{insist_rows}}
#'          \code{\link{assert_rows}}
#' @examples
#'
#' insist(iris, within_n_sds(3), Sepal.Length)   # returns iris
#'
#' # equivalent using standard evaluation
#' insist_(iris, within_n_sds(3), "Sepal.Length")
#'
#' library(magrittr)
#'
#' iris %>%
#'   insist(within_n_sds(4), Sepal.Length:Petal.Width)
#'   # anything here will run
#'
#' \dontrun{
#' iris %>%
#'   insist(within_n_sds(3), Sepal.Length:Petal.Width)
#'   # datum at index 16 of 'Sepal.Width' vector is (4.4)
#'   # is outside 3 standard deviations from the mean of Sepal.Width.
#'   # The check fails, raises a fatal error, and the pipeline
#'   # is terminated so nothing after this statement will run}
#'
#' @export
insist <- function(data, predicate_generator, ...,
                   success_fun=success_continue,
                   error_fun=error_stop){
  insist_(data, predicate_generator, .dots = lazyeval::lazy_dots(...),
          success_fun=success_fun,
          error_fun = error_fun)
}

#' @export
#' @rdname insist
insist_ <- function(data, predicate_generator, ..., .dots,
                    success_fun=success_continue,
                    error_fun=error_stop){
  sub.frame <- dplyr::select_(data, ..., .dots = .dots)
  if(!is.null(attr(predicate_generator, "call"))){
    name.of.predicate.generator <- attr(predicate_generator, "call")
  }
  else {
    name.of.predicate.generator <- deparse(substitute(predicate_generator))
    if(length(name.of.predicate.generator)>1)
      name.of.predicate.generator <- gsub("\\s{2,}", " ",
                                          paste0(name.of.predicate.generator,
                                                 collapse=""))
  }

  # get true predicates (not the generator)
  true.predicates <- sapply(names(sub.frame),
                            function(column){predicate_generator(sub.frame[[column]])})

  log.mat <- sapply(names(sub.frame),
                    function(column){
                      this.vector <- sub.frame[[column]]
                      predicate <- true.predicates[[column]]
                      return(apply.predicate.to.vector(this.vector,
                                                       predicate))})

  # if all checks pass *and* there are no leftover errors
  if(all(log.mat) && is.null(attr(data, "assertr_errors")))
    return(success_fun(data))

  messages <- sapply(colnames(log.mat), function(col.name){
    col <- log.mat[, col.name]
    num.violations <- sum(!col)
    if(num.violations==0)
      return("")
    index.of.first.violation <- which(!col)[1]
    offending.element <- sub.frame[[col.name]][index.of.first.violation]
    make.assert.error.message(name.of.predicate.generator, col.name,
                              num.violations, index.of.first.violation,
                              offending.element)
  })

  messages <- paste0(messages[messages!=""], collapse = '')
  error_fun(messages)
}



#' Raises error if dynamically created predicate is FALSE for any row
#' after applying row reduction function
#'
#' Meant for use in a data analysis pipeline, this function applies a
#' function to a data frame that reduces each row to a single value. Then,
#' a predicate generating function is applied to row reduction values. It will
#' then use these predicates to check each of the row reduction values. If any
#' of these predicate applications yield FALSE, this function will raise
#' an error, effectively terminating the pipeline early. If there are no
#' FALSEs, this function will just return the data that it was supplied for
#' further use in later parts of the pipeline.
#'
#' @param data A data frame
#' @param row_reduction_fn A function that returns a value for each row of
#'                          the provided data frame
#' @param predicate_generator A function that is applied to the results of
#'                            the row reduction function. This will produce,
#'                            a true predicate function to be applied to every
#'                            element in the vector that the row reduction
#'                            function returns.
#' @param ... Comma separated list of unquoted expressions.
#'            Uses dplyr's \code{select} to select
#'            columns from data.
#' @param .dots Use insist_rows_() to select columns using standard evaluation.
#' @param success_fun Function to call if assertion passes. Defaults to
#'                    returning \code{data}.
#' @param error_fun Function to call if assertion fails. Defaults to printing
#'                  a summary of all errors.
#'
#' @details The behavior of this function when the assertion passes or fails
#'          is configurable via the \code{success_fun} and \code{error_fun}
#'          parameters, respectively.
#'          The \code{success_fun} parameter takes a function that takes
#'          the data passed to the assertion function as a parameter. You can
#'          write your own success handler function, but there are two
#'          provided by this package:
#'          \enumerate{
#'            \item \code{success_continue} - just returns the data that was
#'                  passed into the assertion function
#'            \item \code{success_logical} - returns TRUE
#'          }
#'          The \code{error_fun} parameter takes a function that takes
#'          the data passed to the assertion function as a parameter. You can
#'          write your own success handler function, but there are two
#'          provided by this package:
#'          \enumerate{
#'            \item \code{error_stop} - Prints a summary of the errors and
#'            halts execution.
#'            \item \code{error_report} - Prints all the information available
#'            about the errors and halts execution.
#'            \item \code{error_append} - Attaches the errors to a special
#'            attribute of \code{data} and returns the data. This is chiefly
#'            to allow assertr errors to be accumulated in a pipeline so that
#'            all assertions can have a chance to be checked and so that all
#'            the errors can be displayed at the end of the chain.
#'            \item \code{error_logical} - returns FALSE
#'          }
#'
#' @return By default, the \code{data} is returned if dynamically created
#'         predicate assertion is TRUE and and error is thrown if not. If a
#'         non-default \code{success_fun} or \code{error_fun} is used, the
#'         return values of these function will be returned.
#' @note See \code{vignette("assertr")} for how to use this in context
#' @seealso \code{\link{insist}} \code{\link{assert_rows}}
#'          \code{\link{assert}} \code{\link{verify}}
#' @examples
#'
#' # returns mtcars
#' insist_rows(mtcars, maha_dist, within_n_mads(30), mpg:carb)
#'
#' # equivalent using standard evaluation
#' insist_rows_(mtcars, maha_dist, within_n_mads(30), "mpg:carb")
#'
#'
#' library(magrittr)                    # for piping operator
#'
#' mtcars %>%
#'   insist_rows(maha_dist, within_n_mads(10), vs:am)
#'   # anything here will run
#'
#' \dontrun{
#' mtcars %>%
#'   insist_rows(maha_dist, within_n_mads(1), everything())
#'   # the assertion is untrue so
#'   # nothing here will run}
#'
#' @export
#'
insist_rows <- function(data, row_reduction_fn, predicate_generator, ...,
                        success_fun=success_continue,
                        error_fun=error_stop){
  insist_rows_(data, row_reduction_fn, predicate_generator,
               .dots = lazyeval::lazy_dots(...),
               success_fun=success_fun, error_fun = error_fun)
}

#' @export
#' @rdname insist_rows
insist_rows_ <- function(data, row_reduction_fn, predicate_generator, ...,
                         .dots, success_fun=success_continue,
                         error_fun=error_stop){
  sub.frame <- dplyr::select_(data, ..., .dots = .dots)
  if(!is.null(attr(predicate_generator, "call"))){
    name.of.predicate.generator <- attr(predicate_generator, "call")
  }
  else {
    name.of.predicate.generator <- deparse(substitute(predicate_generator))
    if(length(name.of.predicate.generator)>1)
      name.of.predicate.generator <- gsub("\\s{2,}", " ",
                                          paste0(name.of.predicate.generator,
                                                 collapse=""))
  }

  redux <- row_reduction_fn(sub.frame)

  predicate <- predicate_generator(redux)

  log.vec <- apply.predicate.to.vector(redux, predicate)

  # if all checks pass *and* there are no leftover errors
  if(all(log.vec) && is.null(attr(data, "assertr_errors")))
    return(success_fun(data))

  num.violations <- sum(!log.vec)
  if(num.violations==0)
    return("")
  loc.violations <- which(!log.vec)

  message <- make.assert_rows.error.message(name.of.predicate.generator,
                                            num.violations,loc.violations)
  error_fun(message)
}





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
#' @param success_fun Function to call if assertion passes. Defaults to
#'                    returning \code{data}.
#' @param error_fun Function to call if assertion fails. Defaults to printing
#'                  a summary of all errors.
#'
#' @details The behavior of this function when the assertion passes or fails
#'          is configurable via the \code{success_fun} and \code{error_fun}
#'          parameters, respectively.
#'          The \code{success_fun} parameter takes a function that takes
#'          the data passed to the assertion function as a parameter. You can
#'          write your own success handler function, but there are two
#'          provided by this package:
#'          \enumerate{
#'            \item \code{success_continue} - just returns the data that was
#'                  passed into the assertion function
#'            \item \code{success_logical} - returns TRUE
#'          }
#'          The \code{error_fun} parameter takes a function that takes
#'          the data passed to the assertion function as a parameter. You can
#'          write your own success handler function, but there are two
#'          provided by this package:
#'          \enumerate{
#'            \item \code{error_stop} - Prints a summary of the errors and
#'            halts execution.
#'            \item \code{error_report} - Prints all the information available
#'            about the errors and halts execution.
#'            \item \code{error_append} - Attaches the errors to a special
#'            attribute of \code{data} and returns the data. This is chiefly
#'            to allow assertr errors to be accumulated in a pipeline so that
#'            all assertions can have a chance to be checked and so that all
#'            the errors can be displayed at the end of the chain.
#'            \item \code{error_logical} - returns FALSE
#'          }
#'
#' @return By default, the \code{data} is returned if predicate assertion
#'         is TRUE and and error is thrown if not. If a non-default
#'         \code{success_fun} or \code{error_fun} is used, the return
#'         values of these function will be returned.
#' @note See \code{vignette("assertr")} for how to use this in context
#' @seealso \code{\link{assert}} \code{\link{insist}}
#' @examples
#'
#' verify(mtcars, drat > 2)     # returns mtcars
#' \dontrun{
#' verify(mtcars, drat > 3)     # produces error}
#'
#'
#' library(magrittr)            # for piping operator
#'
#' \dontrun{
#' mtcars %>%
#'   verify(drat > 3) %>%
#'   # anything here will not run}
#'
#' mtcars %>%
#'   verify(nrow(mtcars) > 2)
#'   # anything here will run
#'
#' alist <- list(a=c(1,2,3), b=c(4,5,6))
#' verify(alist, length(a) > 2)
#' verify(alist, length(a) > 2 && length(b) > 2)
#' verify(alist, a > 0 & b > 2)
#'
#' \dontrun{
#' alist %>%
#'   verify(alist, length(a) > 5)
#'   # nothing here will run}
#'
#'
#' @export
verify <- function(data, expr, success_fun=success_continue,
                   error_fun=error_stop){
  expr <- substitute(expr)
  # conform to terminology from subset
  envir <- data
  enclos <- parent.frame()
  logical.results <- eval(expr, envir, enclos)
  # if all checks pass *and* there are no leftover errors
  if(all(logical.results) && is.null(attr(data, "assertr_errors")))
    return(success_fun(data))
  num.violations <- sum(!logical.results)
  error.message <- make.verify.error.message(num.violations)
  error.message <- paste0(error.message, collapse = '')
  error_fun(error.message)
}
