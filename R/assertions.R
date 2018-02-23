
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
#' @param success_fun Function to call if assertion passes. Defaults to
#'                    returning \code{data}.
#' @param error_fun Function to call if assertion fails. Defaults to printing
#'                  a summary of all errors.
#'
#' @details For examples of possible choices for the \code{success_fun} and
#' \code{error_fun} parameters, run \code{help("success_and_error_functions")}
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
#' # return mtcars
#' assert(mtcars, not_na, mpg:carb)
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
                      error_fun=error_stop, title = NULL){
  keeper.vars <- dplyr::quos(...)
  sub.frame <- dplyr::select(data, rlang::UQS(keeper.vars))
  validation_id <- generate_id()
  name.of.predicate <- lazyeval::expr_text(predicate)
  if(!is.null(attr(predicate, "call"))){
    name.of.predicate <- attr(predicate, "call")
  }

  success_fun_override <- attr(data, "assertr_in_chain_success_fun_override")
  if(!is.null(success_fun_override)){
    if(!identical(success_fun, success_fun_override))
      # warning("user defined success_fun overridden by assertr chain")
    success_fun <- success_fun_override
  }
  error_fun_override <- attr(data, "assertr_in_chain_error_fun_override")
  if(!is.null(error_fun_override)){
    if(!identical(error_fun, error_fun_override))
      # warning("user defined error_fun overriden by assertr chain")
    error_fun <- error_fun_override
  }

  if(!is.null(title) && is.null(attr(data, "assertr_results")))
    attr(data, "assertr_results") <- list()

  if(!is.vectorized.predicate(predicate))
    predicate <- make.predicate.proper(predicate)


  log.mat <- sapply(names(sub.frame),
                    function(column){
                      this.vector <- sub.frame[[column]]
                      return(apply.predicate.to.vector(this.vector,
                                                       predicate))})

  if (!is.null(title)) {
    result <- all(log.mat)
    attr(data, "assertr_results") <- append(
      attr(data, "assertr_results"),
      list(data.frame(title = title, result = result, validation_id = validation_id, stringsAsFactors = FALSE)))
  }

  # if all checks pass *and* there are no leftover errors
  if(all(log.mat) && is.null(attr(data, "assertr_errors")))
    return(success_fun(data))


  errors <- lapply(colnames(log.mat), function(col.name){
    col <- log.mat[, col.name]
    num.violations <- sum(!col)
    if(num.violations==0)
      return(NULL)
    index.of.violations <- which(!col)
    offending.elements <- sub.frame[[col.name]][index.of.violations]
    an_error <- make.assertr.assert.error(name.of.predicate,
                                          col.name,
                                          num.violations,
                                          index.of.violations,
                                          offending.elements,
                                          validation_id)
    return(an_error)
  })

  # remove the elements corresponding to the columns without errors
  errors <- Filter(function(x) !is.null(x), errors)
  error_fun(errors, data=data)
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
#' @param success_fun Function to call if assertion passes. Defaults to
#'                    returning \code{data}.
#' @param error_fun Function to call if assertion fails. Defaults to printing
#'                  a summary of all errors.
#'
#' @details For examples of possible choices for the \code{success_fun} and
#' \code{error_fun} parameters, run \code{help("success_and_error_functions")}
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
                         error_fun=error_stop, title = NULL){
  keeper.vars <- dplyr::quos(...)
  sub.frame <- dplyr::select(data, rlang::UQS(keeper.vars))
  name.of.row.redux.fn <- lazyeval::expr_text(row_reduction_fn)
  name.of.predicate <- lazyeval::expr_text(predicate)
  validation_id <- generate_id()
  if(!is.null(attr(row_reduction_fn, "call"))){
    name.of.row.redux.fn <- attr(row_reduction_fn, "call")
  }
  if(!is.null(attr(predicate, "call"))){
    name.of.predicate <- attr(predicate, "call")
  }

  success_fun_override <- attr(data, "assertr_in_chain_success_fun_override")
  if(!is.null(success_fun_override)){
    if(!identical(success_fun, success_fun_override))
      # warning("user defined success_fun overridden by assertr chain")
    success_fun <- success_fun_override
  }
  error_fun_override <- attr(data, "assertr_in_chain_error_fun_override")
  if(!is.null(error_fun_override)){
    if(!identical(error_fun, error_fun_override))
      # warning("user defined error_fun overriden by assertr chain")
    error_fun <- error_fun_override
  }

  if(!is.null(title) && is.null(attr(data, "assertr_results")))
    attr(data, "assertr_results") <- list()

  if(!is.vectorized.predicate(predicate))
    predicate <- make.predicate.proper(predicate)

  redux <- row_reduction_fn(sub.frame)

  log.vec <- apply.predicate.to.vector(redux, predicate)

  if (!is.null(title)) {
    result <- all(log.vec)
    attr(data, "assertr_results") <- append(
      attr(data, "assertr_results"),
      list(data.frame(title = title, result = result, validation_id = validation_id, stringsAsFactors = FALSE)))
  }

  # if all checks pass *and* there are no leftover errors
  if(all(log.vec) && is.null(attr(data, "assertr_errors")))
    return(success_fun(data))

  num.violations <- sum(!log.vec)
  if(num.violations==0)
    # There are errors, just no new ones, so calling success
    # is inappropriate, so we must call the error function.
    # NOT calling either function would break the pipeline.
    return(error_fun(list(), data=data))
  loc.violations <- which(!log.vec)

  error <- make.assertr.assert_rows.error(name.of.row.redux.fn,
                                          name.of.predicate,
                                          num.violations,
                                          loc.violations,
                                          validation_id)
  error_fun(list(error), data=data)

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
#' @param success_fun Function to call if assertion passes. Defaults to
#'                    returning \code{data}.
#' @param error_fun Function to call if assertion fails. Defaults to printing
#'                  a summary of all errors.
#'
#' @details For examples of possible choices for the \code{success_fun} and
#' \code{error_fun} parameters, run \code{help("success_and_error_functions")}
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
                    error_fun=error_stop, title = NULL){
  keeper.vars <- dplyr::quos(...)
  sub.frame <- dplyr::select(data, rlang::UQS(keeper.vars))
  name.of.predicate.generator <- lazyeval::expr_text(predicate_generator)
  validation_id <- generate_id()
  if(!is.null(attr(predicate_generator, "call"))){
    name.of.predicate.generator <- attr(predicate_generator, "call")
  }

  success_fun_override <- attr(data, "assertr_in_chain_success_fun_override")
  if(!is.null(success_fun_override)){
    if(!identical(success_fun, success_fun_override))
      # warning("user defined success_fun overridden by assertr chain")
    success_fun <- success_fun_override
  }
  error_fun_override <- attr(data, "assertr_in_chain_error_fun_override")
  if(!is.null(error_fun_override)){
    if(!identical(error_fun, error_fun_override))
      # warning("user defined error_fun overriden by assertr chain")
    error_fun <- error_fun_override
  }

  if(!is.null(title) && is.null(attr(data, "assertr_results")))
    attr(data, "assertr_results") <- list()

  # get true predicates (not the generator)
  true.predicates <- sapply(names(sub.frame),
                            function(column){predicate_generator(sub.frame[[column]])})

  log.mat <- sapply(names(sub.frame),
                    function(column){
                      this.vector <- sub.frame[[column]]
                      predicate <- true.predicates[[column]]
                      return(apply.predicate.to.vector(this.vector,
                                                       predicate))})

  if (!is.null(title)) {
    result <- all(log.mat)
    attr(data, "assertr_results") <- append(
      attr(data, "assertr_results"),
      list(data.frame(title = title, result = result, validation_id = validation_id, stringsAsFactors = FALSE)))
  }

  # if all checks pass *and* there are no leftover errors
  if(all(log.mat) && is.null(attr(data, "assertr_errors")))
    return(success_fun(data))

  errors <- lapply(colnames(log.mat), function(col.name){
    col <- log.mat[, col.name]
    num.violations <- sum(!col)
    if(num.violations==0)
      return(NULL)
    index.of.violations <- which(!col)
    offending.elements <- sub.frame[[col.name]][index.of.violations]
    an_error <- make.assertr.assert.error(name.of.predicate.generator,
                                          col.name,
                                          num.violations,
                                          index.of.violations,
                                          offending.elements,
                                          validation_id)
    return(an_error)
  })

  # remove the elements corresponding to the columns without errors
  errors <- Filter(function(x) !is.null(x), errors)

  error_fun(errors, data=data)
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
#' @param success_fun Function to call if assertion passes. Defaults to
#'                    returning \code{data}.
#' @param error_fun Function to call if assertion fails. Defaults to printing
#'                  a summary of all errors.
#'
#' @details For examples of possible choices for the \code{success_fun} and
#' \code{error_fun} parameters, run \code{help("success_and_error_functions")}
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
                         error_fun=error_stop, title = NULL){
  keeper.vars <- dplyr::quos(...)
  sub.frame <- dplyr::select(data, rlang::UQS(keeper.vars))
  name.of.row.redux.fn <- lazyeval::expr_text(row_reduction_fn)
  name.of.predicate.generator <- lazyeval::expr_text(predicate_generator)
  validation_id <- generate_id()
  if(!is.null(attr(row_reduction_fn, "call"))){
    name.of.row.redux.fn <- attr(row_reduction_fn, "call")
  }
  if(!is.null(attr(predicate_generator, "call"))){
    name.of.predicate.generator <- attr(predicate_generator, "call")
  }

  success_fun_override <- attr(data, "assertr_in_chain_success_fun_override")
  if(!is.null(success_fun_override)){
    if(!identical(success_fun, success_fun_override))
      # warning("user defined success_fun overridden by assertr chain")
    success_fun <- success_fun_override
  }
  error_fun_override <- attr(data, "assertr_in_chain_error_fun_override")
  if(!is.null(error_fun_override)){
    if(!identical(error_fun, error_fun_override))
      # warning("user defined error_fun overriden by assertr chain")
    error_fun <- error_fun_override
  }

  if(!is.null(title) && is.null(attr(data, "assertr_results")))
    attr(data, "assertr_results") <- list()

  redux <- row_reduction_fn(sub.frame)

  predicate <- predicate_generator(redux)

  log.vec <- apply.predicate.to.vector(redux, predicate)

  if (!is.null(title)) {
    result <- all(log.vec)
    attr(data, "assertr_results") <- append(
      attr(data, "assertr_results"),
      list(data.frame(title = title, result = result, validation_id = validation_id, stringsAsFactors = FALSE)))
  }

  # if all checks pass *and* there are no leftover errors
  if(all(log.vec) && is.null(attr(data, "assertr_errors")))
    return(success_fun(data))

  num.violations <- sum(!log.vec)
  if(num.violations==0)
    # There are errors, just no new ones, so calling success
    # is inappropriate, so we must call the error function.
    # NOT calling either function would break the pipeline.
    return(error_fun(list(), data=data))
  loc.violations <- which(!log.vec)

  error <- make.assertr.assert_rows.error(name.of.row.redux.fn,
                                          name.of.predicate.generator,
                                          num.violations,
                                          loc.violations)
  error_fun(list(error), data=data)
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
#' @details For examples of possible choices for the \code{success_fun} and
#' \code{error_fun} parameters, run \code{help("success_and_error_functions")}
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
                   error_fun=error_stop, title = NULL){
  expr <- substitute(expr)
  # conform to terminology from subset
  envir <- data
  enclos <- parent.frame()
  logical.results <- eval(expr, envir, enclos)
  validation_id <- generate_id()

  success_fun_override <- attr(data, "assertr_in_chain_success_fun_override")
  if(!is.null(success_fun_override)){
    if(!identical(success_fun, success_fun_override))
      # warning("user defined success_fun overridden by assertr chain")
    success_fun <- success_fun_override
  }
  error_fun_override <- attr(data, "assertr_in_chain_error_fun_override")
  if(!is.null(error_fun_override)){
    if(!identical(error_fun, error_fun_override))
      # warning("user defined error_fun overriden by assertr chain")
    error_fun <- error_fun_override
  }

  if(!is.null(title) && is.null(attr(data, "assertr_results")))
    attr(data, "assertr_results") <- list()

  if (!is.null(title)) {
    result <- all(logical.results)
    attr(data, "assertr_results") <- append(
      attr(data, "assertr_results"),
      list(data.frame(title = title, result = result, validation_id = validation_id, stringsAsFactors = FALSE)))
  }

  # if all checks pass *and* there are no leftover errors
  if(all(logical.results) && is.null(attr(data, "assertr_errors")))
    return(success_fun(data))
  num.violations <- sum(!logical.results)
  if(num.violations==0) return(error_fun(list(), data=data))
  error <- make.assertr.verify.error(num.violations, paste(deparse(expr), collapse = " "), validation_id)
  error_fun(list(error), data=data)
}
