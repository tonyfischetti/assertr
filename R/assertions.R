
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
#' @param skip_chain_opts If TRUE, \code{success_fun} and \code{error_fun}
#'                        are used even if assertion is called within a chain.
#' @param obligatory If TRUE and assertion failed the data is marked as defective.
#'                   For defective data, all the following rules are handled by
#'                   \code{defect_fun} function.
#' @param defect_fun Function to call when data is defective. Defaults to skipping
#'                   assertion and storing info about it in special attribute.
#' @param description Custom description of the rule. Is stored in result
#'                    reports and data.
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
                   error_fun=error_stop, skip_chain_opts=FALSE,
                   obligatory=FALSE, defect_fun=defect_append,
                   description=NA){

  keeper.vars <- dplyr::quos(...)
  name.of.predicate <- rlang::expr_text(rlang::enexpr(predicate))
  if(!is.null(attr(predicate, "call"))){
    name.of.predicate <- attr(predicate, "call")
  }

  success_fun_override <- attr(data, "assertr_in_chain_success_fun_override")
  if(!skip_chain_opts && !is.null(success_fun_override)){
    if(!identical(success_fun, success_fun_override))
      # warning("user defined success_fun overridden by assertr chain")
    success_fun <- success_fun_override
  }
  error_fun_override <- attr(data, "assertr_in_chain_error_fun_override")
  if(!skip_chain_opts && !is.null(error_fun_override)){
    if(!identical(error_fun, error_fun_override))
      # warning("user defined error_fun overriden by assertr chain")
    error_fun <- error_fun_override
  }


  if(!is.vectorized.predicate(predicate))
    predicate <- make.predicate.proper(predicate)

  if(length(keeper.vars)==0)
    stop("assert requires columns to be selected. Check number of arguments", call.=FALSE)

  if(isTRUE(attr(data, "assertr_data_defective")))
    return(defect_fun(
      attr(data, "assertr_errors"), data, "assert", name.of.predicate,
      as.character(dplyr::enexprs(...)), NA, description))

  sub.frame <- dplyr::select(data, !!!(keeper.vars))

  is_successful <- tryCatch(
    expr = {
      res <- predicate(sub.frame)
      length(res) == 1 && isTRUE(res)
    },
    error = function(e){FALSE},
    warning = function(w){FALSE}
  )

  if(is_successful){
    return(success_fun(data, "assert", name.of.predicate, colnames(sub.frame), NA, description))
  }

  log.mat <- sapply(colnames(sub.frame),
                    function(column){
                      this.vector <- sub.frame[[column]]
                      return(apply.predicate.to.vector(this.vector,
                                                       predicate))})

  # if all checks pass in current assertion
  if(all(log.mat))
    return(success_fun(data, "assert", name.of.predicate, colnames(log.mat), NA, description))

  # if errors occurred and verification was obligatory
  if(obligatory)
    attr(data, "assertr_data_defective") <- TRUE

  assertion.id <- generate_id()

  errors <- lapply(colnames(log.mat), function(col.name){
    col <- log.mat[, col.name]
    num.violations <- sum(!col)
    if(num.violations==0)
      return(NULL)
    index.of.violations <- which(!col)
    offending.elements <- sub.frame[[col.name]][index.of.violations]
    an_error <- make.assertr.assert.error("assert",
                                          name.of.predicate,
                                          col.name,
                                          num.violations,
                                          index.of.violations,
                                          offending.elements,
                                          description,
                                          assertion.id)
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
#' @param skip_chain_opts If TRUE, \code{success_fun} and \code{error_fun}
#'                        are used even if assertion is called within a chain.
#' @param obligatory If TRUE and assertion failed the data is marked as defective.
#'                   For defective data, all the following rules are handled by
#'                   \code{defect_fun} function.
#' @param defect_fun Function to call when data is defective. Defaults to skipping
#'                   assertion and storing info about it in special attribute.
#' @param description Custom description of the rule. Is stored in result
#'                    reports and data.
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
                        error_fun=error_stop, skip_chain_opts=FALSE,
                        obligatory=FALSE, defect_fun=defect_append,
                        description=NA){
  keeper.vars <- dplyr::quos(...)
  name.of.row.redux.fn <- rlang::expr_text(rlang::enexpr(row_reduction_fn))
  name.of.predicate <- rlang::expr_text(rlang::enexpr(predicate))
  if(!is.null(attr(row_reduction_fn, "call"))){
    name.of.row.redux.fn <- attr(row_reduction_fn, "call")
  }
  if(!is.null(attr(predicate, "call"))){
    name.of.predicate <- attr(predicate, "call")
  }

  success_fun_override <- attr(data, "assertr_in_chain_success_fun_override")
  if(!skip_chain_opts && !is.null(success_fun_override)){
    if(!identical(success_fun, success_fun_override))
      # warning("user defined success_fun overridden by assertr chain")
    success_fun <- success_fun_override
  }
  error_fun_override <- attr(data, "assertr_in_chain_error_fun_override")
  if(!skip_chain_opts && !is.null(error_fun_override)){
    if(!identical(error_fun, error_fun_override))
      # warning("user defined error_fun overriden by assertr chain")
    error_fun <- error_fun_override
  }

  if(!is.vectorized.predicate(predicate))
    predicate <- make.predicate.proper(predicate)

  if(length(keeper.vars)==0)
    stop("assert_rows requires columns to be selected. Check number of argumentsSelect all columns with everything()", call.=FALSE)

  if(isTRUE(attr(data, "assertr_data_defective")))
    return(defect_fun(
      attr(data, "assertr_errors"), data, "assert_rows", name.of.predicate,
      as.character(dplyr::enexprs(...)), name.of.row.redux.fn, description))

  sub.frame <- dplyr::select(data, !!!(keeper.vars))

  redux <- row_reduction_fn(sub.frame)

  log.vec <- apply.predicate.to.vector(redux, predicate)

  # if all checks pass in current assertion
  if(all(log.vec))
    return(success_fun(
      data, "assert_rows", name.of.predicate, colnames(sub.frame),
      name.of.row.redux.fn, description))

  # if errors occured and verification was obligatory
  if(obligatory)
    attr(data, "assertr_data_defective") <- TRUE

  num.violations <- sum(!log.vec)
  loc.violations <- which(!log.vec)
  offending.elements <- redux[!log.vec]
  assertion.id <- generate_id()

  error <- make.assertr.assert_rows.error("assert_rows",
                                          name.of.row.redux.fn,
                                          name.of.predicate,
                                          as.character(keeper.vars),
                                          num.violations,
                                          loc.violations,
                                          offending.elements,
                                          description,
                                          assertion.id)
  error_fun(list(error), data=data)

}



#' Raises error if dynamically created predicate is FALSE in any columns
#' selected
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
#' @param skip_chain_opts If TRUE, \code{success_fun} and \code{error_fun}
#'                        are used even if assertion is called within a chain.
#' @param obligatory If TRUE and assertion failed the data is marked as defective.
#'                   For defective data, all the following rules are handled by
#'                   \code{defect_fun} function.
#' @param defect_fun Function to call when data is defective. Defaults to skipping
#'                   assertion and storing info about it in special attribute.
#' @param description Custom description of the rule. Is stored in result
#'                    reports and data.
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
                   error_fun=error_stop, skip_chain_opts=FALSE,
                   obligatory=FALSE, defect_fun=defect_append,
                   description=NA){
  keeper.vars <- dplyr::quos(...)
  name.of.predicate.generator <- rlang::expr_text(
      rlang::enexpr(predicate_generator))
  if(!is.null(attr(predicate_generator, "call"))){
    name.of.predicate.generator <- attr(predicate_generator, "call")
  }

  success_fun_override <- attr(data, "assertr_in_chain_success_fun_override")
  if(!skip_chain_opts && !is.null(success_fun_override)){
    if(!identical(success_fun, success_fun_override))
      # warning("user defined success_fun overridden by assertr chain")
    success_fun <- success_fun_override
  }
  error_fun_override <- attr(data, "assertr_in_chain_error_fun_override")
  if(!skip_chain_opts && !is.null(error_fun_override)){
    if(!identical(error_fun, error_fun_override))
      # warning("user defined error_fun overriden by assertr chain")
    error_fun <- error_fun_override
  }

  if(length(keeper.vars)==0)
    stop("insist requires columns to be selected. Check number of arguments", call.=FALSE)

  if(isTRUE(attr(data, "assertr_data_defective")))
    return(defect_fun(
      attr(data, "assertr_errors"), data, "insist",
      name.of.predicate.generator, as.character(dplyr::enexprs(...)), NA, description)
    )

  sub.frame <- dplyr::select(data, !!!(keeper.vars))

  # get true predicates (not the generator)
  true.predicates <- sapply(names(sub.frame),
                            function(column){predicate_generator(sub.frame[[column]])})

  log.mat <- sapply(names(sub.frame),
                    function(column){
                      this.vector <- sub.frame[[column]]
                      predicate <- true.predicates[[column]]
                      return(apply.predicate.to.vector(this.vector,
                                                       predicate))})

  # if all checks pass in current assertion
  if(all(log.mat))
    return(success_fun(data, "insist", name.of.predicate.generator, colnames(sub.frame), NA, description))

  # if errors occured and verification was obligatory
  if(obligatory)
    attr(data, "assertr_data_defective") <- TRUE

  assertion.id <- generate_id()

  errors <- lapply(colnames(log.mat), function(col.name){
    col <- log.mat[, col.name]
    num.violations <- sum(!col)
    if(num.violations==0)
      return(NULL)
    index.of.violations <- which(!col)
    offending.elements <- sub.frame[[col.name]][index.of.violations]
    an_error <- make.assertr.assert.error("insist",
                                          name.of.predicate.generator,
                                          col.name,
                                          num.violations,
                                          index.of.violations,
                                          offending.elements,
                                          description,
                                          assertion.id)
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
#' @param skip_chain_opts If TRUE, \code{success_fun} and \code{error_fun}
#'                        are used even if assertion is called within a chain.
#' @param obligatory If TRUE and assertion failed the data is marked as defective.
#'                   For defective data, all the following rules are handled by
#'                   \code{defect_fun} function.
#' @param defect_fun Function to call when data is defective. Defaults to skipping
#'                   assertion and storing info about it in special attribute.
#' @param description Custom description of the rule. Is stored in result
#'                    reports and data.
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
                        error_fun=error_stop, skip_chain_opts=FALSE,
                        obligatory=FALSE, defect_fun=defect_append,
                        description=NA){
  keeper.vars <- dplyr::quos(...)
  name.of.row.redux.fn <- rlang::expr_text(rlang::enexpr(row_reduction_fn))
  name.of.predicate.generator <- rlang::expr_text(
      rlang::enexpr(predicate_generator))
  if(!is.null(attr(row_reduction_fn, "call"))){
    name.of.row.redux.fn <- attr(row_reduction_fn, "call")
  }
  if(!is.null(attr(predicate_generator, "call"))){
    name.of.predicate.generator <- attr(predicate_generator, "call")
  }

  success_fun_override <- attr(data, "assertr_in_chain_success_fun_override")
  if(!skip_chain_opts && !is.null(success_fun_override)){
    if(!identical(success_fun, success_fun_override))
      # warning("user defined success_fun overridden by assertr chain")
    success_fun <- success_fun_override
  }
  error_fun_override <- attr(data, "assertr_in_chain_error_fun_override")
  if(!skip_chain_opts && !is.null(error_fun_override)){
    if(!identical(error_fun, error_fun_override))
      # warning("user defined error_fun overriden by assertr chain")
    error_fun <- error_fun_override
  }

  if(length(keeper.vars)==0)
    stop("insist_rows requires columns to be selected. Check number of arguments", call.=FALSE)

  if(isTRUE(attr(data, "assertr_data_defective")))
    return(defect_fun(
      attr(data, "assertr_errors"), data, "insist_rows", name.of.predicate.generator,
      as.character(dplyr::enexprs(...)), name.of.row.redux.fn, description)
    )

  sub.frame <- dplyr::select(data, !!!(keeper.vars))

  redux <- row_reduction_fn(sub.frame)

  predicate <- predicate_generator(redux)

  log.vec <- apply.predicate.to.vector(redux, predicate)

  # if all checks pass *and* there are no leftover errors
  if(all(log.vec))
    return(success_fun(
      data, "insist_rows", name.of.predicate.generator,
      colnames(sub.frame), name.of.row.redux.fn, description))

  # if errors occured and verification was obligatory
  if(obligatory)
    attr(data, "assertr_data_defective") <- TRUE

  num.violations <- sum(!log.vec)
  loc.violations <- which(!log.vec)
  offending.elements <- redux[!log.vec]
  assertion.id <- generate_id()

  error <- make.assertr.assert_rows.error("insist_rows",
                                          name.of.row.redux.fn,
                                          name.of.predicate.generator,
                                          as.character(keeper.vars),
                                          num.violations,
                                          loc.violations,
                                          offending.elements,
                                          description,
                                          assertion.id)
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
#' @param skip_chain_opts If TRUE, \code{success_fun} and \code{error_fun}
#'                        are used even if assertion is called within a chain.
#' @param obligatory If TRUE and assertion failed the data is marked as defective.
#'                   For defective data, all the following rules are handled by
#'                   \code{defect_fun} function.
#' @param defect_fun Function to call when data is defective. Defaults to skipping
#'                   assertion and storing info about it in special attribute.
#' @param description Custom description of the rule. Is stored in result
#'                    reports and data.
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
                   error_fun=error_stop, skip_chain_opts=FALSE,
                   obligatory=FALSE, defect_fun=defect_append,
                   description=NA){
  expr <- rlang::enexpr(expr)

  if(isTRUE(attr(data, "assertr_data_defective")))
    return(defect_fun(attr(data, "assertr_errors"), data, "verify", deparse(expr), NA, NA, description))
  # Use eval_tidy here to get the .data pronoun and all the eval_tidy benefits
  logical.results <- rlang::eval_tidy(expr, data, parent.frame())
  # NAs are very likely errors, and cause problems in the all() below.
  logical.results <- ifelse(is.na(logical.results), FALSE, logical.results)

  if (!is.logical(logical.results)) {
    warning("The result of evaluating '", deparse(expr),
      "' is not a logical vector")
  }
  if (length(logical.results) == 0) {
    warning("The result of evaluating '", deparse(expr),
      "' has length zero")
  }

  success_fun_override <- attr(data, "assertr_in_chain_success_fun_override")
  if(!skip_chain_opts && !is.null(success_fun_override)){
    if(!identical(success_fun, success_fun_override))
    success_fun <- success_fun_override
  }
  error_fun_override <- attr(data, "assertr_in_chain_error_fun_override")
  if(!skip_chain_opts && !is.null(error_fun_override)){
    if(!identical(error_fun, error_fun_override))
    error_fun <- error_fun_override
  }

  # if all checks pass in current assertion
  if(all(logical.results))
    return(success_fun(data, "verify", deparse(expr), NA, NA, description))

  # if errors occured and verification was obligatory
  if(obligatory)
    attr(data, "assertr_data_defective") <- TRUE

  num.violations <- sum(!logical.results)
  assertion.id <- generate_id()
  error <- make.assertr.verify.error("verify",
                                     num.violations, deparse(expr),
                                     (1:length(logical.results))[!logical.results],
                                     description, assertion.id)
  error_fun(list(error), data=data)
}
