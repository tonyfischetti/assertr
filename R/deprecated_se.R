##
## this file contains the underscore-postfixed SE verbs, to be removed
## eventually
##

#' @export
#' @param .dots Use assert_() to select columns using standard evaluation.
#' @rdname assert
assert_ <- function(data, predicate, ..., .dots, success_fun=success_continue,
                    error_fun=error_stop){
  warning("`assert_` is deprecated and will eventually be removed from assertr.
          ",
          "Please use `assert` instead.", call. = FALSE)
  sub.frame <- dplyr::select_(data, ..., .dots = .dots)
  name.of.predicate <- rlang::expr_text(predicate)
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
                                          offending.elements)
    return(an_error)
  })

  # remove the elements corresponding to the columns without errors
  errors <- Filter(function(x) !is.null(x), errors)
  error_fun(errors, data=data)
}

#' @export
#' @param .dots Use assert_rows_() to select columns using standard evaluation.
#' @rdname assert_rows
assert_rows_ <- function(data, row_reduction_fn, predicate, ..., .dots,
                         success_fun=success_continue,
                         error_fun=error_stop){
  warning("`assert_rows_` is deprecated and will eventually be removed from
          assertr. ",
          "Please use `assert_rows` instead.", call. = FALSE)
  sub.frame <- dplyr::select_(data, ..., .dots = .dots)
  name.of.row.redux.fn <- rlang::expr_text(row_reduction_fn)
  name.of.predicate <- rlang::expr_text(predicate)
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

  if(!is.vectorized.predicate(predicate))
    predicate <- make.predicate.proper(predicate)

  redux <- row_reduction_fn(sub.frame)

  log.vec <- apply.predicate.to.vector(redux, predicate)

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
                                          loc.violations)
  error_fun(list(error), data=data)

}

#' @export
#' @param .dots Use insist_() to select columns using standard evaluation.
#' @rdname insist
insist_ <- function(data, predicate_generator, ..., .dots,
                    success_fun=success_continue,
                    error_fun=error_stop){
  warning("`insist_` is deprecated and will eventually be removed from assertr.
          ",
          "Please use `insist` instead.", call. = FALSE)
  sub.frame <- dplyr::select_(data, ..., .dots = .dots)
  name.of.predicate.generator <- rlang::expr_text(predicate_generator)
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
                                          offending.elements)
    return(an_error)
  })

  # remove the elements corresponding to the columns without errors
  errors <- Filter(function(x) !is.null(x), errors)

  error_fun(errors, data=data)
}

#' @export
#' @param .dots Use insist_rows_() to select columns using standard evaluation.
#' @rdname insist_rows
insist_rows_ <- function(data, row_reduction_fn, predicate_generator, ...,
                         .dots, success_fun=success_continue,
                         error_fun=error_stop){
  warning("`insist_rows_` is deprecated and will eventually be removed from
          assertr. ",
          "Please use `insist_rows` instead.", call. = FALSE)
  sub.frame <- dplyr::select_(data, ..., .dots = .dots)


  name.of.row.redux.fn <- rlang::expr_text(row_reduction_fn)
  name.of.predicate.generator <- rlang::expr_text(predicate_generator)
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

  redux <- row_reduction_fn(sub.frame)

  predicate <- predicate_generator(redux)

  log.vec <- apply.predicate.to.vector(redux, predicate)

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
