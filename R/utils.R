
##
## utility function that are not exported
##



# as a convenience, this package allows for the creation
# of predicates that only define a false condition. we
# need to wrap the predicate to ensure that it returns
# TRUE (and not NULL) if not FALSE
make.predicate.proper <- function(improper.predicate){
  ret.fun <- function(x){
    return(length(improper.predicate(x))==0 || improper.predicate(x))
  }
  if(is.vectorized.predicate(improper.predicate)){
    attr(ret.fun, "assertr_vectorized") <- TRUE
  }
  return(ret.fun)
}
# this is a closure
# marvel at this function's dedication to the FP paradigm!


is.vectorized.predicate <- function(predicate){
  if(!is.null(attr(predicate, "assertr_vectorized")) &&
     attr(predicate, "assertr_vectorized"))
    return(TRUE)
  return(FALSE)
}


apply.predicate.to.vector <- function(a.column, predicate){
  res <- logical(length(a.column))
  if(is.vectorized.predicate(predicate))
    res <- predicate(a.column)
  else
    res <- vapply(a.column, predicate, logical(1))
  return(res)
}


#' Returns TRUE if data.frame or list has specified names
#'
#' This function checks parent frame environment for existence of names.
#' This is meant to be used with `assertr`'s `verify` function to check
#' for the existence of specific column names in a `data.frame` that is
#' piped to `verify`. It can also work on a non-`data.frame` list.
#'
#' @param ... A arbitrary amount of quoted names to check for
#' @return TRUE is all names exist, FALSE if not
#' @seealso \code{\link{exists}}
#' @examples
#'
#' verify(mtcars, has_all_names("mpg", "wt", "qsec"))
#'
#' library(magrittr)   # for pipe operator
#'
#' \dontrun{
#' mtcars %>%
#'   verify(has_all_names("mpgg"))  # fails
#' }
#'
#' mpgg <- "something"
#'
#' mtcars %>%
#'   verify(exists("mpgg"))   # passes but big mistake
#'
#' \dontrun{
#' mtcars %>%
#'   verify(has_all_names("mpgg")) # correctly fails
#' }
#'
#' @export
has_all_names <- function(...){
  check_this <- list(...)
  parent <- parent.frame()
  given_names <- names(parent$.top_env$.data)
  all(check_this %in% given_names)
}
