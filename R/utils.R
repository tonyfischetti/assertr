
##
## utility function that are not exported
##



# as a convenience, this package allows for the creation
# of predicates that only define a false condition. we
# need to wrap the predicate to ensure that it returns
# TRUE (and not NULL) if not FALSE
make.predicate.proper <- function(improper.predicate){
  ret.fun <- function(x){
    ret <- improper.predicate(x)
    return(length(ret) == 0 || ret)
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
#' @return TRUE if all names exist, FALSE if not
#' @family Name verification
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
  given_names <- rlang::env_names(parent$.top_env)
  given_names <- given_names[given_names != ".data"]
  all(check_this %in% given_names)
}

#' Returns TRUE if data.frame or list has only the specified names
#'
#' This function checks parent frame environment for a specific set of names; if
#' more columns are present than those specified, an error is raised.
#'
#' This is meant to be used with `assertr`'s `verify` function to check
#' for the existence of specific column names in a `data.frame` that is
#' piped to `verify`. It can also work on a non-`data.frame` list.
#'
#' @inheritParams has_all_names
#' @family Name verification
#' @return TRUE is all names exist, FALSE if not
#' @examples
#'
#' # The last two columns names are switched in order, but all column names are
#' # present, so it passes.
#' verify(
#'   mtcars,
#'   has_only_names(c(
#'     "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
#'     "carb", "gear"
#'   ))
#' )
#'
#' # More than one set of character strings can be provided.
#' verify(
#'   mtcars,
#'   has_only_names(
#'     c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am"),
#'     c("carb", "gear")
#'   )
#' )
#'
#' \dontrun{
#' # The some columns are missing, so it fails.
#' verify(mtcars, has_only_names("mpg"))
#' }
#' @export
has_only_names <- function(...) {
  # Collect all arguments
  check_this <- unlist(list(...))
  # Ensure a clear error message if other classes are passed in.
  stopifnot("Arguments to 'has_only_names()' must be character strings."=is.character(check_this))
  parent <- parent.frame()
  given_names <- rlang::env_names(parent$.top_env)
  given_names <- given_names[given_names != ".data"]
  # Verification of exact column order is not feasible because of the conversion
  # to an environment before this function sees the data.  When converted to an
  # environment, the order of names appears to become arbitrary.
  if (length(check_this) != length(given_names)) {
    # extra names exist
    FALSE
  } else {
    # The names may be in any order
    length(setdiff(check_this, given_names)) == 0
  }
}

#' Returns TRUE if data.frame columns have a specified class
#'
#' This is meant to be used with `assertr`'s `verify` function to check
#' for the existence of a specific column class in a `data.frame` that is
#' piped to `verify`.
#'
#' @param ... An arbitrary amount of quoted column names to check for
#' @param class Expected class for chosen columns.
#' @return TRUE if all classes are correct, FALSE if not
#' @examples
#'
#' verify(mtcars, has_class("mpg", "wt", class = "numeric"))
#'
#' library(magrittr)   # for pipe operator
#'
#' \dontrun{
#' mtcars %>%
#'   verify(has_class("mpg", class = "character"))  # fails
#' }
#'
#' @export
has_class <- function(..., class){
  check_this <- list(...)
  parent <- parent.frame()
  given_classes <- lapply(check_this, function(name) class(parent$.top_env[[name]]))
  all(unlist(given_classes) %in% class)
}


#' Generates random ID string
#'
#' This is used to generate id for each assertion error.
#'
#' For single assertion that checks multiple columns, each error log
#' is stored as a separate element. We provide the ID to allow
#' detecting which errors come from the same assertion.
#'
generate_id <- function() {
  paste0(
    paste0(sample(c(LETTERS, letters, 0:9), 5, TRUE), collapse = ""),
    round(as.numeric(Sys.time())*1000)
  )
}
