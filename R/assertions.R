
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
#'            You can treat variable names like they are positions.
#'            Use positive values to select variables; use negative
#'            values to drop variables.
#'
#' @return data if predicate assertion is TRUE. error if not.
#' @note See \code{vignette("assertr")} for how to use this in context
#' @seealso \code{\link{verify}} \code{\link{insist}}
#' @examples
#'
#' assert(mtcars, not_na, vs)           # returns mtcars
#'
#' assert(mtcars, not_na, mpg:carb)     # return mtcars
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
assert <- function(data, predicate, ...){
  sub.frame <- dplyr::select(data, ...)
  name.of.predicate <- as.character(substitute(predicate))
  if(length(name.of.predicate)>1) name.of.predicate <- name.of.predicate[1]
  full.predicate <- make.predicate.proper(predicate)
  # ew! we have to use loops because we should stop
  # at the first violation and we need the index
  vapply(names(sub.frame),
         function(column){
           this.vector <- sub.frame[[column]]
           for(i in 1:length(this.vector)){
             if(!(predicate(this.vector[i]))){
               error.message <- make.assert.error.message(name.of.predicate, i,
                                                          column, this.vector[i])
               stop(error.message, call.=FALSE)}}
           return(TRUE)}, logical(1))
  return(data)
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
#'            You can treat variable names like they are positions.
#'            Use positive values to select variables; use negative
#'            values to drop variables.
#'
#' @return data if dynamically created predicate assertion is TRUE. error if not.
#' @note See \code{vignette("assertr")} for how to use this in context
#' @seealso \code{\link{assert}} \code{\link{verify}}
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
insist <- function(data, predicate_generator, ...){
  sub.frame <- dplyr::select(data, ...)
  name.of.predicate.generator <- as.character(substitute(predicate_generator))
  if(length(name.of.predicate.generator)>1)
    name.of.predicate.generator <- name.of.predicate.generator[1]

  # get true predicates (not the generator)
  true.predicates <- sapply(names(sub.frame),
    function(column){predicate_generator(sub.frame[[column]])})

  # map each predicate to their respective function
  vapply(names(sub.frame),
         function(column){
           this.vector <- sub.frame[[column]]
           for(i in 1:length(this.vector)){
             if(!(true.predicates[[column]](this.vector[i]))){
               error.message <- make.assert.error.message(name.of.predicate.generator,
                                                          i,
                                                          column, this.vector[i])
               stop(error.message, call.=FALSE)}}
           return(TRUE)}, logical(1))
  return(data)
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
#'
#' @return data if verification passes. error if not.
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
verify <- function(data, expr){
  expr <- substitute(expr)
  # conform to terminology from subset
  envir <- data
  enclos <- parent.frame()
  logical.results <- eval(expr, envir, enclos)
  if(all(logical.results))
    return(data)
  num.violations <- sum(!logical.results)
  error.message <- make.verify.error.message(num.violations)
  stop(error.message)
}







