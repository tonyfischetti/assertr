
##
## utility function that are not exported
##



# as a convenience, this package allows for the creation
# of predicates that only define a false condition. we
# need to wrap the predicate to ensure that it returns
# TRUE (and not NULL) if not FALSE
make.predicate.proper <- function(improper.predicate){
  ret.fun <- function(x){
    if(length(improper.predicate(x))==0)    return(TRUE)
    if(!improper.predicate(x))              return(FALSE)
    return(TRUE)
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
     attr(predicate, "assertr_vectorized")==TRUE)
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

