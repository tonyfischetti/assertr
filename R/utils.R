
##
## utility function that are not exported
##


# for error messages, it's useful if the function
# name can be used (so the useR can tell what)
# predicate function failed. The problem is,
# assertions lend themselves to use with unnamed
# lambda functions so we have to make a better
# way to represent that
# stringify() used on a lamda will return a vector
# of character
get.name.of.function <- function(stringified.function){
  if(length(stringified.function) > 1)
    return("(lambda expression)")
  return(stringified.function)
}


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
    comment(ret.fun) <- "assertr/vectorized"
  }
  return(ret.fun)
}
# this is a closure
# marvel at this function's dedication to the FP paradigm!


# abstract out creation of error messages
# so we can make it prettier in future versions
make.assert.error.message <- function(name.of.predicate, column,
                                      num.violations, index.of.first.violation,
                                      offending.element){
  time.or.times <- ifelse(num.violations==1, "time", "times")
  eg.or.value <- ifelse(num.violations==1, "value", "e.g.")
  paste0("\nVector '", column, "' violates assertion '", name.of.predicate,
         "' ", num.violations, " ", time.or.times, " (", eg.or.value, " [",
         offending.element, "] at index ", index.of.first.violation, ")")
}



make.assert_rows.error.message <- function(name.of.predicate, num.violations,
                                           loc.violations){
  time.or.times <- ifelse(num.violations==1, "time", "times")
  eg.or.value <- ifelse(num.violations==1, "", "e.g. ")

  paste0("Data frame row reduction violates predicate '",
         name.of.predicate, "' ", num.violations, " ", time.or.times, " (",
         eg.or.value, "at row number ", loc.violations[1], ")")
}


is.vectorized.predicate <- function(predicate){
  if(!is.null(comment(predicate)) && comment(predicate)=="assertr/vectorized")
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


make.verify.error.message <- function(num.violations){
  sing.plur <- ifelse(num.violations==1, " failure)", " failures)")
  paste0(c("verification failed! (", num.violations, sing.plur))
}


## assertr stop
# stop() with call.=FALSE
assertr_stop <- function(message){
  stop(message, call.=FALSE)
}

