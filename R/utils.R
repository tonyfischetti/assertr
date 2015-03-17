
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
  function(x){
    if(length(improper.predicate(x))==0)    return(TRUE)
    if(!improper.predicate(x))              return(FALSE)
    return(TRUE)
  }
}
# this is a closure
# marvel at this function's dedication to the FP paradigm!


# abstract out creation of error messages
# so we can make it prettier in future versions
make.assert.error.message <- function(name.of.predicate,
                                      index.of.first.violation,
                                      name.of.column,
                                      offending.element){
  paste0("Assertion '", name.of.predicate, "' violated at index ",
         index.of.first.violation, " of vector '", name.of.column,
         "' (value: ", offending.element, ")")
}


make.verify.error.message <- function(num.violations){
  sing.plur <- ifelse(num.violations==1, " failure)", " failures)")
  paste0(c("verification failed! (", num.violations, sing.plur))
}
