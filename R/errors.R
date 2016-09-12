
##
##  defines some error object constructors, error functions, and 
##  error chaining functions
##


#######################################
#   assert error object and methods   #
#######################################
# used by "assert" and "insist"
make.assertr.assert.error <- function(name.of.predicate,
                                      column,
                                      num.violations,
                                      index.of.violations,
                                      offending.elements){
  time.or.times <- ifelse(num.violations==1, "time", "times")
  msg <- paste0("Column '", column, "' violates assertion '",
                name.of.predicate,"' ", num.violations, " ", time.or.times)

  this_error <- list()

  this_error$error_df <- data.frame(index=index.of.violations,
                                    value=offending.elements)
  this_error$message <- msg
  this_error$num.violations <- num.violations
  this_error$call <- name.of.predicate

  class(this_error) <- c("assertr_assert_error", "assertr_error",
                         "error", "condition")
  return(this_error)
}

print.assertr_assert_error <- function(error){
  cat(error$message)
  cat("\n")
  print(error$error_df)
}

summary.assertr_assert_error <- function(error){
  cat(error$message)
  cat("\n")
  numrows <- nrow(error$error_df)
  print(head(error$error_df, n=5))
  if(numrows > 5)
    cat(paste0("  [omitted ", numrows-5, " rows]\n\n"))
}

