
###########################################################
##                                                       ##
##   predicates.R                                        ##
##                                                       ##
##      defines some useful predicates for use with      ##
##      assert functions in assertr                      ##
##                                                       ##
##                Author: Tony Fischetti                 ##
##                        tony.fischetti@gmail.com       ##
##                                                       ##
###########################################################



# assert
## checks arity
## calls delagate functions


# assert_frame
#    example (has_rows)

# assert_vector


# PREDICATES (for vectors)

#' Returns TRUE if element is not NA
#'
#' This is the inverse of \code{\link[base]{is.na}} if it is
#' used on a atomic element. This is a convenience function meant
#' to be used as a predicate in an \code{\link{assertr}} assertion.
#'
#' @param x A single atomic value
#' @param allow.NaN A logical indicating whether NaNs should be allowed
#'        (default FALSE)
#' @return TRUE if x is not NA, FALSE otherwise
#' @seealso \code{\link{is.na}}
#' @examples
#' not_na(NA)
#' not_na(2.8)
#' not_na("tree")
#'
#' @export
not_na <- function(x, allow.NaN=FALSE){
  if(length(x)>1)            stop("not_na must be called with single element")
  if(is.null(x))             stop("not_na must be called with single element")
  if(allow.NaN && is.nan(x)) return(TRUE)
  if(is.na(x))               return(FALSE)
  return(TRUE)
}


# dummy
divby5 <- function(x){
  if(x %% 5 != 0)
    return(FALSE)
}





# # assert predicate
# # this function takes a dataframe, a column,
# # and a predicate
# # if the predicate holds true for every element
# # of the vector, this function passes and just returns the
# # data frame that was passed to it (for later use in pipeline)
# # if it fails, it'll tell you the first element for which it failed
# # and stop execution
# #
# # the predicate can be any function that returns FALSE
# # when an undesirable/unallowable event/condition is met
# # there is no need to explicitly return TRUE
# assertP <- function(aframe, column, predicate){
#   davector <- aframe[[column]]
#   # to help out the user, we only require
#   # that the predicate returns FALSE when something
#   # bad happens. because of this, we need to wrap that
#   # function in another one that explicitly returns TRUE
#   # if there is no return value (for acceptable elements)
#   anotherpred <- function(x){
#     if(length(predicate(x))==0){
#       return(TRUE)
#     }
#     if(predicate(x)==FALSE){
#       return(FALSE)
#     }
#     return(TRUE)
#   }
#   # retlog <- sapply(davector, predicate)
#   retlog <- sapply(davector, anotherpred)
#   viol <- which(!(retlog))
#   if(sum(viol)){
#     firstviol <- viol[1]
#     stop(paste0("Assertion violated at index ",
#                firstviol, " (", davector[firstviol], ")"), call.=FALSE)
#   }
#   return(aframe)
# }
#
#
#
# read.csv("./test.csv", stringsAsFactors=FALSE,
#                  colClasses=c("character",
#                               "numeric",
#                               "numeric")) %>%
#   assertP("height", function(x){!is.na(x)}) %>%
#   assertP("height", notna) %>%
#   assertP("height", function(x){if(x%%5!=0){return(FALSE)}}) %>%
#   assertP("height", divby5) %>%
#   filter(condition=="B")
#
#
#
#
#
#
# # assertv(this, "height", function(x){!is.na(x)})
#
# # make function for
# # notna()
# # between(numeric, numeric)
# #    that returns a function (predicate)
# #    stock predicated
# # lambda functions that only return FALSE if not true
#
