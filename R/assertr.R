#' assertr: Assertive programming for R analysis pipeline.
#'
#' The assertr package supplies a suite of functions designed to verify
#' assumptions about data early in an analysis pipeline.
#'
#' See the assertr vignette or the documentation for more information \cr
#' > \code{vignette("assertr")}
#'
#' You may also want to read the documentation for the function that
#' \code{assertr} provides:
#' \itemize{
#'   \item \code{\link{assert}}
#'   \item \code{\link{verify}}
#'   \item \code{\link{not_na}}
#'   \item \code{\link{in_set}}
#'   \item \code{\link{within_bounds}}
#'   }
#'
#'
#' @examples
#' library(magrittr)     # for the piping operator
#'
#' mtcars %>%
#'   verify(mpg > 0) %>%
#'   assert(in_set(0,1), am, vs) %>%
#'   assert(is.numeric, mpg:carb)
#'   # do something actually interesting here
#'
#'
#' @docType package
#' @name assertr
NULL
