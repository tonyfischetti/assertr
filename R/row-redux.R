
##
##  provides different row reduction functions
##

#' Computes mahalanobis distance for each row of data frame
#'
#' This function will return a vector, with the same length as the number
#' of rows of the provided data frame, corresponding to the average
#' mahalanobis distances of each row from the whole data set.
#'
#' This is useful for finding anomalous observations, row-wise.
#'
#' It will convert any categorical variables in the data frame into numerics.
#'
#' @param data A data frame
#' @param keep.NA Ensure that every row with missing data remains NA in
#'                 the output? TRUE by default.
#' @param robust Attempt to compute mahalanobis distance based on
#'         robust covariance matrix? FALSE by default
#'
#' @return A vector of observation-wise mahalanobis distances.
#' @seealso \code{\link{insist_rows}}
#' @examples
#'
#' maha_dist(mtcars)
#'
#' maha_dist(iris, robust=TRUE)
#'
#'
#' library(magrittr)            # for piping operator
#'
#' # using every column from mtcars, compute mahalanobis distance
#' # for each observation, and ensure that each distance is within 10
#' # median absolute deviations from the median
#' mtcars %>%
#'   insist_rows(maha_dist, within_n_mads(10), everything())
#'   ## anything here will run
#'
#' @export
maha_dist <- function(data, keep.NA=TRUE, robust=FALSE){
  # this implementation is heavily inspired by the implementation
  # in the "psych" package written by William Revelle
  if(!(class(data) %in% c("matrix", "data.frame")))
    stop("\"data\" must be a data.frame (or matrix)", call.=FALSE)
  a.matrix <- data.matrix(data)
  if(ncol(a.matrix)<2)
    stop("\"data\" needs to have at least two columns", call.=FALSE)
  row.names(a.matrix) <- NULL
  a.matrix <- scale(a.matrix, scale=FALSE)
  if(robust){
    if(sum(is.na(a.matrix)))
      stop("cannot use robust maha_dist with missing values", call.=FALSE)
    dcov <- MASS::cov.mcd(a.matrix)$cov
  } else{
    dcov <- cov(a.matrix, use="pairwise")
  }
  inv <- MASS::ginv(dcov)
  dists <- t(apply(a.matrix, 1,
                   function(a.row) colSums(a.row * inv, na.rm=TRUE)))
  dists <- rowSums(dists * a.matrix, na.rm=TRUE)
  if(keep.NA)
    dists[ apply(data, 1, function(x) any(is.na(x))) ] <- NA
  return(dists)
}


#' Counts number of NAs in each row
#'
#' This function will return a vector, with the same length as the number
#' of rows of the provided data frame, corresponding to the number of missing
#' values in each row
#'
#' @param data A data frame
#' @param allow.NaN Treat NaN like NA (by counting it). FALSE by default
#' @return A vector of number of missing values in each row
#' @seealso \code{\link{is.na}} \code{\link{is.nan}} \code{\link{not_na}}
#' @examples
#'
#' num_row_NAs(mtcars)
#'
#'
#' library(magrittr)            # for piping operator
#'
#' # using every column from mtcars, make sure there are at most
#' # 2 NAs in each row. If there are any more than two, error out
#' mtcars %>%
#'   assert_rows(num_row_NAs, within_bounds(0,2), everything())
#'   ## anything here will run
#'
#' @export
num_row_NAs <- function(data, allow.NaN=FALSE){
  if(!(class(data) %in% c("matrix", "data.frame")))
    stop("\"data\" must be a data.frame (or matrix)", call.=FALSE)
  pred <- function(x){ sum((is.na(x)&(!(is.nan(x))))) }
  if(allow.NaN){
    pred <- function(x){ sum((is.na(x) | is.nan(x))) }
  }
  ret.vec <- apply(data, 1, pred)
  return(ret.vec)
}


