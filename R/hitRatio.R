#' Compute the hit ratio of single or the difference between two track records.
#'
#' This function allows you to calculate the percentage of observations greater
#' than zero in the case of a single track record, \code{x}. It also allows you
#' to pass a second track record. If the seconde track record is passed, then
#' the function computes the periodic differences and returns the percentage
#' of observations where the difference \code{x-y} is positive. In the two
#' track record case, the return value becomes an outperformance frequency
#' measure.
#'
#' @param x a series of observations from a given asset.
#' @param y a second track record to compare to x. Default is NULL.
#'
#' @return the percentage of time x is greater than 0 or the percentage of time
#'     \code{x-y} is greater than zero.
#'
#' @examples
#' dts <- seq(Sys.Date()-49, Sys.Date(), 1)
#' returns <- matrix(rnorm(100),ncol = 2) / 100
#' ret.xts <- xts(returns, dts)
#' hitRatio(ret.xts[,1])
#' hitRatio(x = ret.xts[,1], y = ret.xts[,2])
#'
#' @export
#'
hitRatio <- function(x, y = NULL) {

  if(is.null(length(x))) return("x must have positive length.")

  if (is.null(y)) {
    # only one track record supplied...
    out <- sum(as.numeric(x > 0))/length(x)
  } else {
    # two track records supplied...
    if (length(x) != length(y)) {
      return("x and y lengths differ. Recycling not appropriate.")
    } else {
      diff.xy <- x - y
      out <- sum(as.numeric(diff.xy > 0))/length(diff.xy)
    }
  }
  return(out)
}
