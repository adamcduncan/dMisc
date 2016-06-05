#' Apply a set of weights to the columns of an xts object.
#'
#' This function is just a wrapper to the 'sweep' function because the
#' arguments to sweep are too hard to remember.
#'
#'@param w the weight vector. Must have same number of elements as columns in R.
#'@param R the xts object of returns or assets.
#'
#'@return an xts object with the weights, w, applied to R.
#'
#'@examples
#'library(xts)
#'dts <- seq(Sys.Date()-4, Sys.Date(), 1)
#'returns <- matrix(rnorm(10),ncol = 2) / 100
#'ret.xts <- xts(returns, dts)
#'wts <- c(.50, .50)
#'returns.wtd <- applyWeights(wts, ret.xts)
#'# check...
#'returns/returns.wtd
#'
#'@export

applyWeights <- function(w, R) {
  # Apply weights to an xts object of assets R.

  r <- PerformanceAnalytics::checkData(R)
  out <- sweep(r, MARGIN = 2, STATS = w, FUN = '*')

  return(out)
}
