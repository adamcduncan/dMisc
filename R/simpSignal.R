#' Compute a buy/sell momentum signal based on the specifed look back period.
#'
#' This function will return a vector of buy/sell signals based on whether
#' or not the current value of \code{x} is above or below the price of
#' \code{x}. If the current price is above, the return entry is 1 otherwise
#' the return entry will be zero. The return vector will be of length
#' \code{x} - \code{lookback}. This function aids in building a smooth
#' momentum signal by calling simpSignal() with many values of \code{lookback}
#' and averaging the results.
#'
#' @param x an xts object of prices.
#' @param lookback the length of the look back period over which to evalute
#'     the momentum signal.
#' @return an xts object of discrete momentum signals drawn from the set c(-1,1),
#'     where -1 indicates "short" or "sell" and 1 indicates "long" or "buy".
#'
#' @examples
#' library(dMisc)
#' dts <- seq(Sys.Date()-99, Sys.Date(), 1)
#' returns <- matrix(rnorm(100),ncol = 1) / 100
#' ret.xts <- xts(returns, dts)
#' s <- simpSignal(ret.xts, lookback = 5)
#' plot(s, type = "l")
#'
#' @export
#'
simpSignal <- function(x, lookback = 12) {
  if(!is.xts(x)){
    return("Only xts or equivalent objects supported.")
  } else {
    if(length(x) <= (lookback+1)){
      return("x must be longer in length than lookback + 1.")
    } else {
      d <- na.omit(x)
      far.prices <- lag(d, lookback+1)
      near.prices <- lag(d, 1) # exclude current observation
      signals <- sign(near.prices - far.prices)
      return(signals)
    }
  }
}
