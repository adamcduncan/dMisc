#' Compute a 'breakout' momentum signal over the specified lookback period.
#'
#' This function will return a vector of buy/sell signals based on whether
#' or not the current value of \code{x} is above the maximum or below the minimum price of
#' \code{x} over the lookback period. If the current price is above the maximum, the return
#' value is 1. If the current price is below the minumum, then the return value is -1. Otherwise,
#' the return entry will be the previous signal. The return vector will be of length
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
#' s <- breakoutSignal(ret.xts, lookback = 5)
#' plot(s, type = "l")
#'
#' @export
#'
breakoutSignal <- function(x, lookback = 12){
  if(!is.xts(x)){

    return("Only xts or equivalent objects supported.")
  } else {

    if(length(x) <= (lookback + 1)) {
      return("x must be longer in length than lookback + 1.")

    } else {
      d <- TTR::DonchianChannel(x, n = lookback, include.lag = T)

      signals <- NULL

      for (i in 1:nrow(d)) {
        if (is.na(d[i, 1])){
          signals[i] <- NA
        } else {
          if (x[i, 1] < d[i, 3]) {
            signals[i] <- -1
          } else {
            if (x[i, 1] > d[i, 1]){
              signals[i] <- 1
            } else {
              signals[i] <- signals[i-1]
            }
          }
        }
      }
      return(sig = xts(signals, index(d)))
    }
  }
}
