#' Compute a moving average cross-over momentum signal.
#'
#' This function computes a moving average cross-over signal from a price
#' series. The series should be in xts format. The function will return
#' +1 if the shorter moving average of the price series is above the longer
#' moving average and -1 if below. It will return a 0 if the two moving averages
#' are the same.
#'
#' @param x an xts time series of prices.
#' @param s the number of periods for the shorter moving average (assumed to be
#' in the same periodicity as x. Meaning, if you pass daily data and s = 20, then
#' the shorter moving average will be calculated over 20 days. If you pass monthly
#' data, and s = 20, then the shorter moving average will be calculated over 20
#' months.
#' @param l the number of period for the longer moving average.
#'
#' @return a list containing a time series filled with -1, 0, or +1 signals based on the cross-over patterns of the moving averages, the short moving average series, and
#' the long moving average series.
#'
#' @examples
#' dts.long <- seq(Sys.Date()-499, Sys.Date(), 1)
#' ret.long <- matrix(rnorm(500),ncol = 1) * 100
#' long.xts <- xts(ret.long, dts.long)
#' ma.sig <- maCrossoverSignal(long.xts)
#'
#' @export
#'
maCrossoverSignal <- function(x, s = 50, l = 100) {

  if(!is.xts(x)){
    return("Must pass an xts object to this function.")
  } else {

    sa <- rollmean(x, k = s, align = "right")
    names(sa) <- "sa"

    la <- rollmean(x, k = l, align = "right")
    names(la) <- "la"

    both <- merge.xts(sa, la, join = "inner")
    names(both) <- c("sa", "la")

    signal <- xts(sign(both$sa - both$la), index(both))
    names(signal) <- "macSignal"

    return(signal)
  }
}
