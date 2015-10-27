#' Scale an xts object of returns to a target volatility.
#'
#' This function takes an xts object of returns and scales it to the volatility
#' specified by \code{target.vol}. This is useful when you want to put compare
#' the performance of different assets. Putting all assets on the same risk
#' level is important, otherwise comparisons are not meaningful.
#'
#' @param a an xts object of asset returns
#' @param target.vol a target volatility expressed in annualized terms. So, 10%
#'     annualized volatility should be passed as .10/sqrt(12) for monthly data.
#'     Default is 1.0. Meaning, the function will not alter the series if no
#'     target volatility is specified.
#'
#' @return an xts object of returns that has been scaled to have a
#'     standard deviation equal to \code{target.vol}.
#'
#' @examples
#' library(dMisc)
#' dts <- seq(Sys.Date()-11, Sys.Date(), 1)
#' returns <- matrix(rnorm(12),ncol = 1) / 100
#' ret.xts <- xts(returns, dts)
#' target.vol = .10/sqrt(12)
#'
#' ret.va <- volAdjust(ret.xts, target.vol = .10/sqrt(12))
#' trunc(sd(ret.va),5) == trunc(.10/sqrt(12),5) # TRUE
#'
#' @export
volAdjust <- function(a, target.vol = 1) {

  if (ncol(a) == 1) {
    vol.scale.factor <- target.vol / sd(a)
    out <- a * vol.scale.factor
  } else {
    vol.scale.factors = xtsApply(a, function(x) target.vol / sd(x))
    out <- a * vol.scale.factors
  }
  return(out)
}
