#' Compute the Diversification Ratio for a portfolio.
#'
#' This function computes the Diversification Ratio (DR) for a portfolio, given
#' a weight vector, \code{w}, a set of asset returns, \code{R}, and a
#' covariance matrix, \code{sigma}. This ratio is a measure of how well
#' diversified a portoflio is. The square of this number is a proxy for the
#' number of unique sources of variation exist in a portfolio. The higher the
#' number, the more diversified the portfolio.
#'
#' @param w a vector of weights.
#' @param R an xts object or matrix of asset returns.
#' @param sigma a covariance matrix for R.
#' @param scale the annualization factor for R. Default = 1. This parameter is
#'     passed through to wtdAvgVol() and portfolioVol().
#'
#' @return the Diversification Ratio (DR) for the portfolio.
#'
#' @examples
#' dts <- seq(Sys.Date()-199, Sys.Date(), 1)
#' returns <- matrix(rnorm(1000),ncol = 5) / 100
#' ret.xts <- xts(returns, dts)
#' cm <- cov(ret.xts)
#' divRatio(R = ret.xts, sigma = cm)
#'
#' @seealso wtdAvgVol, portfolioVol
#' @export
#'
divRatio <- function(w = NULL, R, sigma, scale = 12) {
  if (is.null(w)) {
    w <- rep(1/ncol(R), ncol(R))
  }
  wav <- dMisc::wtdAvgVol(w = w, R = R, scale = scale)
  pvol <- dMisc::portfolioVol(w = w, R = R, sigma = sigma, scale = scale)
  dr <- wav / pvol
  return(dr)
}
