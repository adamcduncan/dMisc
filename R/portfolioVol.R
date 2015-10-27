#' Compute portfolio volatility from a set of returns.
#'
#' This function just calculates the portfolio volatility given a set of asset
#' returns, \code{R}, a weight vector, \code{w}, and a covariance matrix,
#' \code{sigma}. A \code{scale} value can be supplied which is the annualization
#' factor for the returns if annualized volatility is desired. The default is
#' 1 (ie. no annualization). Supply 12 for monthly data, 252 for daily, and
#' so on.
#'
#' If no weight vector is supplied, the function will calculate equal weights
#' for the portfolio assets.
#'
#' @param w a vector of weights. If not supplied, function will assign equal
#'     weights. Default value is \code{NULL}. (ie. equal weights)
#' @param R an xts or matrix of asset returns.
#' @param sigma a covariance matrix for the asset returns.
#' @param scale an annualization factor for the returns if desired. Default = 1.
#'
#' @return a numeric value representing the portfolio volatility.
#'
#' @examples
#' dts <- seq(Sys.Date()-199, Sys.Date(), 1)
#' returns <- matrix(rnorm(1000),ncol = 5) / 100
#' ret.xts <- xts(returns, dts)
#' cm <- cov(ret.xts)
#' portfolioVol(R = ret.xts, sigma = cm)
#'
#' @seealso wtdAvgVol, divRatio
#'
#' @export
#'
portfolioVol <- function(w = NULL, R, sigma, scale = 1) {

  if (is.null(w)) {
    w <- rep(1/ncol(R), ncol(R))
  }
  S <- sigma
  out <- sqrt(t(w) %*% S %*% w) * sqrt(scale)
  return(as.numeric(out))
}
