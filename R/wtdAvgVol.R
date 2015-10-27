#' Compute the weighted average volatility of assets in a portfolio.
#'
#' Given a weight vector, \code{w}, and an xts object of returns, \code{R} this
#' will return the weighted average volatility assuming all the assets are
#' perfectly uncorrelated. (ie. all pairwise covariances are zero)
#' If \code{equal.wt=TRUE}, then \code{w} is ignored. This is the default.
#' Otherwise, \code{w} is used for the weight vector.
#'
#' This function is useful for calculations like the Diversification Ratio
#' where the weighted average volatility is in the numerator and the fully
#' correlated volatility of the portfolio is in the denominator.
#'
#' @param w a weight vector equal in length to the numnber of columns of R.
#' @param R an xts object of returns.
#' @param equal.wt a flag indicating whether or not to apply equal weights.
#'     Defualt is TRUE.
#' @param scale the annualization factor for the data. If supplied, this will
#'     scale the return volatilities to annualized volatilities. Default is 1.
#'
#' @return a numeric value equal to the weighted average volatility of the
#'     assets.
#'
#' @examples
#' dts <- seq(Sys.Date()-199, Sys.Date(), 1)
#' returns <- matrix(rnorm(1000),ncol = 5) / 100
#' ret.xts <- xts(returns, dts)
#' wtdAvgVol(R = ret.xts)
#'
#' @seealso portfolioVol, divRatio
#'
#' @export
#'
wtdAvgVol <- function(w, R, equal.wt = TRUE, scale = 1) {

  if (equal.wt == TRUE) {
    w <- rep(1/ncol(R), ncol(R))
  }
  avg.vol <- apply(R, 2, sd) * sqrt(scale)
  wtd.avg.vol <- t(w) %*% avg.vol

  return(as.numeric(wtd.avg.vol))
}
