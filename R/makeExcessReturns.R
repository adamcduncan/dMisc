#' Take a set of asset returns and subtract the benchmark returns from them.
#'
#'This function just strubtracts the returns of \code{benchmark} from the
#'returns of \code{assets}. It's just a convenience function for generating
#'excess returns. \code{benchmark} is typically passes as the risk-free rate.
#'
#' @param assets the xts object of asset returns.
#' @param benchmark the xts object of benchmark returns.
#' @return an xts object of excess returns defined as \code{asset - benchmark}
#' @export
#'
#' @examples
#' dts.asset <- seq(Sys.Date()-49, Sys.Date(), 1)
#' ret.asset <- matrix(rep(.05,50),ncol = 1)
#' asset.xts <- xts(ret.asset, dts.asset)
#'
#' dts.benchmark <- seq(Sys.Date()-49, Sys.Date(), 1)
#' ret.benchmark <- matrix(rep(.03,50),ncol = 1)
#' benchmark.xts <- xts(ret.benchmark, dts.benchmark)
#'
#' # Should return .02 everywhere...
#' makeExcessReturns(assets = asset.xts, benchmark = benchmark.xts)
#'
makeExcessReturns <- function(assets, benchmark){
  # Subtracts the returns of the benchmark from the returns of
  # the asset. Only supports a single benchmark.

  a <- PerformanceAnalytics::checkData(assets)
  b <- PerformanceAnalytics::checkData(benchmark)

  er <- dMisc::xtsApply(a, cFUN = function(x) x - b)

  return(er)

}
