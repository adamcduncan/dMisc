#' Convert a wealth index to xts returns. This is the inverse of makeIndex().
#'
#' This function is the inverse of makeIndex. It takes an xts object that
#' contains a wealth index and converts it to returns.
#'
#' @param x an xts object containing one or more wealth indices.
#'
#' @return an xts object containing the returns for each wealth index in x.
#'
#' @export
#'
#' @examples
#' dts <- seq(Sys.Date()-4, Sys.Date(), 1)
#' returns <- matrix(rnorm(5),ncol = 1) / 100
#' ret.xts <- xts(returns, dts)
#' x <- makeIndex(ret.xts)
#' indexToXTSReturns(x)
indexToXTSReturns <- function(x) {

  if (!is.xts(x)) {
    stop ("You must supply an xts object.")
  } else {
    dts <- index(x, 0)
    # we already have an xts object
    ret.ret <- apply(x, MARGIN = 2, Return.calculate, method = "discrete")
  }
  ret.ret[1, ] <- 0
  ret.ret <- xts(ret.ret, dts)
  return(ret.ret)
}
