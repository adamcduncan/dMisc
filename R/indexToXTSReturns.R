#' Convert a wealth index to xts returns. This is the inverse of makeIndex().
#'
#' This function is the inverse of makeIndex. It takes an xts object that
#' contains a wealth index and converts it to returns.
#'
#' @param x an xts object containing one or more wealth indices.

indexToXTSReturns <- function(x) {

  if (names(x)[1] == "Date") {
    dts <- as.Date(x$Date, format = "%Y-%m-%d")
    ret <- subset(x, select = -Date)
    ret <- xts(ret, dts)
    ret.ret <- apply(ret, MARGIN = 2, Return.calculate, method = "log")
  } else {
    dts <- index(x, 0)
    # we already have an xts object
    ret.ret <- apply(x, MARGIN = 2, Return.calculate, method = "log")
  }
  ret.ret[1, ] <- 0
  ret.ret <- xts(ret.ret, dts)
  return(ret.ret)
}
