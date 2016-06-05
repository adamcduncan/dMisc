#' Convert a return series into a cumulative wealth index starting at 100.
#'
#' This function is a flexible function that converts xts objects into
#' cumulative wealth indices. It alllows the user to pass raw price or
#' return series and also can handle FX data where inversion of the series
#' is necessary before computing cumulative wealth. There is also a flag
#' for controlling the removal of NA's in the data. Note that the default
#' behavior for the function is to omit the first return datum. The function
#' returns and xts object of the same length that was passed. This means that
#' the first return is lost due to starting at 100 at the first date.
#' To make full use of ALL of the return data, the return object would have
#' to be expanded by -1 date value. This behavior is not supported.
#'
#' @param x and xts object containing either raw price or return data.
#' @param inv a flag that tells the function whether to invert the
#'   series before computing returns. Useful for FX data.
#' @param ret a flag indicating whether return or price data has been
#'   passed to the function. Default is TRUE.
#' @param na.rm a flag indicating whether NAs should be removed from the
#'   data before computing returns. Default is TRUE.
#'
#' @return an xts object starting at 100 and evolving as the arithmetic
#'   returns of x.
#'
#' @export
#'
#' @examples
#' dts <- seq(Sys.Date()-4, Sys.Date(), 1)
#' returns <- matrix(rnorm(5),ncol = 1) / 100
#' ret.xts <- xts(returns, dts)
#' makeIndex(ret.xts)

makeIndex <- function(x, inv = FALSE, ret = TRUE, na.rm = TRUE) {

  stopifnot(is.xts(x))
  data <- x
  init.val <- 100
  nam <- names(data)
  dts <- as.Date(unlist(strsplit(as.character(index(data)), " ")))

  if (na.rm) {
    data <- na.omit(data)
  }

  if (inv == TRUE) {
    data <- 1/data
  }

  if (!ret) {
    data <- PerformanceAnalytics::Return.calculate(data, method = "discrete")
  }

  n <- nrow(data)
  new.series <- rep(0, n)

  new.series[1] <- init.val * (1 + data[1])

  for (i in 2:n) {
    new.series[i] <- (1 + data[i]) * new.series[i - 1]
  }
  new.series <- xts(new.series, dts)
  names(new.series) <- nam

  return(new.series)
}
