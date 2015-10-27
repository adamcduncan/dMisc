#' Convert a character list of tickers to an xts object.
#'
#' Takes a character list of tickers and returns an xts object of the
#' variables. Useful for creating objects after a call to dMisc::getData().
#' For example, typically after calling dMisc::getData() (a wrapper for
#' quantmod::getSymbols()) you will end up with OHLC objects in .GlobalEnv.
#' This function grabs all the variables and uses zoo::merge.zoo to bind
#' them together in a single object. Return type is set to xts via as.xts().
#'
#' @param tickers a character vector of symbols.
#' @param na.omit a flag indicating whether NAs should be removed.
#'
#' @return a single xts object containing all of the prices in \code{tickers}.
#'
#' @examples
#' tickers <- c("IBM","JPM")
#' quantmod::getSymbols(tickers)
#' data <- Cl(tickersToXTS(tickers)) # just closing prices
#'
#' @export
tickersToXTS <- function(tickers, na.omit = TRUE) {

  list.variables <- lapply(tickers, get)  # a list of actual variables
  lst.len <- length(list.variables)
  out <- as.zoo(list.variables[[1]])
  if (lst.len == 1) {
    # the single element list case...
    if (na.omit) {
      out <- na.omit(out)
    }
    return(as.xts(out))
  } else {
    # list of length > 1
    for (i in 2:length(list.variables)) {
      i <- list.variables[[i]]
      out <- merge.zoo(out, as.zoo(i))
    }
    ifelse(na.omit, out <- na.omit(out))
    return(as.xts(out))
  }
}
