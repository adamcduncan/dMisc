#' A wrapper for getSymbols that can handle lists of tickers.
#'
#' This is just a simple wrapper for getSymbols that can take either a
#' single ticker vector or a list of ticker vectors for, say, multiple
#' asset classes.
#'
#' @param tickers a character vector of symbols to be passed to getSymbols.
#' @param datasrc the data source for the call. Usually, "yahoo" or "google" or "FRED".
#' @param quiet controls verboseness of getSymbols call.
#' @export
getData <- function(tickers, datasrc, quiet = FALSE, ...) {
  # Wrapper for getSymbols that can handle lists of tickers. You can pass a
  # list of ticker vectors or a single ticker vector.  Recursively calls
  # getData for lists and loads xts objects into .global_env

  if (class(tickers) %in% "character") {
    for (i in 1:length(tickers)) {

      quantmod::getSymbols(tickers[i], src = datasrc,
                 auto.assign = getOption("getSymbols.auto.assign", TRUE),
                 env = globalenv(), ...)

      if(quiet == FALSE) {
        cat(tickers[i], i, "\n")
      }
    }
  } else {
    if (class(tickers) %in% "list") {
      for (i in 1:length(tickers)) {
        getData(tickers[[i]], datasrc)  # recursive call to getData
      }
    } else {
      return("Invalid parameter type. Should be character vector or list of
             character vectors.")
    }
    }
  }
