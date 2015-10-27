#' Read in a file containing manager or asset data and return a list.
#'
#' This function reads in a data file and returns a list. The list contains:
#' the raw return data, returns of the managers, a wealth index
#' created by equally weighted average of the managers, returns from that
#' composite index.
#'
#' This function is useful if you have a .csv file with
#' a 'Date' column with dates in "Y-m-d" format and manager or asset return
#' information in each of the other columns. The heading of each of the non-date
#' columns should be the asset name. This wrapper just saves you from having
#' to do all the minor cleaning associated with creating and assembling the
#' data into a format useful for analysis.
#'
#' @param path a character string specifying the path to the data.
#'
#' @return a list containing:
#'      \code{mydata} the raw imported data.
#'      \code{comps.ret} the daily returns of the managers in xts format.
#'      \code{comps.index} an equally weighted wealth index created from
#'          \code{comps.ret}.
#'      \code{comps.index.ret} the returns created from \code{comps.index}.
#'
#' @examples
#' # Requires dMisc::makeIndex()
#' library(dMisc)
#' dts <- seq(Sys.Date()-4, Sys.Date(), 1)
#' returns <- matrix(rnorm(10),ncol = 2) / 100
#' ret.df <- data.frame(as.character(dts),returns)
#' colnames(ret.df) <- c("Date", "asset_1", "asset_2")
#' tf <- write.csv(ret.df,file = "tf.csv", row.names = F)
#'
#' mydata <- getManagerData(path = paste(getwd(),"/tf.csv",sep = ""))
#'
#'
#' @export

getManagerData <- function(path) {

  if (is.null(path)) {
    return(NULL)
  }
  mydata = read.csv(path, header = TRUE, sep = ",")

  # Clean up the data a bit...
  comps <- mydata
  dts <- as.Date(comps$Date, format = "%Y-%m-%d")  # Grab the date column...
  comps <- mydata[, 2:ncol(mydata)]  # no date column...
  comps <- xts(coredata(comps), dts)

  # Check to see whether the user passed return data or wealth index
  # data and do the appropriate thing.

  if (as.numeric(comps[1, 1]) == 1) {
    # We have wealth index data...
    comps <- na.locf(comps, na.rm = FALSE)
    comps <- comps * 100
    comps.ret <- xts(apply(comps, MARGIN = 2,
                           FUN = Return.calculate, method = "discrete"), dts)
    comps.ret[is.na(comps.ret)] <- 0
  } else {
    # We have return data...
    comps.ret <- comps
    comps.ret[is.na(comps.ret)] <- 0  # replace any NA's with zero retuns.
  }

  # Make an index from the peers frame...
  comps.index.ret <- as.xts(apply(comps.ret, MARGIN = 1, FUN = mean))
  comps.index <- makeIndex(comps.index.ret, inv = FALSE, ret = TRUE)

  # Now create the list that holds all the variables and return...
  return(list(mydata = mydata, comps.ret = comps.ret,
              comps.index = comps.index, comps.index.ret = comps.index.ret))
}
