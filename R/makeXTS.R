#' Convert a dataframe of returns to xts format.
#'
#'Supposing you have read in a data file that has the first column containing
#'dates followed by subsequent columns of asset returns, this function will
#'convert that dataframe into an xts object suitable for analysis with xts based
#'analysis routines, like PerformanceAnalytics.
#'
#'@param x a data frame of returns with the first column a date column labeled
#'  'Date'.
#'@param format a recognized date format. Defaults to "Y-m-d" format.
#'@param names a vector of names for each of the columns, excluding the date
#'  column. Defaults to the colnames of the dataframe x[2:ncol(x)].
#'
#'@return an xts object based on x.
#'
#'@examples
#'mydata <- read.csv("mydata.csv",sep = ",")
#'mydata.xts <- makeXTS(mydata)
makeXTS <- function(x, format = "%Y-%m-%d",
                    names = colnames(x)[2:ncol(x)]) {

  dts <- as.Date(x[, 1], format = format)
  data <- x[, 2:ncol(x)]  # remove the date column
  out <- xts(data, dts)  # form the xts object
  names(out) <- names
  return(out)
}
