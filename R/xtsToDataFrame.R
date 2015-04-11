#' Convert and xts object to a data frame, suitable for use in ggplot2.
#'
#' A function to convert an xts object into a data frame that can be plotted
#' usinfg ggplot2. The date column will be the first column of the new data
#' frame and will have the name 'Date'.  This function uses a recursive
#' implementation for xts objects with many columns.
#'
#'@param xts.obj an xts object.
#'
#'@return a dataframe suitable for plotting in ggplot2. Date will be first
#'   column.
#'
#'@export
#'
#'@examples
#'dts <- seq(Sys.Date()-4, Sys.Date(), 1)
#'returns <- matrix(rnorm(10),ncol = 2) / 100
#'ret.xts <- xts(returns, dts)
#'names(ret.xts) <- c("Asset_1","Asset_2")
#'xtsToDataFrame(ret.xts)

xtsToDataFrame <- function(xts.obj) {

  if (ncol(xts.obj) == 1) {
    series.name <- colnames(xts.obj)[1]
    tmp <- xts.obj
    tmp.df <- as.data.frame(coredata(tmp))
    tmp.df$Date <- as.Date(unlist(lapply(strsplit(as.character(index(tmp))," "), function(x) x[1])))
    tmp.df.long <- reshape2::melt(tmp.df, id.var = "Date")
    tmp.df.long$asset <- rep(series.name, nrow(tmp.df.long))
    return(tmp.df.long)
  } else {
    num.assets <- ncol(xts.obj)
    asset.names <- colnames(xts.obj)
    df <- do.call(rbind, lapply(1:num.assets, function(x) {
      xtsToDataFrame(as.xts(xts.obj[, x]))
    }))
    df$asset <- ordered(df$asset, levels = asset.names)
    return(df)
  }
}
