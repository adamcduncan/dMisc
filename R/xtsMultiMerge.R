#' Merge multiple xts objects without going through zoo conversion.
#'
#' This function takes a list of xts objects and joins them, without having
#' to manually convert each to zoo, applying merge.zoo, and then converting
#' back to xts format. Just supply a list of xts objects and specify the join
#' method and the function returns a merged xts object.
#'
#' @param xtslist a list of xts objects.
#' @param join the type of database join to use in merging the xts objects.
#'
#' @return a merged xts object following the database join rules specified.
#'
#' @export
#'
#' @examples
#' dts.long <- seq(Sys.Date()-49, Sys.Date(), 1)
#' ret.long <- matrix(rnorm(50),ncol = 1) / 100
#' long.xts <- xts(ret.long, dts.long)
#'
#' dts.short <- seq(Sys.Date()-9, Sys.Date(), 1)
#' ret.short <- matrix(rnorm(10),ncol = 1) / 100
#' short.xts <- xts(ret.short, dts.short)
#'
#' xtsMultiMerge(list(short.xts, long.xts))
xtsMultiMerge <- function(xtslst, join = "inner") {
  out <- NULL
  for (i in 1:length(xtslst)) {
    out <- merge.xts(out, xtslst[[i]], join = "inner")
  }
  return(out)
}
