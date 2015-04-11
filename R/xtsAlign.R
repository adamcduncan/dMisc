#' Align one xts object to another without merging them.
#'
#' This function aligns two xts objects so that they will have the same
#' date indexing. The default join method is "inner." See the documentation
#' for merge.xts for different join methods, although this operation probably
#' only makes sense using an "inner" join.
#'
#' @param target the xts object you would like to modify.
#' @param match.to the xts object you would like to emulate or align to.
#' @param join the type of database join to use in matching. Default is "inner". Other
#'   methods could lead to strange results.
#'
#' @return an xts object whose date index is now aligned with \code{match.to}.
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
#' xtsAlign(target = long.xts, match.to = short.xts)
xtsAlign <- function(target, match.to, join = "inner") {
  # Takes two xts objects and aligns their dates returning target with the
  # same date structure as match.to (trims does not merge objects)

  dummy <- xts(rep(1, nrow(match.to)), index(match.to))
  ret <- merge.xts(target, dummy, join = join)
  ret <- subset(ret, select = (-dummy))
  rm(dummy)
  return(ret)
}
