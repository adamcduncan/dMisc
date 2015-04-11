#' An apply method for xts objects.
#'
#' This function implements an apply method for xts objects. I works in exactly
#' the same was as apply does, but makes sure you get back an xts object. In
#' some cases the standard apply mehtods return strange resutls when applied to
#' xts objects. This function handles the operations correctly and will do
#' column-wise apply when \code{"margin"} is set to 2, and row-wise when
#' \code{"margin"} is set to 1. \code{"cFUN"} fills the roll of the the standard
#' FUN in apply methods.
#'
#' @param x an xts object over which you want to apply some function.
#' @param cFUN the function you wish to apply.
#' @param margin controls whether you want column-wise apply (margin = 2) or
#'   row-wise apply (margin = 1). Defaults to margin = 2.
#'
#'@return an xts object where cFUN has been applied to the columns or rows.
#'
#'@examples
#'dts <- seq(Sys.Date()-4, Sys.Date(), 1)
#'returns <- matrix(rnorm(10),ncol = 2) / 100
#'ret.xts <- xts(returns, dts)
#'xtsApply(ret.xts, cFUN = sum, margin = 1)
#'
#'Note that apply(ret.xts,2,sum) will return a vector, NOT and xts object!
#'@export
xtsApply <- function(x, cFUN, margin = 2, ...) {
  # an implementation of apply that works for xts objects...
  if (!xts::is.xts(x))
    stop("Must supply function with xts object")

  if (margin == 2) {
    # column-wise calculation
    Z <- x
    for (j in 1:ncol(x)) {
      Z[, j] <- do.call(cFUN, list(x[, j], ...))
    }
  } else {
    # row-wise calculation
    Z <- xts::xts(rep(0, nrow(x)), index(x))
    for (j in 1:nrow(x)) {
      Z[j, 1] <- do.call(cFUN, list(x[j, ], ...))
    }
  }
  return(Z)
}
