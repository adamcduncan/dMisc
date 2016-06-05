#' Compute gross returns from net returns and a fee structure.
#'
#' This function computes gross returns from a set of net returns and a
#' specified fee structure. This is particularly important when factor modeling
#' hedge fund returns. We generally don't want to model net returns. Rather, we
#' want to model gross returns. The default for the function is a standard
#' 2% management fee and 20% performance fee structure with 25 bps of annual
#' expenses. The function also assumes a high water mark is in place such that
#' performance fees are not accrued if current equity (as measured by cumulative
#' wealth from the start of the series) is below the peak equity of the series.
#' The default assumes monthly data.
#'
#' @param nr the net returns in xts format.
#' @param mgmt.fee the annual managment fee, assumed to crystalize in the same
#'     frequency as \code{nr}. If nr is monthly, then mgmt.fee/12 will be
#'     deducted from each monthly return. Default is .02.
#' @param perf.fee the percentage performance fee. As long as the fund is above
#'     high water mark when \code{highwater == TRUE} then this percentage will
#'     be deducted from all positive profits each period. Default is .20.
#' @param highwater a flag indicating whether or not the fund has a high water
#'     mark. Default is TRUE.
#' @param ann.exp the annual expenses of the fund passed through to investors.
#'     Default is 25 bps of annual expenses.
#' @param scale a numeric value indicating the frequency of the data. Default
#'     is 12, indicating monthly return data. Use 4 for quarterly and so on.
#'
#' @return an xts object representing the gross return series.
#'
#' @examples
#' library(dMisc)
#' library(xtsExtra)
#' dts <- seq(Sys.Date()-2519, Sys.Date(), 1)
#' returns <- matrix(rnorm(2520),ncol = 1) / 100
#' ret.xts <- xts(returns, dts)
#' ret.gross <- makeGrossReturns(ret.xts, scale = 252)
#' head(cbind(ret.xts,ret.gross))
#'
#' @export
#'
makeGrossReturns <- function(nr, mgmt.fee = 0.02, perf.fee = 0.2,
                             highwater = TRUE, ann.exp = 0.0025,
                             scale = 12) {
  if (!is.xts(nr))
    stop("Must supply function with xts object")
  dts <- index(nr)
  nr.index <- dMisc::makeIndex(nr)
  gr <- NULL
  gr[1] <- nr[1] + (mgmt.fee + ann.exp)/scale

  for (i in 2:nrow(nr)) {
    ## Step 1: gross up nr by the de-annualized fixed expenses
    gr[i] <- nr[i] + (mgmt.fee + ann.exp)/scale

    # Now, we need to check to see if we are above or below the high
    # water mark...
    hwm <- max(nr.index[0:(i - 1)])
    above.hwm <- ifelse(hwm < nr.index[i], TRUE, FALSE)

    if (above.hwm) {
      ## manager earns performance fee, so we need to gross up the net return...
      gr[i] = gr[i] * (1 + perf.fee)
    }
  }
  gr <- xts(gr, dts)
  names(gr) <- "Gross Return"
  return(gr)
}
