#' Orthogonalize a set of factors with respect to some independent variable.
#'
#' This function will regress the factor set f against 'mkt_var' in a series
#' of single factor regressions. The function will return the an xts object
#' that contains the intercept + residuals from regressing each of the columns
#' of \code{f} on the \code{mkt_var}. If \code{reconstitute} is TRUE, then
#' \code{mkt_var} will be returned in column 1. If \code{reconstitue = FALSE},
#' then \code{mkt_var} is not returned with the others.
#'
#' @param mkt_var and xts vector representing the independent variable.
#' @param f an xts object of factor variables.
#' @param reconstitute a flag to indicate wheter the mkt_var should be
#'     returned along with the orthoganalized factors. Default is TRUE.
#' @return an xts object of factors that have been orthogonalized with respect
#'     to \code{mkt_var}
#' @export

orthogonalize <- function(mkt_var, f, reconstitute = T, demean = F, inc_alphas = F){
  # Regress a set of factors s against the mkt_var and return the alpha+
  # residuals. These are new "pseudo" factors that contain information not
  # captured by mkt_var. (at least not in a linear way)
  # If f is a single column matrix, then function just de-means f.

  if (demean) {
    # just de-mean the variables
    out <- xtsApply(f, center)
  } else {
    # user wants us to actually run regressions and return residuals + Bo.
    out <- xtsApply(f,cFUN = function(x){
      as.numeric(residuals(lm(x~mkt_var))) +
        ifelse(inc_alphas, as.numeric(coef(lm(x~mkt_var))[1]), 0)
    })
  }
  # out now contains alpha + error terms or is just de-meaned in the single
  # column case.

  if (reconstitute){
    # put the mkt_var variable back into the xts object, if requested by caller.
    out <-  out[ , -which(names(out) %in% names(mkt_var))]
    out <- xts(cbind(mkt_var, out),index(mkt_var))
  } else {
    out <- xts(out,index(mkt_var))
  }

  return(out)
}
