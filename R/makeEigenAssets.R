#' Create a time series of synthetic assets built from principal components.
#'
#' This function takes a set of asset returns and applies the weights from
#' the eigen vectors of the asset returns. These 'synthetic assets' can then
#' be compared to factors or manipulated in other useful ways.
#'
#' The calculation is just the matrix multiplication of \code{R} and the
#' \code{rotation} matrix of R derived from PCA.
#'
#' @param R an xts object containing a time series of asset returns.
#' @param rotation an optional matrix containing the eigen vectors derived from the assets.
#' @return an xts object of synthetic 'eigen assets.'
#' @export
makeEigenAssets <- function(R, rotation = prcomp(R)$rotation) {
  # take a set of asset returns in xts format and a PCA rotation (eigenvectors)
  # and apply the eigen vector weights to the asset returns to create
  # synthetic "eigen assets" or factors.

  stopifnot(xts::is.xts(R) || dim(R)[2] == dim(rotation)[2])

  out <- xts(R %*% rotation, index(R))
  return(out)
}
