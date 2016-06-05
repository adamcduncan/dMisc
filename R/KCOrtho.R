#' Orthogonalize a set of factors with respect to all other independent variables.
#'
#' This is an implementation of Klien and Chow (2014?) "Orthogonalized Equity
#' Risk Premia and Systematic Risk Decomposition."
#' Original code by Greg Murphy, 2016.
#'
#' This function will return a matrix of factors that have been completed
#' orthogonalized with respect to every other factor in the set while still
#' preserving the orginal sizes of the variances. This is helpful in producing
#' better factor models that have senisble coefficients. Orthogonalizing the
#' brute force way will yield very tiny variances in the resultant factor set
#' and leave you with massive coefficients in your model. This routine nicely
#' solves this problem.
#'
#' @param factors an xts object containing a time series of independent variables.
#' @return an xts object of factors that have been orthogonalized with respect to
#' all of the other factors in the set.
#' @export
KCOrtho <- function(factors){
  # As the approach is mostly focused on matrix algebra,
  # immediately converting the xts object to a matrix is essential

  f <- PerformanceAnalytics::checkData(factors) %>%
    zoo::coredata(.)

  # f.mean is a matrix consisting of a repeating vector of the mean return
  # of each factor.  Each column consists of only one unique element, the mean
  # return of that factor.

  f.mean <- apply(f, MARGIN = 2, FUN = mean) %>%
    rep(., nrow(f)) %>%
    matrix(.,nrow = nrow(f), ncol = ncol(f), byrow = T)

  # f.demean is the demeaned set of factor returns, f-f.mean
  f.demean <- as.matrix(f - f.mean)

  # f.cov is the factor covariance matrix of demeaned (centered) factor returns
  f.cov <- cov(f.demean, method = "pearson")

  # Scalar multiplication that yields the same matrix as tranpose(f.demean) %*% f.demean.
  # Relates directly to formula for covariance.

  M <- f.cov * (nrow(f) - 1)

  # O is a matrix of eigenvectors of M.
  # D is a diagonal matrix of the corresponding eigen values of M.
  # D is then transformed so as to conform with additional matrix definitions
  # as shown in the paper so that the two may be used to calculate S,
  # the matrix that defines the transformation from the
  # original factor set to the orthogonal set.

  O <- matrix(data = eigen(M)$vectors, nrow = nrow(M), ncol = ncol(M))
  D.init <- diag(x = eigen(M)$values, nrow = nrow(M), ncol = ncol(M))
  D.inv <- (matrixcalc::matrix.inverse(D.init))
  D.final <- sqrt(D.inv)

  S <- O %*% D.final %*% t(O)

  stdev <- apply(f.demean, MARGIN = 2, FUN = PerformanceAnalytics::StdDev) %>%
    diag(x = ., nrow = ncol(f.cov), ncol = nrow(f.cov))

  # S is transformed by the diagonal matrix of standard deviations of the
  # demeaned factor set. (Mathematically equivalent to standard deviations of the
  # original factor set). I believe this is done to preserve the original
  # covaraiance structure, but am not totally clear on this point.

  S.final <- (S %*% stdev) * sqrt(nrow(f) - 1)

  F.ortho <- f %*% S.final

  colnames(F.ortho) <- colnames(f)

  F.ortho <- xts::xts(F.ortho, zoo::index(factors))

  return(F.ortho)
}
