#' Calculate an approximate standard deviation of returns that accounts for
#' auto-correlation in the returns.
#'
#' This function is an implementation of the calculations found in
#' Kinlaw, Kritzman, and Turkington have who published two companion papers
#' called “The Divergence of High- and Low-Frequency Esitmation: Causes and
#' Consequences” and "The Divergence of High- and Low-Frequency Esitmation:
#' Implications for Performance Measurement."
#'
#' The function returns a set of calculations:
#'  1. The no-auto-correlation adjusted standard deviation \code{sigma*sqrt(t)}
#'  2. The auto-correlation adjusted measure : \code{sigma*(sqrt(t) + cors)}
#'  3. The difference between the two measures.
#'  4. The ratio of the two measures.
#'
#' @param x an xts object containing a time series of asset returns.
#' @param L the maximum number of lags to consider. Default is 12.
#' @return a list of four calculations described above.
#' @export
sdAutoCor <- function(x, L = 12){

  # We're going to assume that x is an xts object of continuously
  # compunded returns. They typically are not, so this is an approximation.

  # The full sample sd of x, given it's current periodicity
  sd_x <- apply(x, 2, sd)
  auto_cors <- apply(x, 2, acf, lag.max = L, plot = FALSE)
  c_index <-  seq(L, 1, -1)
  p <- NULL

  for (k in 1:length(auto_cors)) {
    cors <- sum(as.numeric(auto_cors[[k]]$acf[1:L])*c_index)*2
    p[[k]] <- sd_x[[k]]*sqrt(L + cors)
  }

  ret <- list()
  ret$sd_nocor <- as.matrix(sd_x*sqrt(L))
  colnames(ret$sd_nocor) <- c("std_dev_NOAC")

  ret$sd_autocor <- matrix(p, ncol = 1, nrow = ncol(x))
  colnames(ret$sd_autocor) <- c("std_dev_AC")
  rownames(ret$sd_autocor) <- colnames(x)

  ret$diff <- ret$sd_autocor - ret$sd_nocor
  colnames(ret$diff) <- c("Difference")
  rownames(ret$diff) <- colnames(x)

  ret$ratio <- ret$sd_autocor/ret$sd_nocor
  colnames(ret$ratio) <- c("Ratio")
  rownames(ret$ratio) <- colnames(x)

  return(ret)
}
