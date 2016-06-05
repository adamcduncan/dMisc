#' Create a set of returns in xts format that follow a specified distribution.
#'
#' This function returns a set of returns that follow the distribution passed
#' to makeNoise by the user. It is a convenience function when you want a set
#' of random returns that follow a certain distribution. The function is a
#' wrapper for rnorm etc. that handles all the time series wrangling.
#'
#' @param num_assets the number of columns of length \code{n} to create.
#' @param num_returns the number of rows you would like returned.
#' @param dates the dates you would like to use for the returns. If NULL, the
#' function will use \code{SysDate()} and go back in time \code{n} days.
#' @param dist one of "rnorm", "runif", or "dt".
#' @param ... other pass-through parameters to the random number generators.
#' @return a set of returns in xts format following the desired distribution.
#'
#' @examples
#' # Make 5 normally distributed assets...
#' makeNoise(5)
#'
#' # Make 5 normally distributed assets, 10 returns each, with sd = .10/sqrt(252), mean = 0)
#' makeNoise(5, num_returns = 10, mean = 0, sd = .10/sqrt(252))
#' @export
#'
makeNoise <- function(num_assets = 1, num_returns = 60, dates = NULL, dist = "rnorm", ...){
  n <- num_returns
  if (is.null(dates)) {
    dts <- seq(Sys.Date() - (n - 1), Sys.Date(), 1)
  } else {
    dts <- dates
  }
  if (!(dist %in% c("rnorm", "runif", "rt"))) {
    return("'dist' must be one of 'rnorm', 'runif', or 'rt'")
  } else {
    out <- NULL
    for (i in 1:num_assets) {
      if (dist == "rnorm") {
        rets <- matrix(rnorm(n, ...), ncol = 1)
      } else {
        if (dist == "runif") {
          rets <- matrix(runif(n, ...), ncol = 1)
        } else {
          rets <- matrix(rt(n, ...), ncol = 1)
        }
      }
      out <- cbind(out, rets)
      rm(rets)
    }
    out <- xts(out, dts)
    colnames(out) <- paste("Noise_",seq(1,num_assets,1),sep = "")
  }
  return(out)
}
