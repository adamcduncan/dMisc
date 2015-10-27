#' Calculate the RMSE (Root Mean Squared Error) of a prediction model.
#'
#' Given an actual series, \code{y} and a fitted series, \code{yhat}, this
#' function will return the RMSE, what I'm calling 'Prediction Error'. This is
#' useful when comparing out-of-sample results from the modeling process across
#' different fitting methods.
#'
#' @param y the response or actual data series.
#' @param yhat the fitted series.
#'
#' @return the RMSE or prediction error  = sqrt(mean((y-yhat)^2))
#'
#' @examples
#' x <- rnorm(15)
#' y <- x + rnorm(15)
#' fit <- lm(y ~ x)
#' new <- data.frame(x = rnorm(15))
#' yhat <- predict(fit, newdata = new)
#' predictionError(y,yhat)
#'
#' @export
#'
predictionError <- function(y, yhat) {

  if(length(y) == 0 || length(yhat) == 0) {
    return("y and yhat must have postiive length.")
  }

  if(length(y) != length(yhat)){
    return("y and yhat have different lengths. Recyling not supported.")
  }

  ydiff <- y - yhat

  if (is.null(ncol(y))) {
    # we have a single column vector...
    pe <- sqrt(mean((y - yhat)^2))
  } else {
    pe <- apply(ydiff, 2, function(x) sqrt(mean((x)^2)))
  }
  names(pe) <- names(y)
  return(pe)
}
