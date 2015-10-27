#' Return the angle between to vectors, u and v.
#'
#' This function returns the angle between two vectors u and v. If u and v are
#' orthogonal then the angle will be 90 degrees. This is a helper function for
#' dMisc::plotCovEllipse(). It can be called directly if needed.
#'
#' @param u a vector of numeric values. (Must be at least length = 2)
#' @param v a vector of numeric values. (Must be at least length = 2)
#'
#' @return the angle between the two vectors u and v.
#'
#' @examples
#' u <- rnorm(5,0,1)
#' v <- rnorm(5,0,1.5)
#' getAngle(u,v)
#'
#' @export
#'
getAngle <- function(u, v) {

  if(length(u) < 2 || length(v) < 2){
    return("Vectors u and v must both be of at least length 2.")
  } else {
    # Calculate eigen vector and eigen values from the covariance matrix of
    # u and v.
    m <- matrix(cov(u, v), nrow = 2, ncol = 2)
    diag(m) <- c(var(u), var(v))
    ev <- eigen(m)
    eval <- sqrt(ev$values)
    evec <- ev$vectors

    # Make the ellipsoid from scrath...
    a <- eval[1]
    b <- eval[2]
    x0 <- mean(u)
    y0 <- mean(v)
    alpha <- atan(evec[, 1][2] / evec[, 1][1])  #arctan of ratio of eigenvectors
    theta <- seq(0, 2 * pi, length = (1000))

    x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
    y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)
    el <- data.frame(cbind(x, y))

    xend1 <- max(el$x)
    yend1 <- el[which.max(el[, 1]), 2]
    xend2 <- el[which.min(el[, 2]), 1]
    yend2 <- min(el$y)
    vec_1 <- c(xend1, yend1)
    vec_2 <- c(xend2, yend2)

    numerator <- vec_1[1] * vec_2[1] + vec_1[2] * vec_2[2]
    denominator <- sqrt(vec_1[1]^2 + vec_1[2]^2) * sqrt(vec_2[1]^2 + vec_2[2]^2)

    out <- (acos(numerator / denominator))/0.0174532925
    return(out)
  }
}
