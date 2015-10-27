#' Plot covariance elispe between a and b with marginal densitites in margin.
#'
#' This function makes a scatterplot of a and b.  It plots the sample
#' histograms in the margins. It also overlays the kernal densities for a and b.
#' It then overlays the covariance ellipse for a and b. This function is useful
#' for detecting whether or not a and b are invariants.
#'
#' @param a a vector of numeric values
#' @param b a vector of numeric values with the same length as a.
#'
#' @return a scatterplot with the covariance ellipse of a and b displayed.
#'
#' @examples
#' library(dMisc) # requires access to function getAngle()
#' a <- rnorm(500, mean = 0 , sd = 1)
#' b <- rnorm(500, mean = 0, sd  = 1.5)
#' plotCovEllipse(a,b)
#'
#' @export
#'
plotCovEllipse <- function(a, b) {

  a <- as.numeric(coredata(a))
  b <- as.numeric(coredata(b))

  if (!(length(a) == length(b))) {
    return("Variables are not of uniform length.")
  } else {

    # Calculate eigen vector and eigen values from the covariance matrix of
    # a and b.
    m <- matrix(cov(a, b), nrow = 2, ncol = 2)
    diag(m) <- c(var(a), var(b))
    ev <- eigen(m)
    eval <- sqrt(ev$values)
    evec <- ev$vectors

    # Make the ellipsoid from scrath...
    u <- eval[1]
    v <- eval[2]
    x0 <- mean(a)
    y0 <- mean(b)
    alpha <- atan(evec[, 1][2] / evec[, 1][1])  #arctan of ratio of eigenvectors
    theta <- seq(0, 2 * pi, length = (1000))

    x <- x0 + u * cos(theta) * cos(alpha) - v * sin(theta) * sin(alpha)
    y <- y0 + u * cos(theta) * sin(alpha) + v * sin(theta) * cos(alpha)
    el <- data.frame(cbind(x, y))

    # df_ell <- ellipse(m, scale=c(sd(a),sd(b)), centre=c(mean(a),mean(b)))

    xend1 <- max(el$x)
    yend1 <- el[which.max(el[, 1]), 2]
    xend2 <- el[which.min(el[, 2]), 1]
    yend2 <- min(el$y)
    angle <- getAngle(c(xend1, yend1), c(xend2, yend2))

    df <- data.frame(x = a, y = b)

    hist_top <- ggplot() +
      geom_histogram(aes(a), colour = "grey60", binwidth = 0.1) +
      ggtitle("Invariant Analysis for Two Assets")
    empty <- ggplot() + geom_smooth(aes(a, b), colour = "red") +
      theme(legend.position = "none")

    hist_right <- ggplot() +
      geom_histogram(aes(b), colour = "grey60", binwidth = 0.1) +
      coord_flip()

    scatter <- ggplot(data = df, aes(x = x, y = y), colour = "red",
                      environment = environment()) +
      geom_point(size = 1.5, alpha = 0.8, colour = "dark blue") +
      geom_path(data = el,aes(x = x, y = y),
                colour = "black",
                size = 1.5,
                linetype = 1) +
      geom_point(aes(x = mean(x), y = mean(y)), colour = "red", size = 3.5) +
      geom_segment(aes(x = mean(x), y = mean(y), xend = xend1, yend = yend1),
                   colour = "black",
                   size = 1.0,
                   arrow = arrow(length = unit(0.3, "cm"))) +
      geom_segment(aes(x = mean(x),
                       y = mean(y),
                       xend = xend2,
                       yend = yend2),
                   colour = "black",
                   size = 1.0,
                   arrow = arrow(length = unit(0.3, "cm"))) +
      ggtitle(paste("Angle: ", round(angle,3)))

    # drawing
    grid.arrange(hist_top, empty, scatter, hist_right, ncol = 2, nrow = 2,
                 widths = c(3, 1), heights = c(1, 3))
  }
}
