#' Repeat the rows of a matrix n times.
#'
#' This function replicates the rows of matrix n times.
#'
#' @param x the row you want replicated in matrix form.
#' @param n the number of rows you would like in your matrix.
#'
#' @return a matrix with x repeated n times.
#'
#' @examples
#' a <- c(1,2,3,4,5)
#' repRow(a, 10)
#'
#' @seealso repCol
#'
#' @export
#'
repRow <- function(x, n) {
  matrix(rep(x, each = n), nrow = n)
}
