#' Form a matrix based on repeating a column x, n times.
#'
#' This function replicates the column x, n times and returns a matrix.
#'
#' @param x the column you want replicated in matrix form.
#' @param n the number of columns you would like in your matrix.
#'
#' @return a matrix with column x repeated n times.
#'
#' @examples
#' a <- c(1,2,3,4,5)
#' repCol(a, 10)
#'
#' @seealso repRow
#'
#' @export
#'
repCol <- function(x, n) {
  matrix(rep(x, each = n), ncol = n, byrow = TRUE)
}
