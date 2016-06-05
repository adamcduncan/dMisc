#' Flip the signs on eigen vectors so that PC1 is positive.
#'
#' This function ensures that PC1 is always positive. Sometimes, the rotation
#' matrix form PCA returns a set of eigen vectors where PC1 is mostly negative
#' signs. That's usually undesirable for most financial applications. This
#' function just flips things around so they are more sensible from a finance
#' perspective.
#'
#' @param x a matrix of eigen vectors.
#' @return a matrix containing flipped eigen vectors.
#' @export
#'
pcaFlip <- function(x){
  # flip signs on eigen vectors based on signs of first eigen vector.
  vecs <- x
  f <- sign(vecs[1,])
  out <- sweep(vecs, 2, STATS = f, FUN = "*")
  return(out)
}
