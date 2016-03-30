#' Orthogonalize a set of factors with respect to all other independent variables.
#'
#' This function will regress each elemen of the factor set against every other
#' element in the factor set. This produces a factor set where each factor in the set
#' contains no information about the other factors in the set. 
#' 
#' @param f an xts vector representing the factor set.
#' 
#' @return an xts object of factors that have been orthogonalized with respect
#'     to all the other factors in the set.
#' @export

orthogonalize_all <- function(F){
  # Given a set of factors F regress each element of f against all of the other
  # elements of F to create a fully orhtogonal factor set where each element of
  # f contains no information from any of the other factors in the set.

  f <- checkData(F)

  out <- xtsApply(f, cFUN = function(x){
  	as.numeric(residuals(lm(x ~ ., data = f))) + as.numeric(coef(lm(x ~ ., data = f))[1])
  })

  return(out)
}

