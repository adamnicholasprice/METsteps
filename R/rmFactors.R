#' Remove factors
#'
#' This function removes factors from numeric vector, matrix, or dataframe.  Accepts data UNIFORM IN TYPE of types
#' 'integer', 'double', or 'character'.
#' @param x Object in vector, matrix, or dataframe format, with uniform type.  To check, use 'typeof' function on object.
#' Must be 'integer', 'double', or 'character'.
#' @export
#' @return Input data without factors
#' @examples
#' rmFactors(x)

rmFactors <- function(x){
  if (typeof(x) == 'integer' || typeof(x) == 'double'){
    return(as.numeric(as.character(x)))
  }else if (typeof(x) == 'character'){
    return(as.character(x))
  }else{
    stop('Date not uniform type')
  }
}