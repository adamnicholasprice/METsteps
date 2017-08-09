#' Create Properly Structured Sentence
#'
#' Paste and collapses vector of strings into correct sentence structure using "," and "and".
#' 
#' @param x Character; Vector of strings.
#' @param period Logical; If TRUE, return output with concluding period. Defaults to FALSE.
#' @export
#' @return Shiny application
#' @examples
#' pasteSentence()

pasteSentence <- function(x,
                          period = FALSE){
  if (typeof(x) != 'character'){
    warning("pasteSentence() input must be a character vector. Returning original data.")
    return(x)
  }else{
    if (length(x) == 1){
      if (period){
        return(paste0(x, '.'))
      }else{
        return(x)
      }
    }else if (length(x) == 2){
      if (period){
        return(paste0(paste0(x, collapse = ' and '), '.'))
      }else{
        return(paste0(x, collapse = ' and '))
      }
    }else if (length(x) > 2){
      n <- length(x)
      xL <- x[n]
      xB <- x[-n]
      sB <- paste0(xB, collapse = ', ')
      sL <- paste0(sB, ', and ', xL)
      if (period){
        return(paste0(sL, '.'))
      }else{
        return(sL)
      }
    }
  }
}