#' Short head of object
#'
#' Returns a more limited head (~10 col) of the object.
#' @param x Vector, matrix, dataframe, or list.
#' @param n_col Vector; Which columns to return. Defaults to 10.
#' @param returnDim Logical; Return dimensions of data? Defaults to TRUE
#' @export
#' @return Head of x.

shead = function(x, n_col = 1:10, returnDim = TRUE){
  if (is.null(dim(x))){
    if (returnDim) cat(paste0('(length: ', length(x), ')','\n'))
    print(head(x))
  }
  if (!(is.null(dim(x)))){
    if (ncol(x) < 10){
      #cat(paste0('(ncol: ', ncol(x), ')', '  ', '(nrow: ', nrow(x), ')', '\n'))
      if (returnDim) cat(paste0('[', nrow(x), ',', ncol(x), ']', '\n'))
      print(head(x))
    }else{
      #cat(paste0('(ncol: ', ncol(x), ')', '  ', '(nrow: ', nrow(x), ')', '\n'))
      if (returnDim) cat(paste0('[', nrow(x), ',', ncol(x), ']', '\n'))
      print(head(x[,n_col]))
    }
  }
}


#' Coin of data set
#'
#' Returns both the short head and short tail of dataset
#' @param x Vector, matrix, dataframe, or list.
#' @param full Logical; Should the full head and full tail be returned instead of shorts?
#' @export
#' @return Head and tail of x.

coin = function(x, full = FALSE){
  if (full){
    cat('head: \n')
    print(head(x))
    cat('tail: \n')
    print(tail(x))
  }else{
    cat('head: \n')
    METsteps::shead(x)
    cat('tail: \n')
    METsteps::shead(x = x[((nrow(x)-6):nrow(x)),],
                    returnDim = FALSE)
  }
}


#' Check matching between vectors
#'
#' Returns a more limited head (~10 col) of the object.  There are more options in function that are commented out
#' that provide more information.
#' @param x Vector
#' @param y Vector same length as x.
#' @export
#' @return Logical value of TRUE if x and y are the same.

test_match_order <- function(x,y) {
  #if (isTRUE(all.equal(x,y))) print('Perfect match in same order')
  #if (!isTRUE(all.equal(x,y)) && isTRUE(all.equal(sort(x),sort(y)))) print('Perfect match in wrong order')
  #if (!isTRUE(all.equal(x,y)) && !isTRUE(all.equal(sort(x),sort(y)))) print('No match')
  return((isTRUE(all.equal(x,y))))
}


#' Clear Everything From R Session
#'
#' Clears all objects, variables, and figures from R.  Optional argument to reset R.
#' @param wd Character; Optional character path for new working directory. Defaults to NULL.
#' @param resetR Logical; Optional argument to reset entire R session. Helps to clear memory. Defaults to FALSE.
#' @export
#' @return Head of x.
#' @examples
#' METclear()

METclear <- function(resetR = FALSE){
  cat("\014")
  rm(list  = ls(envir = as.environment('.GlobalEnv')),
     envir = as.environment('.GlobalEnv'))
  if (dev.cur() != 1)  dev.off()
  gc()
  if (resetR) .rs.restartR()
}





