#' Coin of data set
#'
#' Returns both the head and tail of dataset
#' @param x Vector, matrix, dataframe, or list.
#' @export
#' @return Head and tail of x.

coin = function(x){
  cat('head: \n')
  print(head(x))
  cat('tail: \n')
  print(tail(x))
}


#' Short head of object
#'
#' Returns a more limited head (~10 col) of the object.
#' @param x Vector, matrix, dataframe, or list.
#' @export
#' @return Head of x.

shead = function(x, n_col = 1:10){
  if (is.null(dim(x))){
    cat(paste0('(length: ', length(x), ')','\n'))
    print(head(x))
  }
  if (!(is.null(dim(x)))){
    if (ncol(x) < 10){
      #cat(paste0('(ncol: ', ncol(x), ')', '  ', '(nrow: ', nrow(x), ')', '\n'))
      cat(paste0('[', nrow(x), ',', ncol(x), ']', '\n'))
      print(head(x))
    }else{
      #cat(paste0('(ncol: ', ncol(x), ')', '  ', '(nrow: ', nrow(x), ')', '\n'))
      cat(paste0('[', nrow(x), ',', ncol(x), ']', '\n'))
      print(head(x[,n_col]))
    }
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








