#' Number of Cells in Object
#'
#' Function to return total number of cells in objects such as vectors, matrices, dataframes, or lists.
#' @param x Object; Vector, matrix, dataframe, or list object.
#' @export
#' @return Number of cells in object.

nCell = function(x){ #x is vector, matrix/df, list of mats/dfs, or list of lists
  objType = function(x){
    if (is.vector(x) && is.list(x) == FALSE){  #vectors
      return('vector')
    }else if(is.zoo(x) && is.null(dim(x))){  #vectors
      return('vector')
    }else if(is.zoo(x) && is.null(dim(x))==FALSE){
      return('mat/df')
    }else if (is.matrix(x) || is.data.frame(x)){ #matrix/dataframe
      return('mat/df')
    }else if (is.list(x) && is.data.frame(x) == FALSE){ #list
      return('list')
    }else{
      return(NA)
    }
  }
  
  nCell_vector = function(x){return(length(x))}
  nCell_matdf = function(x){return(nrow(x)*ncol(x))}
  nCell_nonList = function(x){
    if (objType(x) == 'vector'){  #vectors
      nCell = nCell_vector(x)
    }else if (objType(x) == 'mat/df'){ #matrix/dataframe
      nCell = nCell_matdf(x)
    }
  }
  
  if (objType(x) != 'list'){  #vectors
    nCell = nCell_nonList(x)
  }else if (objType(x) == 'list'){ #list
    nCell = sum(unlist(lapply(x, nCell_nonList)))
  }
  return(nCell)
}