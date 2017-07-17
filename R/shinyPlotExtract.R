#' Return Dataframe of Plotting Functions
#'
#' For use in Shiny server. This function returns information (dataframe or list) about each of the plotting
#' function names.  Function names should be of the format "shinyFun_one_FunctionName",
#' "shinyFun_two_FunctionName", "shinyFun_poly_FunctionName", where "one", "two", and "poly" tell the app 
#' how many input datasets the function can handle. If function can handle two or more, copy/paste function 
#' with new names.  "FunctionName" is the name of the function as it will appear in the Shiny interface. 
#' Any underscores after c("one", "two", "poly") will be converted to spaces.
#' @param numD Character, optional; If set to one of c('one', 'two', 'poly'), returns a list of functions for 
#' that number of input datasets names with formal names ("FunctionName").  If set to NULL, returns dataframe
#' of all 'shinyFun_' functions. Defaults to NULL.
#' @param pkg Character; Which package to look in for function names?  Defaults to "METsteps".
#' @export
#' @return Numeric vector.
#' @examples
#' shinyFunExtract()
shinyPlotExtract   <- function(numD = NULL, pkg = 'METsteps'){
  require(METsteps)
  if (!is.null(numD)){
    if (!(numD %in% c('HUC'))) stop("Argument 'numD' must be one of the following: NULL, or 'HUC'.")
  }
  x <- ls('package:METsteps')
  x <- x[substr(x, 1, 10) == 'shinyPlot_']
  if (length(x) > 0){
    retData <- function(y){
      z <- strsplit(x     = y,
                    split = '_',
                    fixed = T)[[1]][-1]
      sz <- z[1]
      rm <- z[-1]
      if (length(rm) == 1){
        nm <- rm
      }else{
        nm <- paste0(rm, collapse = ' ')
      }
      return(c(sz, nm, y))
    }
    allData <- lapply(X = x,
                      FUN = retData)
    allData <- do.call(rbind.data.frame, c(allData, stringsAsFactors = F))
    colnames(allData) <- c('numDataSets', 'FormalName', 'FunctionName')
    if (is.null(numD)){
      return(allData)
    }else{
      subsetData <- allData[allData$numDataSets == numD,]
      listData <- as.list(subsetData$FunctionName)
      names(listData) <- subsetData$FormalName
      return(listData)
    }
  }else{
    return(NA)
  }
}
