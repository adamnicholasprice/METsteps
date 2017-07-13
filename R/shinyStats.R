#' Return Dataframe of Stats Functions
#'
#' For use in Shiny server. This function returns information about each of the function names provided
#' in a dataframe.  Function names should be of the format "shinyFun_one_FunctionName",
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
shinyFunExtract   <- function(numD = NULL, pkg = 'METsteps'){
  require(METsteps)
  if (!is.null(numD)){
    if (!(numD %in% c('one', 'two', 'poly'))) stop("Argument 'numD' must be one of the following: NULL, 'one', 'two', or 'poly'.")
  }
  x <- ls('package:METsteps')
  x <- x[substr(x, 1, 9) == 'shinyFun_']
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

#' Lapply version of mean
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
shinyFun_one_Mean   <- function(x){
  return(mean(x))
}

#' Lapply version of NSE
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
shinyFun_two_NSE   <- function(x){
  require(hydroGOF)
  return(NSE(sim   = x[, 1],
             obs   = x[, 2],
             na.rm = T))
}


#' Lapply version of RMSE
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
shinyFun_two_RMSE  <- function(x){
  require(hydroGOF)
  return(rmse(sim   = x[, 1], 
              obs   = x[, 2],
              na.rm = T))
}

#' Lapply version of PBIAS
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
shinyFun_two_Percent_Bias <- function(x){
  require(hydroGOF)
  return(pbias(sim   = x[, 1],
               obs   = x[, 2],
               na.rm = T))
}

#' Lapply version of kendall correlation
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
shinyFun_two_Kendall_Cor <- function(z){
  return(cor(x      = as.numeric(z[, 1]),
             y      = as.numeric(z[, 2]),
             method = 'kendall'))
}

#' Lapply version of spearman correlation
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
shinyFun_two_Spearman_Cor <- function(z){
  return(cor(x      = as.numeric(z[, 1]),
             y      = as.numeric(z[, 2]),
             method = 'spearman'))
}

#' Lapply version of difference
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
shinyFun_two_Difference   <- function(z){
  return(mean(x     = (z[, 1] - z[, 2]),
              na.rm = T))
}

#' Lapply version of peak function
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
shinyFun_two_Peak_Timing  <- function(x){
  require(zoo)
  #Split into wateryears
  OCT <- which(regexpr(pattern = 'Oct',
                       text    = zoo::yearmon(index(x))) == 1)
  OCT <- c(1, OCT, (nrow(x)+1))
  dd  <- matrix(data = NA,
                nrow = (length(OCT) - 1),
                ncol = 2)
  for (i in 1:(length(OCT) - 1)){
    wy.sub   <- x[OCT[i]:(OCT[i+1] - 1), ]
    dd[i, 1] <- base::which.max(wy.sub[, 1])
    dd[i, 2] <- base::which.max(wy.sub[, 2])
  }
  return(mean(dd[, 1] - dd[, 2]))
}

#' Lapply version of KS test
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
shinyFun_two_KS_Test    <- function(z){
  if (nrow(na.omit(z)) == 0){
    val <- NA
    return(val)
  } else{
    val <- (suppressWarnings(
      ks.test(x = as.numeric(z[, 1]),
              y = as.numeric(z[, 2]))))$statistic
    return(val)
  }
}

#' Lapply Version of Standard Deviation
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
shinyFun_two_Standard_Dev    <- function(z){
  return(mean(apply(X      = z,
                    MARGIN = 1,
                    FUN    = sd,
                    na.rm  = T)))
}

#' Lapply Version of Standard Deviation
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
shinyFun_poly_Standard_Dev    <- function(z){
  return(mean(apply(X      = z,
                    MARGIN = 1,
                    FUN    = sd,
                    na.rm  = T)))
}


#' Lapply Version of Coefficient of Variation
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
shinyFun_two_CV    <- function(z){
  #require(raster)
  return(mean(apply(X      = z,
                    MARGIN = 1,
                    #FUN    = raster::cv,
                    FUN    = function(x, na.rm = T){return(
                      (sd(x, na.rm = T)/mean(x, na.rm = T))*100
                    )
                    },
                    na.rm  = T)))
}

#' Lapply Version of Coefficient of Variation
#'
#' For use in Shiny app
#' @export
#' @return Numeric vector.
shinyFun_poly_CV    <- function(z){
  #require(raster)
  return(mean(apply(X      = z,
                    MARGIN = 1,
                    #FUN    = raster::cv,
                    FUN    = function(x, na.rm = T){return(
                      (sd(x, na.rm = T)/mean(x, na.rm = T))*100
                    )
                    },
                    na.rm  = T)))
}
