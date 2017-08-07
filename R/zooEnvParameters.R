#' Zoo Envelope Parameters
#'
#' Merge time-series and produces parameters required for zoo envelope plots:  time-series
#' of min/max, 25th/75th percentiles, mean, and median.
#' @param zoo.all List of all time series objects in zoo format. If null, zoo.fnames is required. 
#' @param zoo.fnames List of filenames of objects to import.  Required if zoo.all is NULL.
#' @export
#' @return Numeric vector.
#' @examples
#' zooEnvParameters()

zooEnvParameters <- function(zoo.fnames,
                             timeStep. = 'month'
                             ){
  zoo.fnames = paste0("C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/PointData/ET/New2/",ptSP.trim@data$OurID, '.csv')
  # Load in all files (depends on zoo.all or zoo.fnames)
  imFun <- function(path){
    y <- zoo::read.zoo(file = path)
    # if not zoo obj already...
    if (sum(y, na.rm = T) == 0){
      y <- read.csv(path,
                    header = F,
                    stringsAsFactors = F)
      yy <- zoo::as.zoo(y[,2])
      zoo::index(yy) <- as.Date(y[,1])
      y <- yy
    }
    # fill in missing sequences with NAs
    fD <- seq.Date(from = min(zoo::index(y)),
                   to = max(zoo::index(y)),
                   by = timeStep.)
    fS <- rep(NA, length(fD))
    fS <- zoo::as.zoo(fS)
    zoo::index(fS) <- as.Date(fD)
    # merge
    yM <- merge(y, fS)
    yM <- yM[,1]
    
    #return
    return(yM)
  }
  z <- do.call("merge.zoo", lapply(X = zoo.fnames,
                                   FUN = imFun))
  
  
  ########  Calc ts ranges
  # Define full date sequence as NAs
  indMer <- seq.Date(min(index(z)), max(index(z)), by = 'month')
  indMer2 <- zoo::as.zoo(rep(NA, length(indMer)))
  
  # Identify min/max for full sequence
  zoo::index(indMer2) <- indMer
  zRange <- suppressWarnings(apply(z, MARGIN = 1, FUN = range, na.rm = T))
  zRange[is.infinite(zRange)] <- NA
  
  envMax <- as.zoo(zRange[2,])
  zoo::index(envMax) <- as.Date(colnames(zRange))
  envMax <- merge(envMax, indMer2)[,1]
  
  envMin <- as.zoo(zRange[1,])
  zoo::index(envMin) <- as.Date(colnames(zRange))
  envMin <- merge(envMin, indMer2)[,1]
  
  # Identify quantiles when there are > 2 measurements per date
  quant.25.75 <- function(x){
    x <- as.numeric(x)
    if (sum(!is.na(x)) > 2){
      return(as.numeric(stats::quantile(x, probs = c(0.25, 0.75), na.rm = T)))
    }else{
      return(c(NA, NA))
    }
  }
  
  envQuant <- apply(z, MARGIN = 1, FUN = quant.25.75)
  env75 <- as.zoo(envQuant[2,])
  zoo::index(env75) <- as.Date(colnames(envQuant))
  env75 <- merge(env75, indMer2)[,1]
  
  env25 <- as.zoo(envQuant[1,])
  zoo::index(env25) <- as.Date(colnames(envQuant))
  env25 <- merge(env25, indMer2)[,1]
  
  # Trim NAs
  env75 <- zoo::na.trim(env75)
  env25 <- zoo::na.trim(env25)
  
  #Calculate means of obs
  envMean <- as.zoo(rowMeans(z, na.rm = T))
  zoo::index(envMean) <- as.Date(colnames(envQuant))
  envMean <- merge(envMean, indMer2)[,1]
  #Calculate medians of obs
  envMedian <- as.zoo(apply(z, MARGIN = 1, FUN = median, na.rm = T))
  zoo::index(envMedian) <- as.Date(colnames(envQuant))
  envMedian <- merge(envMedian, indMer2)[,1]
  
  return(list(
    envInput = list(
      xMinMax = as.Date(zoo::index(envMax)),
      yMin = as.numeric(envMin),
      yMax = as.numeric(envMax),
      x2575 = as.Date(zoo::index(env25)),
      y25 = as.numeric(env25),
      y75 = as.numeric(env75)),
    envZoo = list(
      envMin    = envMin,
      envMax    = envMax,
      env25     = env25,
      env75     = env75,
      envMedian = envMedian,
      envMean   = envMean)
  ))
  
}
