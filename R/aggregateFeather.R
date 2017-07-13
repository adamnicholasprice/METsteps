#' Aggregate Time Series Data in Feather Format
#'
#' Aggregate already-processed feather files to a lower-resolution time step (eg. daily to monthly).
#' 
#' @param fNames Character; Feather filenames to aggregate.
#' @param fPath Character; Path to folder containing feather files. If working directory is set to containing
#' folder, can be left as NULL.
#' @param to Character; Target timestep to aggregate to.  Should be either 'month' or 'year'.
#' @param fun Function; Function by which to compute summary statistics. Defaults to sum.
#' @param writeFeather Logical; Should new feather files be written? Defaults to FALSE.
#' @param reorder Logical; Should datasets be reordered by name and HUC level? Defaults to TRUE.
#' @param returnOriginals Logical; Return the original feather files in list?
#' @export
#' @return List of aggregated feather files.  Name of each list item is the updated filename, accessed through names(x),
#' where x is the returned list.  Original feather files included if returnOriginals == TRUE.
#' @examples
#' aggregateFeather()

aggregateFeather <- function(fNames,
                             fPath = NULL,
                             to = c('month', 'year'),
                             fun = sum,
                             writeFeather = FALSE,
                             reorder = TRUE,
                             returnOriginals = FALSE){
  require(METsteps)
  require(raster)
  
  fileInfo <- extractMetadata(fNames)
  
  # Check to make sure target timeStep is lower resolution that origin timestep
  order <- c('day', 'month', 'year')
  order.pos <- which(order == to)
  if (length(order.pos) != 1) stop("'to' must be one of c('day', 'month', 'year').")
  
  testOrder <- function(x, order) return(which(order == x)) 
  if (any(unlist(lapply(X     = fileInfo$timeStep,
                        FUN   = testOrder,
                        order = order)) >= order.pos)) {
    stop ("At least one of the supplied feather files has a timestep equal to or lower than the value supplied in the 'to'
          argument. Order of timesteps is c('hourly', 'day', 'month', 'year').")
  }
  
  # Add forward slash to fPath as necessary
  if (!is.null(fPath)) {
    if (METsteps::substrRight(x = fPath) != '/'){fPath = paste0(fPath, '/')}
  }
  
  # Reorder fNames by HUC
  if (reorder){
    orderMat <- (METsteps::extractMetadata(fNames))[,c('dataName', 'HUC')]
    fNames <- fNames[order(orderMat$dataName, orderMat$HUC)]
  }
  
  # Function to aggregate with
  aggFun <- function(x, to, fun, fPath){
    z <- zoo(feather::read_feather(paste0(fPath, x)))
    mDat <- METsteps::extractMetadata(x)
    index(z) <- seq.Date(from       = as.Date(mDat$startDate),
                         by         = mDat$timeStep,
                         length.out = nrow(z))
    # Define index vector
    if (to == 'year'){
      by <- lubridate::year(index(z))
    }else if (to == 'month'){
      by <- zoo::as.yearmon(index(z))
    }
    
    # Aggregate
    newZ <- aggregate(x = z,
                      by = by,
                      FUN = sum)
    index(newZ) <- as.Date(paste0(index(newZ), '-01-01'))
    
    # return result
    return(newZ)
  }
  
  #Aggregate
  rr <- pbapply::pblapply(X = fNames,
                          FUN = aggFun,
                          to = to,
                          fun = fun,
                          fPath = fPath)
  #Create new names
  splitOld <- strsplit(fNames, '_', fixed = T)
  rmExt <- function(x){
    return(
      c(x[1:5],
      unlist(strsplit(x[6], '.', fixed = T))[1])
      )
  }
  splitOld <- lapply(X = splitOld,
                     FUN = rmExt)
  
  extractSdates <- function(x){return(as.character(zoo::index(x)[1]))}
  extractEdates <- function(x){return(as.character(zoo::index(x)[nrow(x)]))}
  
  newStartDates <- (unlist(lapply(X   = rr,
                                        FUN = extractSdates)))
  newEndDates <- (unlist(lapply(X   = rr,
                                      FUN = extractEdates)))
  # Create new filenames
  fNames.new <- vector()
  for (i in 1:length(splitOld)){
    newName <- splitOld[[i]]
    newName[4] <- to
    newName[5] <- newStartDates[i]
    newName[6] <- newEndDates[i]
    newName <- paste(newName, collapse = '_')
    newName <- paste0(newName, '.feather')
    fNames.new <- c(fNames.new, newName)
  }
  
  #assign Fnames.new as list names
  names(rr) <- fNames.new
  
  # write feather files if requested
  if (writeFeather){
    for (i in 1:length(rr)){
      feather::write_feather(rr[[i]], path = paste0(fPath, names(rr)[i]))
    }
  }
  
  if (returnOriginals){
    cat('Returning results and original datasets\n')
    retFun <- function(x){
      z <- zoo(feather::read_feather(x))
      mDat <- METsteps::extractMetadata(x)
      index(z) <- seq.Date(from       = as.Date(mDat$startDate),
                           by         = mDat$timeStep,
                           length.out = nrow(z))
      return(z)
    }
    
    oo <- lapply(X = paste0(fPath, fNames),
                 FUN = retFun)
    names(oo) <- fNames
    
    return(list(results = rr,
                originals = oo))
  }else{
    cat('Returning results datasets\n')
    return(rr)
  }
}
