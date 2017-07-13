#' Rasterize Polygons in Parallels
#'
#' Function to chunk features in shapefile, rasterize in parallels, and assign values. Can be performed not in parallels, but is
#' significantly slower and the user can just as easily use the raster::rasterize() function.
#'
#' @param shp Shapefile to rasterize; Shapefile or path to shapefile.
#' @param mask.raster Raster object; NA-filled raster object or path to raster (NA's will be inserted).
#' @param shpValues The values to be transferred to raster.  Must be single number or vector of numbers the length of 
#' the number of spatial features. If vector of numbers, will be assigned in the same order as the order of features in shapefile.
#' @param cl Cluster object created through parallel::makeCluster(n). Defaults to NULL, in which case function will not be run
#' in parallels and will operate more slowly.
#' @param layer Layer argument if required for importing shapefile through rgdal::readOGR(). Defaults to NULL. Only required if
#' shp is path to file.
#' @param verbose Show progress? Defaults to TRUE.
#' @param plotMerged Plot resulting raster?  Defaults to TRUE, but will change to FALSE if verbose==FALSE.
#' @export
#' @return 
#' features.
#' @examples
#' clRasterize()

clRasterize <- function(shp, mask.raster, shpValues, cl = NULL, layer = NULL, verbose = TRUE, plotMerged = TRUE){
  require(rgdal)
  require(raster)
  library(pbapply)
  library(parallel)
  require(progress)
  
  # Load shapefile
  if (typeof(shp) != 'S4'){
    if (typeof(shp) == 'character'){
      shp <- rgdal::readOGR(dsn = shp,
                            layer = layer,
                            verbose = FALSE,
                            stringsAsFactors = FALSE)
    }else{
      stop("'shp' is not an S4 or character object.")
    }
  }else{
    if (class(shp) != "SpatialPolygonsDataFrame") stop("'shp' is an S4 object but not of class 'SpatialPolygonsDataFrame'.")
  }
  # Load mask.raster
  if (typeof(mask.raster) != 'S4'){
    if (typeof(mask.raster) == 'character'){
      mask.raster <- raster::raster(mask.raster)
      mask.raster[,] <- NA
    }else{
      stop("'mask.raster' is not an S4 or character object.")
    }
  }else{
    if (class(mask.raster) != "RasterLayer") stop("'mask.raster' is an S4 object but not of class 'RasterLayer'.")
  }
  
  #### If running in parallels...
  if (!is.null(cl)){
    if (!('cluster' %in% class(cl))) stop("'cl' is not of class 'cluster'.")
    
    # convert to chunked dataframe with values
    if ((length(shpValues) > 1) && (length(shpValues) != nrow(shp))) stop("Length of 'shpValues' is not equal to 1 or to the number of features in 'shp'.")
    rnms <- 1:nrow(shp)
    rnmsVals <- data.frame(rnsm = rnms,
                           vals = shpValues)
    rnmsVals <- split(rnmsVals, 
                      ceiling(seq_along(rnms)/(length(rnms)/length(cl))))
    
    shp2list <- function(rowNum, shp){
      shpNew <- shp[rowNum$rnsm,]
      shpNew$clRasterizeValues <- rowNum$vals
      return(shpNew)
    }
    
    shpList <- lapply(X = rnmsVals,
                      FUN = shp2list,
                      shp = shp)
    
    # Function to crop and rasterize
    crFun <- function(ss, rr){
      rr <- raster::crop(rr, raster::extent(ss))
      rr <- raster::rasterize(x= ss,
                              y = rr,
                              field = as.numeric(ss$clRasterizeValues))
      return(rr)
    }
    
    # Rasterize in parallels
    if (verbose){
      listRasters <- pbapply::pblapply(X = shpList,
                                       FUN = crFun,
                                       rr = mask.raster,
                                       cl = cl)
    }else{
      listRasters <- parallel::parLapply(cl = cl,
                                         X = shpList,
                                         fun = crFun,
                                         rr = mask.raster)
    }
    
    
    # Merge in loop (will need to figure out how to use do.call() to speed this up)
    mrg <- listRasters[[1]]
    if (verbose){
      pb <- progress_bar$new(
        format = "  downloading [:bar] :percent eta: :eta",
        total = (length(listRasters)-1), clear = FALSE, width = 60)
    }
    for (i in 2:length(listRasters)){
      if (verbose) pb$tick()
      mrg <- raster::merge(mrg, listRasters[[i]])
    }
    
    if (verbose){
      if (plotMerged){
        cat('Plotting rasterized polygons...\n')
        plot(mrg)
      }
    }
    
    return(mrg)
    ###############################################################################################
  }else{
    cat("Not running in parallels (cl == NULL).  Will still chunk datasets and run via lapply. FYI, it's going to take about 15x as long...\n")
    
    # convert to chunked dataframe with values
    if ((length(shpValues) > 1) && (length(shpValues) != nrow(shp))) stop("Length of 'shpValues' is not equal to 1 or to the number of features in 'shp'.")
    rnmsVals <- data.frame(rnsm = 1:nrow(shp),
                           vals = shpValues)
    rnmsVals <- split(rnmsVals,
                      ceiling(seq_along(rnms)/1))
    
    shp2list <- function(rowNum, shp){
      shpNew <- shp[rowNum$rnsm,]
      shpNew$clRasterizeValues <- rowNum$vals
      return(shpNew)
    }
    
    shpList <- lapply(X = rnmsVals,
                      FUN = shp2list,
                      shp = shp)
    
    # Function to crop and rasterize
    crFun <- function(ss, rr){
      rr <- raster::crop(rr, raster::extent(ss))
      rr <- raster::rasterize(x= ss,
                              y = rr,
                              field = as.numeric(ss$clRasterizeValues))
      return(rr)
    }
    
    # Rasterize in parallels
    if (verbose){
      listRasters <- pbapply::pblapply(X = shpList,
                                       FUN = crFun,
                                       rr = mask.raster)
    }else{
      listRasters <- lapply(X = shpList,
                            FUN = crFun,
                            rr = mask.raster)
    }
    
    
    # Merge in loop (will need to figure out how to use do.call() to speed this up)
    mrg <- listRasters[[1]]
    if (verbose){
      pb <- progress_bar$new(
        format = "  downloading [:bar] :percent eta: :eta",
        total = (length(listRasters)-1), clear = FALSE, width = 60)
    }
    for (i in 2:length(listRasters)){
      if (verbose) pb$tick()
      mrg <- raster::merge(mrg, listRasters[[i]])
    }
    
    if (verbose){
      if (plotMerged){
        cat('Plotting rasterized polygons...\n')
        plot(mrg)
      }
    }
    return(mrg)
  }
}