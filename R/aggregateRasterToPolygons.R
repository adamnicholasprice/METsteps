#' Aggregate Time Series Rasters to Polygons
#'
#'Aggregates time-series rasters by polygon primarily through the 'velox' package. Allows the user to take advantage of the
#'1000x raster extraction speed increases (over 'raster') resulting from the 'velox' package and should be operated in parallels.
#'Accepts any datatype compatible with the 'raster' package. Defaults to a NAD83 projection and transforms as necessary. Datasets
#'are assumed to be in depth per measurement and results are calculated as length = mm.
#'
#' @param dataPath Character, required; Path to containing folder of gridded datasets.
#' @param dataName Character, required; Name of gridded dataset.
#' @param dataExtension Character, required; Gridded file extension to import, must be compatible with 'raster' package (tif',
#' 'nc', ect).  The .grd format is discouraged due to connection issues that occur during processing.
#' @param dataCategory Character, required; Type of dataset ('precip', 'AET', 'SWE', etc).
#' @param startDate Character, required; Starting date of rasters in format "YYYY-MM-DD".  DD required but irrelevant if not daily rasters.
#' @param timeStep Character, required; Time step between layers.  Options are 'day', 'week', 'month', 'quarter', and 'year'.
#' @param unitDepth Character, required; String providing the unit measurement of depth of dataset.  Converts all datasets to mm.
#' Defaults to 'mm'.  Supported formats supplied in ?measurements::conv_unit.
#' @param aggFUN Character, required; Function to use on cells within each polygon.  Options are c('mean', 'sum').  Use 'mean' 
#' on rates and 'sum' on volumes. Defaults to 'mean'.
#' @param MET.HUC10 Logical, optional; If TRUE, uses the included METsteps simplified HUC-10 shapefile.  If FALSE,
#' requires polyFname, polyLayer, and polyIDs arguments. Defaults to FALSE.
#' @param polyFname Character, optional; If MET.HUC10 is FALSE, required filename of shapefile to aggregate gridded dataset. 
#' Defaults to NULL.
#' @param polyLayer Character; If MET.HUC10 is FALSE, required layer of shapefile to aggregate gridded dataset as per 
#' rgdal::readOGR() function. Defaults to NULL.
#' @param polyIDs Character string/vector; If MET.HUC10 is FALSE, required IDs for polygons to be used as rownames in 
#' return matrix.  Requires either the identification column (in character string) in the shapefile (to be called by 
#' polys@data[[polyIDs]]) OR a character vector equal in length to the number of features. Defaults to NULL.
#' @param maxLayers Integer, optional; Maximum number of layers/bands to allocate to each chunk.  Defaults to 6. If you are
#' experiencing memory issues (overloading RAM), then drop this number or reduce number of nodes in cl.
#' @param cushion Numeric, optional; Number of GB of memory to leave unused during processing.  Strongly advised to not decrease
#' lower than 2.  Default is 3.
#' @param verbose Logical, optional; If FALSE, suppress all text outputs, including the parallel processing progress bar. Defaults 
#' to TRUE.
#' @param cl Cluster object, optional; If supplied, extraction run in parallels.  Suggested max number of cores is 10.
#' @param disag Logical; Allow resampling if necessary.  Defaults to TRUE.  Resampling occurs when (average cell size of dataset) x
#' 4 > average size of polygons in shapefile.
#' @param cancelReproject Logical; If FALSE, reprojection is forced if necessary.  Set to TRUE to disable raster reprojection.
#' Defaults to FALSE. Suggested for use when WGS84 is projection and extents align with included datasets (polyHUC2 for example).
#' @param recursive Logical; Should the listing recurse into directories? Defaults to TRUE.
#' @param projManual Character, optional; Projection for datasets to be converted to.  Gridded data and shapefile projections will be
#' transformed to projManual as necessary. For CONUS projects, suggested to leave at default. Defaults to NAD83 
#' ('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0').
#' @export
#' @return Table of mean raster values for each gridded dataset layer.  Columns are gridded layers, rows are shapefile
#' features.
#' @examples
#' aggregateRasterToPolygons()


aggregateRasterToPolygons <- function(dataPath,
                                dataName,
                                dataExtension,
                                dataCategory,
                                startDate,
                                timeStep,
                                unitDepth = 'mm',
                                aggFUN = 'mean',
                                MET.HUC10 = TRUE,
                                polyFname = NULL,
                                polyLayer = NULL,
                                polyIDs = NULL,
                                maxLayers = 6,
                                cushion = 3,
                                verbose = TRUE,
                                cl = NULL,
                                disag = TRUE,
                                cancelReproject = FALSE,
                                recursive = TRUE,
                                projManual = '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'){
  
  dataPath = "C:/Users/ssaxe/Documents/Scripts/R Scripts/Model Evaluation Tool/raster/AET/SSEBopMonthly"
  dataName = 'MOD16-A2'
  dataExtension = 'tif'
  dataCategory = 'AET'
  startDate = '2000-1-1'
  timeStep = 'month'
  unitDepth = 'mm'


  # # dataPath = "C:/Users/ssaxe/Documents/Scripts/R Scripts/Model Evaluation Tool/raster/Storage/Grace-CSR/"
  # # dataName = 'GRACE-CSR'
  # # dataExtension = 'tif'
  # # dataCategory = 'Storage'
  # # startDate = '2002-04-01'
  # # timeStep = 'month'
  # # unitDepth = 'mm'
  aggFUN = 'mean'
  MET.HUC10 = TRUE
  polyFname = NULL
  polyLayer = NULL
  polyIDs = NULL
  maxLayers = 6
  cushion = 3
  verbose = TRUE
  projManual = '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
  disag = TRUE
   cl = parallel::makeCluster(5)
  
  # Libraries
  libs.spatial = c('maptools', 'raster', 'rgdal', 'sp', 'rgeos', 'velox')
  libs.misc = c('progress', 'lubridate', 'pbapply', 'measurements')
  libs.parallel = c('parallel')
  xrm = lapply(c(libs.spatial, libs.misc, libs.parallel), require, character.only = TRUE); rm(xrm);
  
  
  # Check inputs and error handling
  stopifnot(
    is.character(dataPath),
    is.character(dataExtension),
    is.character(dataName),
    is.character(dataCategory),
    is.character(startDate),
    is.character(timeStep),
    is.character(unitDepth),
    (tolower(aggFUN) %in% c('mean', 'sum')),
    is.logical(MET.HUC10),
    (is.character(polyFname) || is.null(polyFname)),
    (is.character(polyLayer) || is.null(polyLayer)),
    (is.character(polyIDs) || is.null(polyIDs)),
    (is.numeric(maxLayers)),
    is.numeric(cushion),
    is.logical(verbose),
    is.logical(disag),
    is.character(projManual)
  )
  if (MET.HUC10 == FALSE){
    stopifnot(
      is.character(polyFname),
      is.character(polyLayer)
    )
  }
  
  stopifnot(timeStep %in% c('day', 'week', 'month', 'quarter', 'year'))
  
  if (grepl('_', dataName, fixed = T)){stop('Can not have an underscore (_) in dataName. Please use a dash (-) instead.')}
  
  xrm = lapply(X <- c(dataPath, dataExtension, dataName, dataCategory, startDate, timeStep, aggFUN,
                      MET.HUC10, polyFname, polyLayer, maxLayers, cushion, verbose),
               FUN = function(y){
                 stopifnot(length(y) == 1)
                 })
  rm(xrm)
  sp::CRS(projManual)
  aggFUN = tolower(aggFUN)
  
  # Load/Import shapefile and transform projection if necessary
  if (verbose) writeLines('Loading/Importing shapefile')
  if (MET.HUC10){
    polys <- METsteps::HUC10_Shape
  }else {
    polys <- rgdal::readOGR(dsn             = polyFname,
                           layer            = polyLayer,
                           verbose          = FALSE,
                           stringsAsFactors = FALSE)
    if (length(polyIDs) < 1){
      stop('length(polyIDs) < 1')
    } else if (length(polyIDs) == 1){
      if (!(polyIDs %in% colnames(polys@data))){
        stop('polyIDs not found as column name in shapefile')
      }
    } else if (length(polyIDs) > 1){
      if (length(polyIDs != nrow(polys@data))){
        stop('length(polyIDs) != number of shapefile features')
      }
    }
  }
  if (raster::projection(polys) != projManual){
    if (verbose == T) writeLines('Transforming shapefile projection')
    polys = sp::spTransform(polys, CRS(projManual))
  }
  
  
  # Acquire/List gridded datasets
  if (verbose) writeLines('Identifying gridded datasets')
  D.names <- list.files(path       = dataPath,
                       pattern     = paste0('\\.', gsub('.', '', dataExtension, fixed = T), '$'),
                       full.names  = TRUE,
                       recursive = recursive)
  if (verbose) writeLines(paste0('Found ', length(D.names), ' unique .', dataExtension, ' files in dataPath'))
  D.proj <- projection(suppressWarnings(raster(D.names[1])))
  if (is.na(D.proj)) stop('No coordinate reference system associated with rasters.')
  # Retrieve datum of dataset projection
  if (!cancelReproject){
    .br <- unlist(strsplit(x     = D.proj,
                           split = ' ',
                           fixed = T))
    .br <- .br[grepl(pattern = '+datum',
                     x       = .br,
                     fixed   = T)]
    .br <- (unlist(strsplit(x     = .br,
                            split = '=',
                            fixed = T)))[2]
    if (length(.br) > 0){
      if (.br == 'WGS84' && projManual == "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") {
        warning(paste('Projection of', dataName, 'has datum of WGS84. Transformation of this datum to NAD83 is implemented incorrectly in GDAL. Please convert projection to GCS_NAD83 in ArcGIS, QGIS, or other GIS software.'))
      } else if (.br == 'WGS84'){
        warning(paste('Projection of', dataName, 'has datum of WGS84.  If your dataset requires a transformation, GDAL does not correctly transform the WGS84 datum to other GCS projections and will result in NA values.  Please convert projection in ArcGIS, QGIS, or other GIS software.'))
      }
    }else{
      warning('No datum information (+datum) in projection of dataset.')
    }
  }

  # Produce time series for raster
  LayersPerFile <- suppressWarnings(nlayers(stack(D.names[1])))
  TotalFiles <- length(D.names)
  TotalLayers <- LayersPerFile*TotalFiles
  dates.all <- seq.Date(from       = as.Date(startDate),
                       by          = timeStep,
                       length.out  = TotalLayers)
  
  
  # Create dataframe of distribution of layers by file
  dz <- rep(1:TotalFiles, each = LayersPerFile)
  dzz <- as.data.frame(cbind(1:TotalLayers, dz))
  dzz <- as.data.frame(cbind(rep(1:LayersPerFile, TotalFiles), dz))
  colnames(dzz) <- c('Layer', 'File')
  
  # Max possible distribution
  no_cores <- length(cl)
  maxSplit <- suppressWarnings(
    split(dzz,ceiling(seq_along(dzz[,1])/(nrow(dzz)/no_cores))))
  
  # Use gdalWarp to reproject rasters in parallels as necessary
  maxSplit <- suppressWarnings(
    split(D.names,ceiling(seq_along(D.names)/(length(D.names)/no_cores))))
  
  if (cancelReproject == FALSE){
    if (verbose) writeLines('Reprojecting as necessary')
    applyWarp3 <- function(fnameList, desiredProjection, dataPath, assignProj){
      # Wrapper for applyWarp function to be used in parallel lapply function.
      #
      # Args:
      #   fnameList: Vector/list of full filenames (paths) to transform.
      #   desiredProjection: Target projection for raster transform.
      #
      # Returns:
      #   List of lists of reprojected rasterStacks.
      applyWarp <- function(fnameSingle, desiredProjection, dataPath, assignProj){
        # Wrapper for gdalUtils::gdalwarp function to be used in applyWarp3 lapply function.
        #
        # Args:
        #   fnameSingle: Single full filename (path) to raster.
        #   desiredProjection: Target projection for raster transform.
        #
        # Returns:
        #   List reprojected rasterStacks.
        require('gdalUtils')
        require('raster')
        require('ncdf4')
        dstfile <- tempfile(tmpdir = dataPath, fileext = '.tif')
        # dstfile <- gsub('\\', '', dstfile, fixed = T)
        file.create(dstfile)
        if (is.null(assignProj)){
          gdalSourceProj <- projection(raster(fnameSingle))
        }else{
          gdalSourceProj <- assignProj
        }
        ras <- gdalUtils::gdalwarp(srcfile       = fnameSingle,
                                   dstfile       = dstfile,
                                   s_srs         = gdalSourceProj,
                                   t_srs         = desiredProjection,
                                   output_Raster = FALSE,
                                   overwrite     = TRUE,
                                   r             = "bilinear",
                                   verbose       = FALSE)
        ras <- gdalUtils::gdalwarp(srcfile       = fnameSingle,
                                   dstfile       = dstfile,
                                   s_srs         = projection(raster(fnameSingle)),
                                   t_srs         = desiredProjection,
                                   output_Raster = FALSE,
                                   overwrite     = TRUE,
                                   r             = 'bilinear',
                                   verbose       = TRUE)
        #ras <- as.list(raster::stack(x = dstfile))
        return(dstfile)
      }
      return(lapply(X = fnameList, FUN = applyWarp, desiredProjection = desiredProjection, dataPath = dataPath))
    }
    projList1 <- pbapply::pblapply(X                  = maxSplit,
                                   FUN                = applyWarp3,
                                   desiredProjection  = projManual,
                                   dataPath           = dataPath,
                                   cl                 = cl)
    projList1 <- unlist(projList1)
    projList2 <- raster::stack(projList1)
  }else {
    if (verbose) writeLines('Passing over reprojection step as defined in argument .cancelReproject.')
    projList2 <- suppressWarnings(raster::stack(D.names))
    if (!is.null(cl)){
      
      D.names.split <- split(D.names,
                             ceiling(seq_along(1:length(D.names))/(length(D.names)/length(cl)))
                             )
      projList2 <- raster::stack(pbapply::pblapply(X = D.names.split,
                                                    FUN = raster::stack,
                                                    cl = cl))
    }
    
  }

  
  # projList2 <- as.list(projList2)  # return a list of rasterStacks
  maxLayers <- floor(maxLayers)
      # splitRas  <- suppressWarnings(
      #   split(projList2, ceiling(seq_along(projList2)/maxLayers)))
  splitRas  <- suppressWarnings(
    split(1:nlayers(projList2), ceiling(seq_along(1:nlayers(projList2))/maxLayers)))
  
  # Compute average cell size (if projection is unprojected aka "longlat")
  if (disag){
    exRas <- projList2[[1]]
    prj <- unlist(strsplit(x     = raster::projection(exRas),
                           split = ' ',
                           fixed = T))
    prj <- prj[grepl(pattern = 'proj',
                     x       = prj)]
    prj <- unlist(strsplit(x     = prj,
                           split = '=',
                           fixed = T))[2]
    if (prj == 'longlat'){
      rArea <- raster::cellStats(x = area(exRas),
                                 stat = 'mean')
      sArea <- mean(area(polys)/1000000)
      if ((rArea*4) > sArea){
        resFact <- ceiling((rArea*4)/sArea)
        }else{
        resFact <- 1
        }
    }else{
      resFact <- 1
    }
  }else{
    resFact <- 1
  }
  
  # define extraction function
  # applyVxExtract <- function(x, sp, aggFUN, disagFactor){
  #   # Wrapper for velox raster extraction by polygon to be used in lapply function.
  #   #
  #   # Args:
  #   #   x: List of rasters created through raster package.
  #   #   sp: A SpatialPolygons object at which to average raster pixels.
  #   #   fun: function to use on cells.  Should be a mean or a sum function.
  #   #
  #   # Returns:
  #   #   Mean or sum values within sp for each layer in x.
  #   require(velox)
  #   require(raster)
  # 
  #   st <- raster::stack(x)
  #   st <- raster::crop(x = st,
  #                      y = extent(sp))
  #   st <- raster::disaggregate(x    = st,
  #                              fact = disagFactor)
  #   vx <- velox::velox(raster::stack(st))
  #   if (aggFUN == 'mean') {
  #     vfun   <- function(x){return(mean(x, na.rm = T))}
  #   }else if (aggFUN == 'sum'){
  #       vfun <- function(x){return(sum(x, na.rm = T))}
  #   }else{
  #       stop('aggFUN must be either mean or sum.')
  #   }
  #   ex <- vx$extract(sp   = sp,
  #                    fun  = vfun)
  #   rm(st)
  #   rm(vx)
  #   gc()
  #   return(ex)
  # }
  applyVxExtract <- function(x, sp, mainStack, aggFUN, disagFactor){
    # Wrapper for velox raster extraction by polygon to be used in lapply function.
    #
    # Args:
    #   x: List of rasters created through raster package.
    #   sp: A SpatialPolygons object at which to average raster pixels.
    #   fun: function to use on cells.  Should be a mean or a sum function.
    #
    # Returns:
    #   Mean or sum values within sp for each layer in x.
    require(velox)
    require(raster)
    st <- mainStack[[x]]

      # st <- raster::stack(x)
    st <- raster::crop(x = st,
                       y = extent(sp))
    st <- raster::disaggregate(x    = st,
                               fact = disagFactor)
    vx <- velox::velox(raster::stack(st))
    if (aggFUN == 'mean') {
      vfun   <- function(x){return(mean(x, na.rm = T))}
    }else if (aggFUN == 'sum'){
      vfun <- function(x){return(sum(x, na.rm = T))}
    }else{
      stop('aggFUN must be either mean or sum.')
    }
    ex <- vx$extract(sp   = sp,
                     fun  = vfun)
    rm(st)
    rm(vx)
    gc()
    return(ex)
  }
  
  if (verbose && resFact == 1) writeLines('Extracting at polygons')
  if (verbose && resFact != 1) writeLines('Extracting at polygons and disaggregating to ~4 cells/polygon')
  # vxList <- pblapply(X           = splitRas,
  #                    FUN         = applyVxExtract,
  #                    sp          = polys,
  #                    aggFUN      = aggFUN,
  #                    disagFactor = resFact,
  #                    cl          = cl)
  vxList <- pbapply::pblapply(X           = splitRas,
                              FUN         = applyVxExtract,
                              sp          = polys,
                              mainStack   = projList2,
                              aggFUN      = aggFUN,
                              disagFactor = resFact,
                              cl          = cl)
  gc()
  
  # Format results
  RESULTS <- do.call(cbind, vxList)
  d.range <- lubridate::decimal_date(dates.all)
  colnames(RESULTS) <- d.range #decimal dates
  if (MET.HUC10 == TRUE){
    rownames(RESULTS) <- polys@data[['HUC10']] #HUC ids
  }else if (MET.HUC10 == FALSE){
    if (length(polyIDs) == 1){
      rownames(RESULTS) <- polys@data[[polyIDs]] #HUC ids
    }else if (length(polyIDs) > 1){
      rownames(RESULTS) <- polyIDs
    }
  }
  
  # Convert units
  if (verbose && unitDepth != 'mm') writeLines(paste('Converting units from', unitDepth, 'to mm'))
  RESULTS <- RESULTS * measurements::conv_unit(x    = 1,
                                                 from = unitDepth,
                                                 to   = 'mm')
  
  # Create endDate object
  endDate <- date_decimal(as.numeric(colnames(RESULTS)[length(colnames(RESULTS))]))
  endDate <- substr(x = endDate,
                    start = 1,
                    stop = regexpr(' ', endDate) - 1)
  
  # Remove temporary files
  if (exists('projList1')) file.remove(projList1)
  
  # Compile information about dataset
  info <- c(dataName, dataExtension, dataCategory, timeStep, startDate, endDate, projManual)
  names(info) <- c('dataName', 'dataExtension', 'dataCategory', 'timeStep', 'startDate', 'endDate', 'projManual')
  
  gc()
  # Return
  return(list(HUC10matrix = RESULTS, info = info))
}



