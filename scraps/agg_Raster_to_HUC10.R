#' Raster Aggregation to HUC-10
#'
#'This function streamlines the process of compiling mean raster values at the HUC-10 level using an included, simplified
#'HUC-10 shapefile.  Accepts any datatype accepted by the 'raster' package. NOTE:  Currently, gridded datasets files must
#'have the same number of layers per file.  User can swap in alternate polygonal datasets and set manual, non-NAD83 
#'projections.
#'
#' @param dataPath Character; Path to containing folder of gridded datasets.
#' @param dataName Character; Name of gridded dataset.
#' @param dataExtension Character; Gridded file extension to import, must be compatible with 'raster' package (tif',
#' 'nc', ect).  The .grd format is discouraged due to connection issues that occur when processing long series.
#' @param dataCategory Character; Type of dataset ('precip', 'AET', 'SWE', etc)
#' @param startDate Character; Starting date of rasters in format "YYYY-MM-DD".  DD required but irrelevant if not daily rasters.
#' @param timeStep Character; Time step between layers.  Options are 'day', 'week', 'month', 'quarter', and 'year'.
#' @param MET.HUC10 Logical; If TRUE, uses the included simplified HUC-10 shapefile.  If FALSE,
#' requires poly.fname and poly.layer.  Defaults to TRUE.
#' @param polyFname Character; If MET.HUC10==FALSE, required filename of shapefile to aggregate gridded dataset. Defaults
#' to NULL.
#' @param polyLayer Character; If MET.HUC10==FALSE, required layer of shapefile to aggregate gridded dataset. Defaults to 
#' NULL.
#' @param polyIDs Character string/vector; IDs for polygon to be used as rownames in return values matrix.  Requires 
#' either the identification column (in character string) in the shapefile to be called by polys@data[[polyIDs]] OR a 
#' character vector the length of the shapefile. Defaults to NULL.
#' @param maxBand Integer; Maximum number of bands to process in extract function through 'velox' package.  Defaults to 8.
#' @param maxCore Integer; Maximum number of cores to utilize during parallel processing. Defaults to 10 and will
#' be changed as necessary depending on host machine, leaving one core available.
#' @param percMemory Integer; Maximum percentage of total available R memory to use during processing. Default 
#' of 90 is recommended.  Use the METsteps::systemMemory() function to get accurate system memory usage.
#' @param verbose Logical; If FALSE, suppress all text outputs, including the parallel processing progress bar. Defaults 
#' to TRUE.
#' @param inParallels Logical; If TRUE, runs operation in parallel across maxCores. Defaults to TRUE.
#' @param projManual Character; Projection for datasets.  Gridded data and shapefile projections will be
#' transformed to projManual as necessary. Defaults to NAD83.  For CONUS projects, suggested to leave at default.
#' @export
#' @return Table of mean raster values for each gridded dataset layer.  Columns are gridded layers, rows are shapefile
#' features.
#' @examples
#' agg_Raster_to_HUC10()


agg_Raster_to_HUC10 <- function(dataPath,
                                dataName,
                                dataExtension,
                                dataCategory,
                                startDate,
                                timeStep,
                                MET.HUC10 = TRUE,
                                polyFname = NULL,
                                polyLayer = NULL,
                                polyIDs = NULL,
                                maxBand = 8,
                                maxCore = 20,
                                percMemory = 25,
                                verbose = TRUE,
                                inParallels = TRUE,
                                projManual = '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'){
  
  # dataPath = 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/SampleRasters/'
  # dataExtension = 'tif'
  # dataName = 'SSEBop'
  # dataCategory = 'AET'
  # startDate = '2000-1-1'
  # timeStep = 'month'
  # MET.HUC10 = TRUE
  # polyFname = 'C:/Users/ssaxe/Documents/Scripts/R Scripts/Model Evaluation Tool/shapefiles/mapshaper/HUC10/HUC_no_orph1.shp'
  # polyLayer = 'HUC_no_orph1'
  # polyIDs = NULL
  # maxBand = 8
  # maxCore = 20
  # percMemory = 25
  # verbose = TRUE
  # inParallels = TRUE
  # projManual = '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
  # 
  # gc()
  
  
  # Libraries
  libs.spatial = c('maptools', 'raster', 'rgdal', 'sp', 'rgeos', 'velox')
  libs.misc = c('progress', 'lubridate', 'pbapply')
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
    is.logical(MET.HUC10),
    (is.character(polyFname) || is.null(polyFname)),
    (is.character(polyLayer) || is.null(polyLayer)),
    (is.character(polyIDs) || is.null(polyIDs)),
    is.double(maxBand),
    is.double(maxCore),
    is.double(percMemory),
    is.logical(verbose),
    is.logical(inParallels),
    is.character(projManual)
  )
  if (MET.HUC10 == FALSE){
    stopifnot(
      is.character(polyFname),
      is.character(polyLayer)
    )
  }
  stopifnot(
    timeStep %in% c('day', 'week', 'month', 'quarter', 'year')
  )
  xrm = lapply(X = c(dataPath, dataExtension, dataName, dataCategory, startDate, timeStep, MET.HUC10,
                     polyFname, polyLayer, maxBand, maxCore, percMemory, verbose, inParallels),
               FUN = function(y){stopifnot(length(y) == 1)}); rm(xrm);
  sp::CRS(projManual)
  
  
  # Load/Import shapefile and transform projection if necessary
  if (verbose == T) {writeLines('\nLoading/Importing shapefile')}
  if (MET.HUC10 == TRUE){
    polys = METsteps::HUC10_Shape
  }else if (MET.HUC10 == FALSE){
    polys = rgdal::readOGR(dsn = polyFname,
                           layer = polyLayer,
                           verbose = FALSE,
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
    if (verbose == T) {writeLines('\nTransforming shapefile projection')}
    polys = sp::spTransform(polys, CRS(projManual))
  }
  
  
  # Acquire/List gridded datasets
  if (verbose == T) {writeLines('\nIdentifying gridded datasets')}
  D.names = list.files(path = dataPath,
                       pattern = paste0('\\.', dataExtension, '$'),
                       full.names = TRUE)
  if (verbose == T) {writeLines(paste0('\n(found ', length(D.names), ' unique .', dataExtension, ' files in dataPath)'))}
  D.proj = projection(suppressWarnings(raster(D.names[1])))
  if (is.na(D.proj)){stop('No coordinate reference system associated with rasters.')}
  
  
  # Produce time series for raster
  LayersPerFile = suppressWarnings(nlayers(stack(D.names[1])))
  TotalFiles = length(D.names)
  TotalLayers = LayersPerFile*TotalFiles
  dates.all = seq.Date(from = as.Date(startDate), by = timeStep, length.out = TotalLayers)
  
  
  # Create dataframe of distribution of layers by file
  dz = rep(1:TotalFiles, each = LayersPerFile)
  dzz = as.data.frame(cbind(1:TotalLayers, dz))
  dzz = as.data.frame(cbind(rep(1:LayersPerFile, TotalFiles), dz))
  colnames(dzz) = c('Layer', 'File')
  
  
  # Chunk dataset
  if (verbose == T) {writeLines('\nChunking dataset')}
  split.note = rep(1:ceiling(TotalLayers/maxBand), each = maxBand)
  chunk.list = suppressWarnings(split(dzz, split.note))
  
  
  # Define functions for lapply
  funMean = function(x){return(mean(x, na.rm = T))} #mean function including na.rm for 'velox' extract
  uu <- function(x){
    require(raster); require(velox)
    #Stop if memory overloaded
    if (memory.size() > (memory.limit()*(percMemory/100))){
      stop('MEMORY LIMIT EXCEEDED')
    }
    
    funMean = function(x){return(mean(x, na.rm = T))} #mean function including na.rm for 'velox' extract
    
    # Compile layers as defined in chunk.list into rasterStack
    D.names.cur = D.names[unique(x[,2])]
    D.names.cur.num = cbind(D.names.cur, unique(x[,2]))
    stack.it <- function(D.names.cur.single){
      return(
        raster::stack(D.names.cur.single[1], 
                      bands = x[(x[,2] == as.numeric(D.names.cur.single[2])),1]
                      )
        )
      }
    chunk.stack = raster::stack(apply(D.names.cur.num, MARGIN = 1, stack.it))
    
    # Reproject rasters to NAD83 
    if (raster::projection(chunk.stack) != projManual){
      chunk.stack = raster::projectRaster(chunk.stack, crs = sp::CRS(projManual))
    }
    
    # Crop rasterStack, crop, convert to velox object, and extract by polys object
    chunk.stack = raster::crop(chunk.stack, raster::extent(polys))
    chunk.velox = velox::velox(raster::stack(chunk.stack))
    OUT = chunk.velox$extract(polys, fun=funMean)
    
    # Perform garbage collection to maintain available memory
    gc()
    
    return(OUT)
  }
  
  
  # lapply functions either in parallel or single thread
  if (inParallels == TRUE){
    if (verbose == T) {writeLines('\nAggregating (in parallels)')}
    if (verbose == T) {writeLines('\nCreating cluster')}
    # Open cluster
    no_cores = parallel::detectCores()-1
    if (no_cores > maxCore){no_cores = maxCore}
    cl <- parallel::makeCluster(no_cores)
    # Export variables to nodes
    if (verbose == T) {writeLines('\nExporting variables to nodes')}
    parallel::clusterExport(cl = cl, c('percMemory', 'D.names', 'polys', 'projManual'), envir = environment())
    # lapply uu function in parallels
    if (verbose == T) {writeLines('\nExtracting and aggregating in parallels')}
    results.parList = pbapply::pblapply(chunk.list, uu, cl=cl)
    # Close cluster
    if (verbose == T) {writeLines('\nClosing cluster')}
    parallel::stopCluster(cl)
    gc()
    
  }else{
    if (verbose == T) {writeLines('\nExtracting and aggregating (single threaded')}
    # lapply uu function, single threaded
    results.parList = pbapply::pblapply(chunk.list, uu)
  }
  
  
  # Format results
  RESULTS = do.call(cbind, results.parList)
  d.range = lubridate::decimal_date(dates.all)
  colnames(RESULTS) =  d.range #decimal dates
  if (MET.HUC10 == TRUE){
    rownames(RESULTS) = polys@data[['HUC10']] #HUC ids
  }else if (MET.HUC10 == FALSE){
    if (length(polyIDs) == 1){
      rownames(RESULTS) = polys@data[[polyIDs]] #HUC ids
    }else if (length(polyIDs) > 1){
      rownames(RESULTS) = polyIDs
    }
  }

  
  # Compile information about dataset
  info = c(dataName, dataExtension, dataCategory, timeStep, projManual)
  names(info) = c('dataName', 'dataExtension', 'dataCategory', 'timeStep', 'projManual')
  
  
  # Return
  return(list(HUC10matrix = RESULTS, info = info))
}



