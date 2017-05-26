#' Raster Aggregation to HUC-10
#'
#' This functions streamlines the process of collecting mean raster values at the HUC-10 level.
#' It will accept any raster format natively supported by the 'raster' package.
#' It will automatically reproject the HUC-10 shapefile to match the projection of the supplied rasters.
#' Currently requires the following packages: 'maptools', 'raster', 'rgdal', 'sp', 'rgeos', 'velox', 'doParallel',
#' 'foreach', 'progress', 'lubridate', and 'pbapply'.
#' @param Data.path Path to raster folder
#' @param Data.type Raster file extension.  Options are '.tif', '.nc', and '.bil'.
#' @param start.d Starting date of rasters in format "YYYY-MM-DD".  DD required but irrelevant if not daily rasters.
#' @param step Time step between rasters.  Options are 'day', 'week', 'month', 'quarter', and 'year'.
#' @param huc.path Path to HUC-10 shapefiles.  Defaults to my 'Model Evaluation Tool/shapefiles/mapshaper/HUC10/HUC_no_orph1.shp' location.
#' @param huc.layer Layer name for readOGR function.  Defaults to my layer name 'HUC_no_orph1'.
#' @param resample Logical to determine if to resample rasters.  Defaults to TRUE.  Resamples when resolution < 0.005.
#' @param maxband Maximum number of bands to process in extract function through 'velox' package.  Defaults to 8.
#' @param maxCore Maximum number of cores to utilize during parallel processing.  Defaults to 10.
#' @param perc.mem Maximum percentage of total available R memory to use during processing.  Default of 25 recommended.
#' @param disag Forget what this is for.
#' @param verbose If FALSE, suppress all text outputs, mainly the progress bar. Defaults to TRUE.
#' @param parallel If TRUE, runs operation in parallel across maxCores.
#' @export
#' @return Table of HUC-10 means by HUC.  Time-step x HUC-10.
#' @examples
#' agg_Raster_to_HUC10_Parallel()


#----- Required Files
#  HUC10 shapefile

agg_Raster_to_HUC10_Parallel <- function(Data.path = 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/SampleRasters/',
                                Data.type = '.tif',
                                Data.name = 'SSEBop',
                                start.d = '2000-1-1',
                                step = 'month',
                                huc.path = 'C:/Users/ssaxe/Documents/Scripts/R Scripts/Model Evaluation Tool/shapefiles/mapshaper/HUC10/HUC_no_orph1.shp',
                                huc.layer = 'HUC_no_orph1',
                                resample = TRUE,
                                maxband = 8,
                                maxCore = 10,
                                perc.mem = 25,
                                disag = NULL,
                                verbose = TRUE,
                                parallel = FALSE){

  gc()
  #-------------------------------------------------------------------------------------------------#
  #------- Libraries
  #spatial libraries
  library('maptools'); library(raster); library(rgdal); library(sp); library(rgeos); library(velox);
  #misc
  library(progress); library(lubridate); require(pbapply)
  #parallel libraries
  library(parallel)
  #-------------------------------------------------------------------------------------------------#
  ###################################################################################################
  #-------------------------------------------------------------------------------------------------#
  #------- Test for errors:
  if (verbose == T) {writeLines('Checking inputs')}
  #Make sure all required values are provided:
  vals = c('Data.path', 'Data.type', 'huc.path', 'huc.layer', 'perc.mem', 'start.d', 'end.d', 'step')
  check.existence = function(var.str){if (!(exists(var.str))){stop(paste0(var.str, ' missing'))}}
  
  #Check length of Data.type
  if (length(Data.type) > 1){stop('Data.type should be of Length = 1')}
  
  #Check Data.type
  appr.d = c('.tif', '.nc', '.bil')  #add .bil?
  if (!(Data.type %in% appr.d)){stop(paste0('Data.type not supported (supported: ', toString(appr.d), ')'))}
  
  #Check timestep
  if ((step %in% c('day', 'week', 'month', 'quarter', 'year')) == FALSE){
    stop('Incorrect step supplied')
  }
  
  # 
  #   #Make sure correct memory percentage applied
  #   perc.mem = as.numeric(perc.mem)
  #   if (!(perc.mem > 1 && perc.mem < 80)){stop(paste0(perc.mem, ' is not permissible. Limits are 1-80%'))}
  #-------------------------------------------------------------------------------------------------#
  ###################################################################################################
  #-------------------------------------------------------------------------------------------------#
  #------- Import and reproject HUCs as necessary
  if (verbose == T) {writeLines('\nImporting shapefile')}
  #HUC = readOGR(dsn = huc.path, layer = huc.layer, verbose = FALSE, stringsAsFactors = F)
  #attach shapefile data
  HUC = METsteps::HUC10_Shape
  
  #List names of all files of Data.type in directory
      # D.names = list.files(path = Data.path, pattern = Data.type, full.names = TRUE)
  D.names = list.files(path = Data.path,
                       pattern = paste0('\\', Data.type, '$'),
                       full.names = TRUE)
      # D.names = D.names[extension(D.names) == Data.type]

      #if (Data.type == '.bil'){D.names = D.names[-(seq(1, 183, 13))]}
  
  #Acquire projection of gridded dataset
  D.proj = projection(suppressWarnings(raster(D.names[1])))
  if (is.na(D.proj)){stop('No coordinate reference system associated with rasters.')}
  
  
  # #Reproject HUC as necessary, loading one layer for projection
  # if (D.proj != projection(HUC)){writeLines('\nReprojecting shapefile'); HUC.D = spTransform(HUC, CRS(D.proj))}else {HUC.D = HUC}; rm(HUC)
  HUC.D = HUC
  
  #-------------------------------------------------------------------------------------------------#
  ###################################################################################################
  #-------------------------------------------------------------------------------------------------#
  #Number of layers per file
  LayersPerFile = suppressWarnings(nlayers(stack(D.names[1])))
  #Total number of files
  TotalFiles = length(D.names)
        # #Number of total layers
        # if (LayersPerFile > 50){
        #   TotalLayers = LayersPerFile*TotalFiles
        # }else{
        #   TotalLayers = suppressWarnings(nlayers(stack(D.names)))
        # }
  #Number of total layers
  TotalLayers = LayersPerFile*TotalFiles
  
  #Define time series
  dates.all = seq.Date(from = as.Date(start.d), by = step, length.out = TotalLayers)
  #-------------------------------------------------------------------------------------------------#
  ###################################################################################################
  #-------------------------------------------------------------------------------------------------#
  
  #Location of layers
  z = rep(1:TotalFiles, each = LayersPerFile)
  zz = as.data.frame(cbind(1:TotalLayers, z))
  zz = as.data.frame(cbind(rep(1:LayersPerFile, TotalFiles), z))
  colnames(zz) = c('Layer', 'File')
  
  
  #-------------------------------------------------------------------------------------------------#
  ###################################################################################################
  #-------------------------------------------------------------------------------------------------#
  if (verbose == T) {writeLines('\nAggregating')}
  
  #Split layers into chunks of 20
  #Number of bands/layers per chunk
  split.note = rep(1:ceiling(TotalLayers/maxband), each = maxband)
  chunk.list = suppressWarnings(split(zz, split.note))
  
  #Parallelize raster extract
  funmean = function(x){return(mean(x, na.rm = T))}
  #Create velox stack
  uu <- function(x){
    require(raster)
    require(velox)
    #Stop if memory overloaded
    if (memory.size() > (memory.limit()*(perc.mem/100))){
      stop('MEMORY LIMIT EXCEEDED')
    }
    
    #libs
    funmean = function(x){return(mean(x, na.rm = T))}
    #relevant d.names
    D.names.cur = D.names[unique(x[,2])]
    D.names.cur.num = cbind(D.names.cur, unique(x[,2]))
    stack.it <- function(D.names.cur.single){
      return(
        stack(
          D.names.cur.single[1], 
          bands = x[(x[,2] == as.numeric(D.names.cur.single[2])),1]
        )
      )
    }
    chunk.stack = stack(apply(D.names.cur.num, MARGIN = 1, stack.it))
    if (is.null(disag) == FALSE){
      chunk.stack = disaggregate(chunk.stack, disag)
    }
    
    #Reproject rasters to projection of shapefile (NAD83) 
    if (projection(chunk.stack) != projection(HUC.D)){
      chunk.stack = projectRaster(chunk.stack, crs = CRS(raster::projection(HUC.D)))
    }
    
    chunk.stack = crop(chunk.stack, extent(HUC.D))

    chunk.velox = velox(stack(chunk.stack))
    
    OUT = chunk.velox$extract(HUC.D, fun=funmean); OUT
    # 
    gc()

    return(OUT)
  }
  
  if (parallel == TRUE){
    if (verbose == T) {writeLines('\nCreating Cluster')}
    #Open cluster
    no_cores = detectCores()-1
    if (no_cores > maxCore){no_cores = maxCore}
    cl <- makeCluster(no_cores)
    
    #aply function over chunk list
    if (verbose == T) {writeLines('\nExporting Variables to Nodes')}
    clusterExport(cl = cl, c('perc.mem', 'D.names', 'disag', 'HUC.D'), envir = environment())
    if (verbose == T) {writeLines('\nExtracting in Parallels')}
    results.parList = pblapply(chunk.list, uu, cl=cl)
    
    #Close cluster
    if (verbose == T) {writeLines('\nClosing Cluster')}
    stopCluster(cl)
  }else{
    #Apply function over chunk list
    results.parList = pblapply(chunk.list, uu)
  }
  
  #Format results 
  RESULTS = do.call(cbind, results.parList)
  
  d.range = decimal_date(dates.all)
  colnames(RESULTS) =  d.range #decimal dates
  rownames(RESULTS) = HUC.D@data$HUC10 #HUC ids

  #compile data information
  info = c(Data.name, start.d, step)
  names(info) = c('Data.name', 'Start.d', 'tstep')
  
  return(list(HUC10matrix = RESULTS, info = info))
}
