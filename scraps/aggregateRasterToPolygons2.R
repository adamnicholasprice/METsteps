#' Aggregate Time Series Rasters to Polygons
#'
#'Aggregates time series rasters by polygon through the 'raster', 'velox', 'rgdal', and 'sp' packages.  Includes a specialized 
#'function for chunking data. Effectively allows the user to take advantage of the 1000x speed increases (over 'raster')
#'resulting from the 'velox' package without the memory issues caused by R's in-memory processing. Accepts any datatype
#'compatible with the 'raster' package. NOTE: Currently, gridded dataset files must have equal number of layers per file. 
#'Defaults to a NAD83 projection and transforms as necessary.
#'
#' @param dataPath Character, required; Path to containing folder of gridded datasets.
#' @param dataName Character, required; Name of gridded dataset.
#' @param dataExtension Character, required; Gridded file extension to import, must be compatible with 'raster' package (tif',
#' 'nc', ect).  The .grd format is discouraged due to connection issues that occur during processing.
#' @param dataCategory Character, required; Type of dataset ('precip', 'AET', 'SWE', etc).
#' @param startDate Character, required; Starting date of rasters in format "YYYY-MM-DD".  DD required but irrelevant if not daily rasters.
#' @param timeStep Character, required; Time step between layers.  Options are 'day', 'week', 'month', 'quarter', and 'year'.
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
#' @param projManual Character, optional; Projection for datasets.  Gridded data and shapefile projections will be
#' transformed to projManual as necessary. For CONUS projects, suggested to leave at default. Defaults to NAD83 
#' ('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0').
#' @export
#' @return Table of mean raster values for each gridded dataset layer.  Columns are gridded layers, rows are shapefile
#' features.
#' @examples
#' aggregateRasterToPolygons2()


aggregateRasterToPolygons2 <- function(dataPath,
                                dataName,
                                dataExtension,
                                dataCategory,
                                startDate,
                                timeStep,
                                MET.HUC10 = TRUE,
                                polyFname = NULL,
                                polyLayer = NULL,
                                polyIDs = NULL,
                                maxLayers = 6,
                                cushion = 3,
                                verbose = TRUE,
                                cl = NULL,
                                projManual = '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'){
  
  # dataPath = 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/SampleRasters/'
  # dataPath = 'C:/Users/ssaxe/Documents/Scripts/R Scripts/Model Evaluation Tool/raster/monthlyETa/monthlyETa/SSEBopMonthly/'
  # dataExtension = 'tif'
  # dataName = 'SSEBop'
  # dataCategory = 'AET'
  # startDate = '2000-1-1'
  # timeStep = 'month'
  # MET.HUC10 = TRUE
  # polyFname = 'C:/Users/ssaxe/Documents/Scripts/R Scripts/Model Evaluation Tool/shapefiles/mapshaper/HUC10/HUC_no_orph1.shp'
  # polyLayer = 'HUC_no_orph1'
  # polyIDs = NULL
  # maxLayers = 6
  # cushion = 3
  # verbose = TRUE
  # cl = makeCluster(20)
  # cl = cl
  # projManual = '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'

  gc()
  
  
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
    (is.numeric(maxLayers)),
    is.numeric(cushion),
    is.logical(verbose),
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
                     polyFname, polyLayer, maxLayers, cushion, verbose),
               FUN = function(y){stopifnot(length(y) == 1)}); rm(xrm);
  sp::CRS(projManual)
  
  # Load/Import shapefile and transform projection if necessary
  if (verbose) {writeLines('\nLoading/Importing shapefile')}
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
  if (verbose) {writeLines('\nIdentifying gridded datasets')}
  D.names = list.files(path = dataPath,
                       pattern = paste0('\\.', gsub('.', '', dataExtension, fixed = T), '$'),
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
  
  # Max possible distribution
  no_cores = length(cl)
  TotalLayers/no_cores
  maxSplit = suppressWarnings(
    split(dzz, ceiling(seq_along(dzz[,1])/(nrow(dzz)/no_cores)))
    )
  
  
  # Use gdalwarp to reproject rasters as necessary
  if (verbose) {writeLines('\nTransforming raster projections')}
  applyWarp2 <- function(layerFileinfo, filenames, desiredProjection){
    applyWarp <- function(layerFileinfo, filenames, desiredProjection){
      require(gdalUtils); require(raster);
      fileNo = unique(layerFileinfo$File)
      srcfile = filenames[fileNo]
      ras = gdalUtils::gdalwarp(srcfile,
                                dstfile = tempfile(fileext = '.tif'),
                                t_srs=desiredProjection,
                                output_Raster=TRUE,
                                overwrite=TRUE,
                                r = "bilinear",
                                verbose=FALSE)
      ras = raster::subset(ras, layerFileinfo$Layer)
      return(ras)
    }
    layerFileinfo = split(layerFileinfo, layerFileinfo$File)
    return(
      lapply(layerFileinfo, applyWarp, filenames=filenames, desiredProjection=desiredProjection)
    )
  }
  applyWarp3 <- function(filename, desiredProjection){
    require(gdalUtils); require(raster);
    saveName = tempfile(fileext = '.tif')
    ras = gdalUtils::gdalwarp(srcfile = filename,
                              dstfile = saveName,
                              t_srs=desiredProjection,
                              output_Raster=TRUE,
                              overwrite=TRUE,
                              r = "bilinear",
                              verbose=FALSE)
    return(ras)
  }
  # projList2 = pblapply(X = D.names, FUN = applyWarp3, desiredProjection = projManual, cl = cl)
  
  projList1 = pblapply(X = maxSplit, FUN = applyWarp2, filenames = D.names, desiredProjection = projManual, cl = cl)
  projList2 = unlist(projList1)
  
  maxLayers = floor(maxLayers)

  splitRas = suppressWarnings(
    split(projList2, ceiling(seq_along(projList2)/maxLayers))
  )
    
  
  # define extraction function
  applyVxExtract <- function(listRas, polys){
    require(velox); require(raster)
    vx = velox::velox(raster::stack(listRas))
    ex = vx$extract(polys, function(x){return(mean(x, na.rm = T))})
    rm(vx); gc();
    return(
      ex
    )
  }
  
  if (verbose) {writeLines('\nExtracting at polygons')}
  vxList = pblapply(X = splitRas, FUN = applyVxExtract, polys = polys, cl = cl)
  #Split by 1 = 9.5 min
  #Split by 2 = 5m 3 s 
  #Split by 3 = 4m 11s
  #Split by 4 = 3m 15s
  #Split by 5 = 2m 29s
  #Split by 6 = 2m 21s
  #Split by 7 = 2m 23s
  #Split by 8 = 3m 08s
  
  # Use gdalwarp to reproject rasters as necessary
  # vxWarpExtract = function(x, polys, projManual){
  #   # libs
  #   require(raster); require(velox); require(gdalUtils)
  #   #Import and transform gridded dataset
  #   fList = split(x, x$File)
  #   applyWarp <- function(layerFileinfo, filenames, desiredProjection){
  #     require(gdalUtils); require(raster);
  #     fileNo = unique(layerFileinfo$File)
  #     srcfile = filenames[fileNo]
  #     ras = gdalUtils::gdalwarp(srcfile,
  #                               dstfile = tempfile(fileext = '.tif'),
  #                               t_srs=desiredProjection,
  #                               output_Raster=TRUE,
  #                               overwrite=TRUE,
  #                               verbose=FALSE)
  #     ras = raster::subset(ras, layerFileinfo$Layer)
  #     return(ras)
  #   }
  #   applyWarp2 <- function(filename, desiredProjection){
  #     require(gdalUtils); require(raster);
  #     ras = gdalUtils::gdalwarp(filename,
  #                               dstfile = tempfile(fileext = '.tif'),
  #                               t_srs=desiredProjection,
  #                               output_Raster=TRUE,
  #                               overwrite=TRUE,
  #                               verbose=FALSE)
  #     return(ras)
  #   }
  #   tic()
  #   xxx = parLapply(cl = cl, X = D.names, fun = applyWarp2, desiredProjection = projManual)
  #   toc()
  #   yyy = stack(xxx)
  #   
  #   
  #   testtest = lapply(fList, FUN = applyWarp, filenames = D.names, desiredProjection = projManual) #1.88 min
  #   rStack = stack(testtest)
  #   
  #   # transform polygon projection as necessary
  #   if(projection(polys)!=projManual){polys=sp::spTransform(polys, CRS(projManual))}
  #   
  #   # Convert to velox object
  #   vx = velox(rStack) #0.55 min
  #   # extract means at polys
  #   ex1 = vx$extract(sp = polys, fun = function(x){return(mean(x, na.rm = T))}) #1.28 min
  #   # clear remaining vars
  #   rm(vx); rm(rStack); rm(polys); gc();
  #   return(
  #     ex1
  #   )
  # }
  # 
  
  
  
  
  
  
  
  
  ###########################################################################
  # New memory usage adaptation
#   
#   Define extraction function
#   vxExtraction = function(x, polys, projManual){
#     # libs
#     require(raster); require(velox); require(gdalUtils)
#     # gridded datasets
#     uniqueFile = unique(x$File)
#     for (i in 1:length(uniqueFile)){
#       y = x[x$File == uniqueFile[i],]
#       if (exists('rStack') == FALSE){
#         rStack = stack(D.names[uniqueFile[i]], bands = y$Layer)
#       }else{
#         rStack = stack(rStack,
#                        stack(D.names[uniqueFile[i]], bands = y$Layer))
#         rStack = addLayer(rStack,
#                           stack(D.names[uniqueFile[i]], bands = y$Layer))
#       }
#     }
#     # project as necessary
#     tic(); if(projection(rStack)!=projManual){rStack=raster::projectRaster(rStack, crs=CRS(projManual))}; toc()
#     if(projection(polys)!=projManual){polys=sp::spTransform(polys, CRS(projManual))}
# 
#     #gdalwarp test
#     require(gdalUtils)
# 
#     gdal_setInstallation()
#     valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
#     if(require(raster) && require(rgdal) && valid_install)
#     {
#       # Example from the original gdal_translate documentation:
#       src_dataset <- system.file("external/tahoe_highrez.tif", package="gdalUtils")
#       src_dataset <- "C:/Users/ssaxe/Documents/Scripts/R Scripts/Model Evaluation Tool/raster/Precip/Maurer_daily/gridded_obs.daily.Prcp.1950.nc"
#       src_dataset <- "C:/Users/ssaxe/Documents/Projects/Model Evaluation/SampleRasters/m0004.modisSSEBopET.tif"
#       # Command-line gdalwarp call:
#       # gdalwarp -t_srs '+proj=utm +zone=11 +datum=WGS84' raw_spot.tif utm11.tif
#       gdalwarp(src_dataset,dstfile="tahoe_highrez_utm11.tif",
#                t_srs='+proj=utm +zone=11 +datum=WGS84',output_Raster=TRUE,
#                overwrite=TRUE,verbose=TRUE)
#       tic(); nn = gdalwarp(src_dataset,dstfile="deleteme1.tif",
#                            t_srs=projManual,output_Raster=TRUE,
#                            overwrite=TRUE,verbose=TRUE, r = 'bilinear',
#                            layer = 1); toc()
#     }
# 
# 
#     # Convert to velox object
#     vx = velox(rStack)
#     # extract means at polys
#     ex1 = vx$extract(sp = polys, fun = function(x){return(mean(x, na.rm = T))})
#     # clear remaining vars
#     rm(vx); rm(rStack); rm(polys); gc();
#     return(
#       ex1
#     )
#   }
# 
#   vxWarpExtract = function(x, polys, projManual){
#     # libs
#     require(raster); require(velox); require(gdalUtils)
#     #Import and transform gridded dataset
#     fList = split(x, x$File)
#     applyWarp <- function(layerFileinfo, filenames, desiredProjection){
#       require(gdalUtils); require(raster);
#       fileNo = unique(layerFileinfo$File)
#       srcfile = filenames[fileNo]
#       ras = gdalUtils::gdalwarp(srcfile,
#                                 dstfile = tempfile(fileext = '.tif'),
#                                 t_srs=desiredProjection,
#                                 output_Raster=TRUE,
#                                 overwrite=TRUE,
#                                 verbose=FALSE)
#       ras = raster::subset(ras, layerFileinfo$Layer)
#       return(ras)
#     }
#     applyWarp2 <- function(filename, desiredProjection){
#       require(gdalUtils); require(raster);
#       ras = gdalUtils::gdalwarp(filename,
#                                 dstfile = tempfile(fileext = '.tif'),
#                                 t_srs=desiredProjection,
#                                 output_Raster=TRUE,
#                                 overwrite=TRUE,
#                                 verbose=FALSE)
#       return(ras)
#     }
#     tic()
#     xxx = parLapply(cl = cl, X = D.names, fun = applyWarp2, desiredProjection = projManual)
#     toc()
#     yyy = stack(xxx)
#     
#     
#     testtest = lapply(fList, FUN = applyWarp, filenames = D.names, desiredProjection = projManual) #1.88 min
#     rStack = stack(testtest)
# 
#     # transform polygon projection as necessary
#     if(projection(polys)!=projManual){polys=sp::spTransform(polys, CRS(projManual))}
#     
#     # Convert to velox object
#     vx = velox(rStack) #0.55 min
#     # extract means at polys
#     ex1 = vx$extract(sp = polys, fun = function(x){return(mean(x, na.rm = T))}) #1.28 min
#     # clear remaining vars
#     rm(vx); rm(rStack); rm(polys); gc();
#     return(
#       ex1
#     )
#   }
#   
#   applyWarp2 <- function(filename, desiredProjection){
#     require(gdalUtils); require(raster);
#     ras = gdalUtils::gdalwarp(filename,
#                               dstfile = tempfile(fileext = '.tif'),
#                               t_srs=desiredProjection,
#                               output_Raster=TRUE,
#                               overwrite=TRUE,
#                               verbose=FALSE)
#     return(ras)
#   }
#   
#   rStack = pblapply(X = D.names, FUN = applyWarp2, desiredProjection = projManual, cl = cl)
#   rStack = stack(rStack)
#   
#   vxExtract2 = function(rStacksub, polys){
#     require(raster); require(velox);
#     
#     vx = velox(rStacksub)
#     ex1 = vx$extract(sp = polys, fun = function(x){return(mean(x, na.rm = T))})
#     rm(vx);
#     return(ex1)
#   }
#   
# 
#   #memory used to extract from one raster
#   tic()
#   startMem = systemMemory()$MemoryInUse
#   rmx= vxWarpExtract(x = dzz[1,], polys = polys, projManual = projManual)
#   endMem = systemMemory()$MemoryInUse
#   memCost = endMem - startMem; rm(endMem); rm(startMem)
#   hardCodeMult = 3.5
#   toc()
#   # approximately 3.5x increase over "memCost" increase
#   # so, if we want to estimate total chunking:
#   # number of cores * CX * memCost * 3.5 = memory usage
#   
#   # known:
#   # noCores = length(cl)
#   # memCost (estimate beforehand)
#   # 3.5 (hardcode)
#   # required memory usage = systemMemory()$FreeMemory - cushion (say, 3)
#   
#   # so equation is:  CX = memoryusage/(noCores * memCost * 3.5)
#   # result when cushion = 3:
#   CX = (systemMemory()$FreeMemory - cushion)/(length(cl)*memCost*3.5)
#   # round down for system safety
#   CX = floor(CX)
#   print(CX)
#   
#   #Chunk data
#   chunks = split(dzz, ceiling(seq_along(1:nrow(dzz))/CX)); length(chunks)
#   chunks = split(dzz, seq(nrow(dzz)))
#   
#   # Export variables to nodes
#   if (verbose == T) {writeLines('\nExporting variables to nodes')}
#   parallel::clusterExport(cl = cl, c('D.names'), envir = environment())
#   
#   require(pbapply)
#   if (verbose == T) {writeLines('\nExtracting in parallels')}
#   results.parList = pblapply(X = chunks, FUN = vxWarpExtract, polys = polys, projManual = projManual, cl = cl)
# # 
# #   # Identify memory usage per layer
# #   predictVxMem <- function(x, multiplier = 1){
# #     allMem = sum(file.info(x)$size)*multiplier
# #     return(allMem/1000000000)
# #   }
# #   predMem = predictVxMem(D.names) #in GB
# #   avMemPerLayer = predMem/TotalLayers #in GB
# #   
# #   # Maximum number of layers to be processed at a time, given a 3 GB cushion
# #   freeMemory = (METsteps::systemMemory()$FreeMemory)-cushion
# #   maxLayers = floor(freeMemory/avMemPerLayer)
# #   
# #   #Operate in parallels?
# #   if (!is.null(cl)){
# #     inParallels = TRUE
# #   }else{
# #     inParallels = FALSE
# #   }
# #   
# #   # Define maximum number of layers per core
# #   if (inParallels == TRUE){
# #     # Define number of cores to use if operating in parallels
# #     no_cores = length(cl)
# #     maxLayerChunks = floor(maxLayers/no_cores)
# #     divLayerChunks = ceiling(TotalLayers/no_cores)
# #     if (divLayerChunks > maxLayerChunks){divLayerChunks = maxLayerChunks}
# #   }else{
# #     no_cores = 1
# #     divLayerChunks = floor(maxLayers/no_cores)
# #   }
# #   
# #   # Divide dataset layers into chunks, with each chunk containing nLayers = layerChunks
# #   if (verbose == T) {writeLines('\nChunking dataset')}
# #   split.note = rep(1:ceiling(TotalLayers/divLayerChunks), each = divLayerChunks)
# #   chunk.list = suppressWarnings(split(dzz, split.note))
# # 
# #     # # Chunk dataset
# #     # if (verbose == T) {writeLines('\nChunking dataset')}
# #     # split.note = rep(1:ceiling(TotalLayers/maxLayers), each = maxLayers)
# #     # chunk.list = suppressWarnings(split(dzz, split.note))
# #   
# #   # Define functions for lapply
# #   funMean = function(x){return(mean(x, na.rm = T))} #mean function including na.rm for 'velox' extract
# #   uu <- function(x){
# #     require(raster); require(velox)
# #     #Stop if memory overloaded and ask if user wants to restart
# #     if (METsteps::systemMemory()$FreeMemory <= cushion){
# #       stop(paste0('Memory limit exceeded (Free memory < ', cushion, 'GB)'))
# #       askRestartR()
# #     }
# #     
# #     funMean = function(x){return(mean(x, na.rm = T))} #mean function including na.rm for 'velox' extract
# #     
# #     # Compile layers as defined in chunk.list into rasterStack
# #     D.names.cur = D.names[unique(x[,2])]
# #     D.names.cur.num = cbind(D.names.cur, unique(x[,2]))
# #     stack.it <- function(D.names.cur.single){
# #       return(
# #         raster::stack(D.names.cur.single[1], 
# #                       bands = x[(x[,2] == as.numeric(D.names.cur.single[2])),1]
# #         )
# #       )
# #     }
# #     chunk.stack = raster::stack(apply(D.names.cur.num, MARGIN = 1, stack.it))
# #     
# #     # Reproject rasters to NAD83 
# #     if (raster::projection(chunk.stack) != projManual){
# #       chunk.stack = raster::projectRaster(chunk.stack, crs = sp::CRS(projManual))
# #     }
# #     
# #     # Crop rasterStack, crop, convert to velox object, and extract by polys object
# #     chunk.stack = raster::crop(chunk.stack, raster::extent(polys))
# #     chunk.velox = velox::velox(raster::stack(chunk.stack))
# #     OUT = chunk.velox$extract(polys, fun=funMean)
# #     
# #     # Perform garbage collection to maintain available memory
# #     gc()
# #     
# #     return(OUT)
# #   }
# #   
# #   
# #   # lapply functions either in parallel or single thread
# #   if (inParallels){
# #     if (verbose == T) {writeLines('\nAggregating (in parallels)')}
# #     # Export variables to nodes
# #     if (verbose == T) {writeLines('\nExporting variables to nodes')}
# #     parallel::clusterExport(cl = cl, c('cushion', 'D.names', 'polys', 'projManual'), envir = environment())
# #     # lapply uu function in parallels
# #     if (verbose == T) {writeLines(paste0('\nExtracting and aggregating in parallels.  Sending ',
# #                                          divLayerChunks, ' layers per node.'))}
# #     results.parList = pbapply::pblapply(chunk.list, uu, cl=cl)
# #     gc()
# #   }else{
# #     if (verbose == T) {writeLines('\nExtracting and aggregating (single threaded')}
# #     # lapply uu function, single threaded
# #     results.parList = pbapply::pblapply(chunk.list, uu)
# #   }
# #   
  
  # Format results
  RESULTS = do.call(cbind, vxList)
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



