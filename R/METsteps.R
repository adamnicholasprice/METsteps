#' Model Evaluation Tool steps: A package for simplifying the process of water balance component product evaluation.
#'
#' The METsteps package provides functions to streamline the processing of gridded datasets in parallels through the 'sp',
#' 'rgdal', 'raster', 'gdalUtils', and 'velox' packages.  The 'velox' package is key in extracting cells by polygon, in that
#' it operates ~150x faster than the 'raster' package and ~4x faster than the python 'rasterstats' module, though it is affected
#' by the effects of in-memory processing.
#'
#' @docType package
#' @name METsteps
#' @examples
#' library(METsteps)
#' library(parallel)
#' 
#' # Start timing
#' tic()
#' 
#' # Initialize cluster
#' cl <- makeCluster(25)
#' 
#' # Aggregate to HUC-10
#' x <- aggregateRasterToPolygons(dataPath      = 'C:/.../SSEBop/',
#'                                dataName      = 'SSEBop',
#'                                dataExtension = 'tif',
#'                                dataCategory  = 'AET',
#'                                startDate     = '2000-01-01',
#'                                timeStep      = 'month',
#'                                MET.HUC10     = TRUE,
#'                                cl            = cl)
#'
#' # Downscale to HUCs 2-8 and create zoo objects
#' y <- downscaleHUC10(x)
#' 
#' # Save to folder to access via Shiny app
#' zooHUCtoFeather(zoo.obj    = y,
#'                 Shiny.path = 'C:/.../folder/')
#' 
#' # Close cluster
#' stopCluster(cl)
#' 
#' # Report run time
#' toc()
NULL