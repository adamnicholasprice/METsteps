#' Filename Extraction and Creation
#'
#' Function to return total number of cells in objects such as vectors, matrices, dataframes, or lists.  dataName, HUC, timestep, and 
#' startDate can be lists as long as matching lengths.  If any filenames supplied, other inputs ignored.
#' @param fnames Character; List of filenames of feather files from which to extract data name, HUC number, timestep, and starting date.
#' @param dataName Character; Name of dataset.
#' @param HUC Character or numeric; Number of HUC unit of dataset.
#' @param timeStep Character; Time step between measurements.  Can be 'day', 'week', 'month', 'year', etc.
#' @param startDate Character; Starting date for measurements in 'YYYY-M-D' format.  Set 'D' to 1 for time steps > weekly.
#' @param infoReturn Character; Data to return when extracting from filenames.  List of objects 'dataName', 'HUC', 'timeStep', and 'startDate'.
#' Defaults to returning all data.
#' @param returnList Logical; Option whether to return data in list format or not.  Defaults to FALSE, not returning a list.
#' @export
#' @return Number of cells in object.

filenames <- function(fnames = NULL,
                      dataName = NULL, HUC = NULL, timeStep = NULL, startDate = NULL,
                      infoReturn = c('dataName', 'HUC', 'timeStep', 'startDate'), returnList = FALSE){
  if (is.null(fnames)){ #if no filename supplied, create filename(s)
    if (is.null(dataName) || is.null(HUC) || is.null(timeStep) || is.null(startDate)){
      stop('Missing information for dataName, HUC, timestep, or startDate.')
    }
    lengths.inputs = c(length(dataName), length(HUC), length(timeStep), length(startDate))
    if (length(unique(lengths.inputs)) > 1){
      stop('Unequal lengths of inputs for dataName, HUC, timeStep, or startDate.')
    }
    #Create filename(s)
    startDate = gsub('-', '_', startDate)
    if (returnList == TRUE){
      return(as.list(paste0(dataName, '__HUC', HUC, '_', timeStep, '__', startDate, '.feather')))
    }else{
      return(paste0(dataName, '__HUC', HUC, '_', timeStep, '__', startDate, '.feather'))
    }
  }else{  #Extract info from filenames
    extracted = vector('list')
    if ('dataName' %in% infoReturn){extracted$dataName = METsteps::extractDataName(fnames)}
    if ('HUC' %in% infoReturn){extracted$HUC = METsteps::extractHUCnumber(fnames)}
    if ('timeStep' %in% infoReturn){extracted$timeStep = METsteps::extractTimeStep(fnames)}
    if ('startDate' %in% infoReturn){extracted$startDate = METsteps::extractStartingDate(fnames)}
    
    if (returnList == TRUE){
      return(extracted)
    }else{
      if (length(infoReturn) == 1){
        return(as.vector(unlist(extracted)))
      }else{
        return(data.frame(extracted))
        }
      }
  }
}