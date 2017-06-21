#' HRU Table Aggregate to HUC-10
#'
#' This function aggregates HRU values in table format to HUC-10 values.  It produces the same list output as the 
#' aggregateRasterToPolygons() function and output can be fed directly into the downscaleHUC10 function.
#' 
#' @param HRUtable Data.frame of the HRU data to be converted to HUC-10.  Each row = HRU, each column = time step.
#' Column names should be in either decimal format (produced through lubridate::decimal_date) or date format
#' (YYYY-MM-DD).  DD is required but irrelevant for time steps monthly or greater.
#' @param dateFormat Character; String providing the date format. Either 'decimal' or 'date'.
#' @param dataName Character; Name of dataset.
#' @param dataCategory Character; Category of dataset (AET, precip, etc).
#' @param timeStep Character; Step between measurements (day, month, quarter, annual, etc).
#' @param startDate Character; Start date of measurements in 'YYYY-MM-DD' string format.
#' @export
#' @return Table of HUC-10 means by HUC.  Time-step x HUC-10.
#' @examples
#' aggHRUTableToHUC10()

aggHRUTableToHUC10 <- function(HRUtable,
                               dateFormat = c('decimal', 'string'),
                               dataName,
                               dataCategory,
                               timeStep,
                               startDate){
  require(lubridate)
  require(pbapply)
  require(maptools)
  
  # Input error handling
  lengthTest <- function(x){if (length(x) != 1) stop(paste(deparse(substitute(x)), 'not of length == 1'))}
  rmx = lapply(X   = c(dateFormat, dataName, dataCategory, timeStep, startDate),
               FUN = lengthTest); rm(rmx)
  if ((dateFormat %in% c('decimal', 'string')) == FALSE) stop('dateFormat should be either decimal or string')
  
  # Load METsteps conversion objects
  HUC10.info <- (METsteps::HUC_IDs)$HUC10.info
  convers <- METsteps::HRU_to_HUC10_percData
  
  # Function to lapply to HUC10.info list
  conv_via_HUC10info <- function(HUC10.ID, HRUtable, conversions = convers){
    # Filter to current HUC10
    convers.sub           <- conversions[conversions$HUC10 == as.numeric(HUC10.ID), ]
    # Calculate weights
    convers.sub$weights   <- convers.sub$PERCENTAGE/sum(convers.sub$PERCENTAGE)
    # Append HUC values
    HRUtable.sub          <- HRUtable[convers.sub$HRUid, ]
    # Weight and sum by date
    HRUtable.sub.weighted <- colSums(as.matrix(HRUtable.sub) * convers.sub$weights)
    # Return weighted values
    return(HRUtable.sub.weighted)
  }
  
  # Perform lapply to get HUC10 values
  result <- pbapply::pblapply(X           = HUC10.info,
                              FUN         = conv_via_HUC10info,
                              HRUtable    = HRUtable,
                              conversions = convers) #rt = 04m06s
  
  # Format results
  result.mat <- do.call(rbind, result)
  rownames(result.mat) <- HUC10.info
  
  # Define endDate
  if (dateFormat == 'decimal'){
    endDate <- lubridate::date_decimal(as.numeric(colnames(result.mat)[ncol(result.mat)]))
  }else if (dateFormat == 'date'){
    endDate <- colnames(result.mat)[ncol(result.mat)]
  }
  endDate <- substr(x = endDate,
                    start = 1,
                    stop = regexpr(' ', endDate) - 1)
  
  # Compile metadata
  info <- c(dataName,
            dataCategory,
            timeStep,
            startDate,
            endDate)
  names(info) <- c('dataName', 'dataCategory', 'timeStep', 'startDate', 'endDate')
  
  #Return
  return(list(HUC10matrix = result.mat,
              info = info))
}