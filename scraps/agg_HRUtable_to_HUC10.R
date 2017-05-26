#' HRU Table Aggregate to HUC-10
#'
#' This function aggregates HRU values in table format to HUC-10 values.  It produces the same list output as the 
#' agg_Raster_to_HUC10 function and output can be fed directly into the downscale_HUC10 function.
#' Currently requires the following packages: ___.
#' @param HRUtable Data.frame of the HRU data to be converted to HUC-10.  Each row = HRU, each column = time step.
#' Column names should be in either decimal format (produced through lubridate::decimal_date) or date format
#' (YYYY-MM-DD).  DD is required but irrelevant for time steps monthly or greater.
#' @param Data.name The name to identify the dataset.
#' @param Date.format Character string providing the date format. Either 'decimal' or 'date'.
#' @param step Character string identifying the time step of the HRUtable data. Options are 'day', 'week',
#' 'month', 'quarter', and 'year'.
#' @export
#' @return Table of HUC-10 means by HUC.  Time-step x HUC-10.
#' @examples
#' agg_HRUtable_to_HUC10()

agg_HRUtable_to_HUC10 <- function(HRUtable,
                                  Data.name,
                                  Date.format = c('decimal', 'date'),
                                  step = c('day', 'week', 'month', 'quarter', 'year')){
  #required datasets:  HUC10.info (from HUC10 shapefile), convers
  require(lubridate); require(dplyr); require(tibble); require(pbapply); require(maptools);

  #import data
      # HUC10.info = readShapePoly('C:/Users/ssaxe/Documents/Scripts/R Scripts/Model Evaluation Tool/shapefiles/mapshaper/HUC10/HUC_no_orph1.shp',
      #                            )@data[,14]
      # HUC10.info = rmFactors(sort(HUC10.info))
  HUC10.info = (METsteps::HUC_IDs)$HUC10.info
      # convers = read.csv('C:/Users/ssaxe/Documents/Scripts/R Scripts/Model Evaluation Tool/tabintnew/tabintall.csv',
      #                    header = T, stringsAsFactors = F)[,c(2,3,5)]
  convers = METsteps::HRU_to_HUC10_percData
  
  # results = as_tibble(matrix(NA, nrow = length(HUC10.info), ncol = ncol(HRUtable)+1))
  # #add column names as dates in decimal format
  # dates.temp = colnames(HRUtable)
  # if (Date.format == 'decimal'){
  #   colnames(results) = c('HUC10', dates.temp)
  # }else if (Date.format == 'date'){
  #   dates.temp = as.Date(dates.temp)
  #   dates.temp = decimal_date(dates.temp)
  #   colnames(results) = c('HUC10', dates.temp)
  # }
  # #Add HUC10 ids
  # results$HUC10 = HUC10.info
  # 
  #Function to lapply to HUC10.info list
  conv_via_HUC10info <- function(HUC10.ID, HRUtable){
    #filter to current HUC10
    convers.sub = filter(convers, HUC10 == as.numeric(HUC10.ID))
    #Calculate weights
    convers.sub$weights = convers.sub$PERCENTAGE/sum(convers.sub$PERCENTAGE)
    #Append HUC values
    HRUtable.sub = HRUtable[convers.sub$HRUid,]
    #Weight and sum by date
    HRUtable.sub.weighted = colSums(as.matrix(HRUtable.sub) * convers.sub$weights)
    #return weighted values
    return(HRUtable.sub.weighted)
  }
  
  #Perform lapply to get HUC10 values
  result = pblapply(HUC10.info, conv_via_HUC10info, HRUtable)
  result.mat = do.call(rbind, result)
  #result.df = as.data.frame(result.mat)
  rownames(result.mat) = HUC10.info

  #Compile info
  
  info = c(Data.name,
           as.character(as.Date(date_decimal(as.numeric(colnames(result.mat)[1])))),
           step)
  names(info) = c('Data.name', 'Start.d', 'tstep')
  
  #Return
  return(list(HUC10matrix = result.mat,
              info = info))
  
}