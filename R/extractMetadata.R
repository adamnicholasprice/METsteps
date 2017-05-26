#' Extract Metadata From File Name
#'
#' Function to extract data from feather filenames to be used in METsteps Shiny application
#' @param METname List/vector of feather file names.
#' @param df Logical; If TRUE, return results as dataframe object. Otherwise, returns list object.  Defaults to TRUE.
#' @export
#' @return List of filenames for zoo objects.
#' @examples
#' extractMetadata(zoo.obj)

extractMetadata <- function(METname, df = TRUE){
  if (length(METname >= 1) && (sum(is.na(METname)) == 0)){
    info <- strsplit(x     = METname,
                     split  = '_',
                     fixed  = T)
    nameItandFixIt <- function(x){
      x         <- as.data.frame(x                = t(x),
                                 stringsAsFactors = F)
      names(x)  <- c('dataCategory', 'dataName', 'HUC', 'timeStep', 'startDate', 'endDate')
      x$HUC     <- as.numeric(gsub('HUC', '', x$HUC))
      x$endDate <- gsub(pattern     = '.feather',
                        replacement = '',
                        x           = x$endDate,
                        fixed       = T)
      return(x)
    }
    info <- lapply(info, nameItandFixIt)
    if (df){
      info2        <- data.frame(matrix(data  = unlist(info),
                                        nrow  = length(info),
                                        byrow = T), 
                                 stringsAsFactors = FALSE)
      names(info2) <- names(info[[1]])
      info2$HUC    <- as.numeric(info2$HUC)
    }else{
      info2        <- info
    }
    
    return(info2)
  }else if (sum(is.na(METname)) > 0) {
    return(NA)
  }else {
    return(NA)
  }
  
}
