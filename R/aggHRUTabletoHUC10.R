#' Aggregate HRU Table to HUC-10
#'
#' This function aggregates HRU values in table format to HUC-10 values.  It produces the same list output as the 
#' aggregateRasterToPolygons() function and output can be fed directly into the downscaleHUC10 function.
#' 
#' @param HRUfolder Character, required; Path to folder containing HRU region files.  Files should be in order by region 
#' from r01 to r18.
#' @param dataName Character, required; Name of gridded dataset.
#' @param dataExtension Character, required; Gridded file extension to import, must be compatible with 'raster' package 
#' (tif', 'nc', ect). The .grd format is discouraged due to connection issues that occur during processing.
#' @param dataCategory Character, required; Type of dataset ('precip', 'AET', 'SWE', etc).
#' @param startDate Character, optional; Starting date of rasters in format "YYYY-MM-DD". DD required but irrelevant 
#' if not daily rasters.  Defaults to "auto".  PRMS files should include a date column in each table.  If startDate is set to 
#' "auto", identify the date column by number if it is different than 1.
#' @param timeStep Character, required; Time step between layers. Options are 'day', 'week', 'month', 'quarter', and 'year'.
#' Defaults to day.
#' @param ext Character, optional; File extension.  Defaults to 'csv'.
#' @param sep Character, optional; Field seperator character. Defaults to 'auto'.
#' @param header Logical, optional; Do files have headers? Defaults to TRUE.
#' @param dateColumn Integer, required; If startDate == "auto", which column in the tables contains the dates?
#' @param sortNames Logical, optional; Should files names be sorted alphabetically? Defaults to TRUE.
#' @param verbose Logical, optional; Should progress updates be printed to console?
#' @export
#' @return Table of mean raster values for each gridded dataset layer. Columns are gridded layers, rows are shapefile features.
#' @examples
#' aggHRUTableToHUC10(HRUfolder    = "C:/Path/To/PRMS/Folder",
#'                    dataName     = "PRMS",
#'                    dataCategory = "Recharge")
#' 
aggHRUTableToHUC10 <- function(HRUfolder, 
                               dataName,
                               dataCategory,
                               startDate = 'auto',
                               timeStep = 'day',
                               ext = 'csv',
                               sep = 'auto',
                               header = TRUE,
                               dateColumn = 1,
                               sortNames = T,
                               verbose = T){
  # HRUfolder <- 'C:/Users/ssaxe/Documents/Scripts/R Scripts/Model Evaluation Tool/raster/Recharge/PRMS'
  
  if (startDate == 'auto') {
    if (is.numeric(dateColumn) == FALSE){
      stop("If startDate is 'auto', dateColumn must be defined as a positive integer.")
    }else{
      if (dateColumn < 1){
        stop("dateColumn must be a positive integer.")
      }
    }
  }
  
  # Notes
  if (verbose) cat('Note: Expect total RAM usage to be about ~1/2 of total csv folder size\n')
  
  # list files
  ext = gsub('.', '', ext, fixed = T)
  fNames <- list.files(path       = HRUfolder,
                       pattern    = '\\.csv',
                       full.names = T)
  if (sortNames) fNames <- sort(fNames)
  
  # Sample data for dimension info
  if (startDate == 'auto'){
    samplefData <- data.table::fread(input  = fNames[1],
                                     header = header,
                                     select = dateColumn,
                                     showProgress = T)
    dateVector <- as.Date(as.matrix(samplefData[,1])[,1])
    
  }else{
    samplefData <- data.table::fread(input  = fNames[1],
                                     header = header,
                                     select = 1,
                                     showProgress = T)
    dateVector <- as.Date(as.matrix(samplefData[,1])[,1])
  }
 
  
  
  # Create baseline/conversions list (after loading in sample dataset)
  fillFun <- function(HUC, convData = METsteps::HRU_to_HUC10_percData, tLength = length(dateVector)){
    y <- convData[which(convData$HUC10 == as.numeric(HUC)), c('HRUid', 'PERCENTAGE')]
    y$PERCENTAGE <- y$PERCENTAGE/100
    return(
      list(baseline    = rep(0, tLength),
           conversions = y)
    )
  }
  
  
  weightFun <- function(lItem, df){
    require(dplyr)
    baseline    <- lItem$baseline
    conversions <- lItem$conversions
    COI         <- conversions[conversions$HRUid %in% as.numeric(colnames(df)), ]
    if (nrow(COI) == 0){
      return(
        list(baseline    = baseline,
             conversions = conversions)
      )
    }else{
      IOI <- df[, match(COI$HRUid, as.numeric(colnames(df)))]
      IOI <- t(t(IOI) * COI$PERCENTAGE)
      return(
        list(baseline    = (baseline + rowSums(IOI)),
             conversions = conversions)
      )
    }
  }
  
  # Define progress bar
  pb <- progress::progress_bar$new(
    format = "  processing [:bar] :percent in :elapsed",
    total = length(fNames), clear = FALSE, width= 60)
  
  for (i in 1:length(fNames)){
    if (verbose) {
      cat("\014") 
      cat(paste0("Processing dataset ", i, '/', length(fNames), '\n'))
    }
    # Create baseline list
    if (i == 1){
      # Load data
      fData <- data.table::fread(input        = fNames[i],
                                 header       = T,
                                 showProgress = F)
      # Isolate Date column
      if (startDate == 'auto'){
        fData <- as.matrix(fData[,-dateColumn])
      }else{
        fData <- as.matrix(fData)
      }
          # dateVector <- as.Date(as.matrix(fData[,1])[,1])
          # # Remove Date Column from original file
      
      # Create column names
      colnames(fData) <- METsteps::HRUids[[i]]
      # Create baseline list
      baselineList <- lapply(X        = sort(METsteps::HUC_IDs$HUC10.info),
                             FUN      = fillFun,
                             convData = METsteps::HRU_to_HUC10_percData,
                             tLength  = length(dateVector))
      names(baselineList) <- sort(METsteps::HUC_IDs$HUC10.info)

      # Add fData to baselineList
      baselineList <- lapply(X   = baselineList,
                             FUN = weightFun,
                             df  = fData)
      rm(fData); gc()
    }else{
      # Load data
      fData <- data.table::fread(input        = fNames[i],
                                 header       = T,
                                 showProgress = F)
      # Isolate Date column
      # Isolate Date column
      if (startDate == 'auto'){
        fData <- as.matrix(fData[,-dateColumn])
      }else{
        fData <- as.matrix(fData)
      }
      
          # dateVector <- as.Date(as.matrix(fData[,1])[,1])
          # # Remove Date Column from original file
          # fData <- as.matrix(fData[,-1])
      # Create column names
      colnames(fData) <- METsteps::HRUids[[i]]
      # Add fData to baselineList
      baselineList <- lapply(X   = baselineList,
                             FUN = weightFun,
                             df  = fData)
      rm(fData); gc()
    }
  }
  
  # Convert to dataframe
  xx <- lapply(baselineList, function(x){return(x$baseline)})
  df <- (matrix(data  = unlist(x         = xx,
                               use.names = FALSE),
                ncol  = length(xx[[1]]),
                byrow = T))
  rownames(df) <- names(xx)
  if (startDate == 'auto'){
    startDate.pr <- as.Date(dateVector[1])
  }else{
    startDate.pr <- as.Date(startDate)
  }
  datesAll <- seq.Date(from       = startDate.pr,
                       by         = timeStep,
                       length.out = length(dateVector))
  colnames(df) <- lubridate::decimal_date(datesAll)
  
  # Compile info
  endDate <- datesAll[length(datesAll)]
  info <- c(dataName, ext, dataCategory, timeStep, as.character(startDate.pr), as.character(endDate))
  names(info) <- c('dataName', 'dataExtension', 'dataCategory', 'timeStep', 'startDate', 'endDate')
  
  return(list(HUC10matrix = df,
              info        = info))
}





