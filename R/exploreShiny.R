#' Shiny Application for Exploring Processed Datasets
#'
#' Launches METsteps Shiny application for interactively exploring datasets, producing statistics, and longer-term, 
#' calculating a national water budget.
#' 
#' @param path.feather Character; Path to folder containing feather files produced through zooHUCtoFeather
#' @param path.obsMeta Character; Path to metadata file.  Defaults to NULL.
#' @param path.obs Character; Path to folder containing point data observation files as listed in metadata file.
#' @export
#' @return Shiny application
#' @examples
#' exploreShiny()

exploreShiny <- function(path.feather,
                         path.obsMeta = NULL,
                         path.obs = NULL){
  require(dplyr)
  require(feather)
  require(hydroGOF)
  require(leaflet)
  require(lubridate)
  require(METsteps)
  require(parallel)
  require(raster)
  require(scales)
  require(shiny)
  require(shinydashboard)
  require(stringr)
  require(viridis)
  require(zoo)
  
  if (length(path.feather) > 1) stop('Please submit only a single folder directory path.')
  
  path.feather       <<- path.feather
  
  fnames.feather     <<- list.files(path      = path.feather,
                                    pattern   = '\\.feather$',
                                    full.name = FALSE)
  
  fileInfo           <<- extractMetadata(METname = fnames.feather,
                                                   df      = T)
  fileInfo           <<- data.frame(fnames           = fnames.feather,
                                    fileInfo,
                                    stringsAsFactors = FALSE)
  fileInfo$startDate <- as.Date(fileInfo$startDate)
  fileInfo           <<- fileInfo
  
  # Map constraints
  bounds             <<- c(-124.8, 23.4, -67, 45 )
  states             <<- METsteps::stateLines
  
  # Set Seasons
  All                <<- (1:12)
  Fall               <<- c(9, 10, 11)
  Winter             <<- c(12, 1, 2)
  Spring             <<- c(3, 4, 5)
  Summer             <<- c(6, 7, 8)
  
  # Get obs metadata
  if (!is.null(path.obsMeta)){
    ptmeta <<- read.csv(file = path.obsMeta,
                        stringsAsFactors = F)
    if (colnames(ptmeta)[1] == 'X') ptmeta <<- ptmeta[,-1]
    timeStep <<- NULL
    ptmeta.map <<- NULL
    path.obs <<- path.obs
  }else{
    ptmeta <<- NULL
    ptmeta.map <<- NULL
  }
  
  # Check to make sure observational data folder exists
  if (!is.null(path.obs)){
    if (!dir.exists(path.obs)) stop("path.obs does not exist.")
  }else{
    path.obs <<- NULL
  }
  
  # Initiate application
  shiny::runApp(appDir = system.file('exploreShiny',
                                     package = 'METsteps'))
}

#METsteps::exploreShiny(path.feather = 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/FeatherFiles', path.obsMeta = 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/PointData/ET/New/metadata.csv', path.obs     = 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/PointData/ET/New/')

METsteps::exploreShiny(path.feather = 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/FeatherFiles',
                       path.obsMeta = 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/PointData/ET/New/metadata2.csv',
                       path.obs     = 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/PointData/ET/New2/')

# x = read.csv('C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/PointData/ET/New/metadata.csv')
# NL <- data.frame(OurID = "Recharge_1",
#                  Lat = 45.5872,
#                  Lon = -122.702,
#                  Source = NA,
#                  SourceID = "01N/01E-09BCB",
#                  TimeStep = 'year',
#                  startDate = "2000-01-01",
#                  endDate = "2009-01-01",
#                  MeasurementType = "GWAD",
#                  Units = "mm",
#                  dataCategory = "Recharge",
#                  misc = NA)
# x2 <- rbind(x, NL)
# write.csv(x2, 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/PointData/ET/New/metadata2_Deleteme.csv')
# x3 <- read.csv( 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/PointData/ET/New/metadata2_Deleteme.csv',
#                 stringsAsFactors = F)
# x3$TimeStep[x3$TimeStep == 'Daily'] <- 'day'
# x3$TimeStep[x3$TimeStep == 'Monthly'] <- 'month'
# 
# write.csv(x3, 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/PointData/ET/New/metadata2_Deleteme.csv',
#           row.names = FALSE)


# 
# 
# setwd('C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/FeatherFiles')
# fnames = c(list.files()[grepl('NCEP-DOE', list.files())],
#            list.files()[grepl('NCEP-NCAR', list.files())],
#            list.files()[grepl('ERA', list.files())]
#            )
# 
# for(i in 1:length(fnames)){
#   print(paste0(i, '/', length(fnames)))
#   require(feather)
#   require(Hmisc)
#   x <- read_feather(fnames[i])
#   m <- METsteps::extractMetadata(fnames[i])
#   dd <- seq.Date(from = as.Date(m$startDate),
#                  length.out   = nrow(x),
#                  by = m$timeStep)
#   if (length(dd) == nrow(x)){
#     convValue <- Hmisc::monthDays(dd)
#                # s   min   h    d     mnth
#     x2 = x * convValue
# 
#     if (identical(as.numeric(unlist(convValue * x[,1])), x2[,1])){
#       write_feather(x = x2,
#                     path = fnames[i])
#     }
#   }else{
#     stop('Error with date length!')
#   }
# }


