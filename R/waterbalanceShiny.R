#' Shiny Application for Calculting CONUS Water Budget
#'
#' Launches METsteps Shiny application for interactively calculating a national water balance using feather tables created
#' within the METsteps workflow.
#' 
#' @param path.feather Character; Path to folder containing feather files produced through zooHUCtoFeather
#' @export
#' @return Shiny application
#' @examples
#' waterbalanceShiny()

waterbalanceShiny <- function(path.feather){
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
  
  # Initiate application
  shiny::runApp(appDir = system.file('waterbalanceShiny',
                                     package = 'METsteps'))
}

#METsteps::waterbalanceShiny(path.feather = 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/FeatherFiles')