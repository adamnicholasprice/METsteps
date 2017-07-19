#' Convert to feather
#'
#' This function converts the monthly mean HUC values to 'feather' format to enable faster importing than .csv 
#' or .txt formats.  Necessary for rapid RShiny app processing.  Requires 'feather' package.
#' @param zoo.obj Either output from downscale_HUC10() function (list object of zoo-format dataframes) or
#' vector of paths to locations of csv files.
#' @param featherPath Path to containing folder for feather files to be used by Shiny application.
#' @export
#' @return List of tables of HUC 2-8 means in 'zoo' format.  Time-step x HUC-10.
#' @examples
#' zooHUCtoFeather()

zooHUCtoFeather<- function(zoo.obj,
                           featherPath = 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/FeatherFiles'){
  require(feather)
  require(zoo)
  
  #Define save.path
  if (substrRight(featherPath) != '/') featherPath = paste0(featherPath, '/')
  #save.path <- paste0(Shiny.path, 'FeatherFiles')

  # If savepath does not exist, prompt user
  if (!dir.exists(featherPath)) stop(paste('featherPath', featherPath, 'does not exist.'))
  
  # Generate filenames and append to save path
  for (i in 1:(length(zoo.obj)-1)){
    feather::write_feather(x    = as.data.frame(zoo.obj[[i]]),
                           path = file.path(featherPath, createMETname(zoo.obj)[i]))
  }
}
