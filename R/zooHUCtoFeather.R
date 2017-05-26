#' Convert to feather
#'
#' This function converts the monthly mean HUC values to 'feather' format to enable faster importing than .csv 
#' or .txt formats.  Necessary for rapid RShiny app processing.  Requires 'feather' package.
#' @param zoo.obj Either output from downscale_HUC10() function (list object of zoo-format dataframes) or
#' vector of paths to locations of csv files.
#' @param Shiny.path Path to save location.  Defaults to current working directory.  Set to your RShiny app directory.
#' Will create a folder for feather files named exactly 'FeatherFiles' if not already available.
#' @export
#' @return List of tables of HUC 2-8 means in 'zoo' format.  Time-step x HUC-10.
#' @examples
#' zooHUCtoFeather()

zooHUCtoFeather<- function(zoo.obj,
                           Shiny.path = 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/'){
  require(feather)
  require(zoo)
  
  #Define save.path
  if (substrRight(Shiny.path) != '/') Shiny.path = paste0(Shiny.path, '/')
  save.path <- paste0(Shiny.path, 'FeatherFiles')

  # If savepath does not exist, prompt user
  if (dir.exists(save.path) == FALSE) {
    writeLines(paste0(
      paste0('Path ~',
             save.path,
             '~ does not exist.'
      )
    ))
    answer <- readline('Would you like the function to create this folder? (Y/N)   ')
    if (toupper(answer) == 'Y'){
      dir.create(paste0(Shiny.path, 'FeatherFiles'))
    }else if (toupper(answer) == 'N'){
      stop('Create Feather folder in path.')
    }else {
      stop('Unrecognized response. Create Feather folder in path.')
    }
  }
  
  # Generate filenames and append to save path
  for (i in 1:(length(zoo.obj)-1)){
    feather::write_feather(x    = as.data.frame(zoo.obj[[i]]),
                           path = file.path(save.path, createMETname(zoo.obj)[i]))
  }
}
