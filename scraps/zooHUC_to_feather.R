#' Convert to feather
#'
#' This function converts the monthly mean HUC values to 'feather' format to enable faster importing than .csv 
#' or .txt formats.  Necessary for rapid RShiny app processing.  Requires 'feather' package.
#' @param zoo.obj Either output from downscale_HUC10() function (list object of zoo-format dataframes) or
#' vector of paths to locations of csv files.
#' @param Shiny.path Path to save location.  Defaults to current working directory.  Set to your RShiny app directory.
#' Will create a folder for feather files named exactly 'FeatherFiles' if not already available.
#' @param HUC.numbers Numbers for HUC units.  Defaults to c(2,4,6,8,10).
#' @param Data.name Character string of name of dataset.  Ex: 'SSEBop', 'MOD16', etc.
#' @param step Character string of the time step of the matrices. Options are 'day', 'week', 'month', 'quarter', and 'year'
#' @param Start.d Character string in format 'YYYY-MM-DD'.
#' @export
#' @return List of tables of HUC 2-8 means in 'zoo' format.  Time-step x HUC-10.
#' @examples
#' agg_Raster_to_HUC10

zooHUC_to_feather<- function(zoo.obj,
                             Shiny.path = 'C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/',
                             HUC.numbers = c(2,4,6,8,10),
                             Data.name = NULL,
                             step = c('day', 'week', 'month', 'quarter', 'year'),
                             Start.d = NULL
                             ){
  require(feather); require(zoo);
  
  #Add '/' onto path if necessary
  if ((substr(Shiny.path, nchar(Shiny.path), nchar(Shiny.path))) != '/'){Shiny.path = paste0(Shiny.path, '/')}
  #Define save.path
  save.path = paste0(Shiny.path, 'FeatherFiles', '/')
  
  #Define save.path.  If FeatherFiles folder not present, create.
  if (dir.exists(save.path) == FALSE){dir.create(paste0(Shiny.path, 'FeatherFiles'))}
  

  if (typeof(zoo.obj) == 'list'){
    #Check to make sure each item is a zoo object
    if (all(unlist(lapply(zoo.obj[names(zoo.obj)!='info'], is.zoo))) == FALSE){stop('At least one list item in zoo.obj is not a zoo object')}
    #check to make sure there are the correct number of list items
    if (length(zoo.obj[names(zoo.obj)!='info']) != length(HUC.numbers)){stop("Length of zoo.obj list != length of HUC.numbers")}
    #Check to make sure information file is associated
    if (is.null(zoo.obj$info)){
      if (is.null(Data.name) || is.null(step) || is.null(start.d)){
        stop('No associated information (zoo.obj$info not present) and not supplied manually')}
    }
    #zoo.obj$info is not available, use manual inputs
    if (is.null(zoo.obj$info)){
      #Test to make sure correct format
      if (is.character(Data.name) == FALSE){stop('Data.name is not string')}
      if ((step %in% c('day', 'week', 'month', 'quarter', 'year')) == FALSE){stop('Incorrect step supplied')}
    }else{
      Data.name = zoo.obj$info['Data.name']
      Start.d = zoo.obj$info['Start.d']
      step = zoo.obj$info['tstep']
    }
    for (i in 1:(length(zoo.obj)-1)){
      #write file
      write_feather(as.data.frame(zoo.obj[[i]]),
                    paste0(save.path, Data.name, '__', 'HUC', HUC.numbers[i],
                           '_', step, '__', gsub('-', '_', Start.d), '.feather')
                    )
    }
  }else if (typeof(zoo.obj) == 'character'){
    #check to make sure there are the correct number of file paths (same as HUC.numbers)
    if (length(zoo.obj) != length(HUC.numbers)){stop("Length of zoo.obj paths != length of HUC.numbers")}
    #Check to make sure info is supplied.  Required for saving file.
    if (is.null(Data.name) || is.null(Start.d) || is.null(step)){stop('No associated information (zoo.obj$info not present) and not supplied manually')}
    #Check to make sure info in correct format
    if (is.character(Data.name) == FALSE){stop('Data.name is not string')}
    if ((step %in% c('day', 'week', 'month', 'quarter', 'year')) == FALSE){stop('Incorrect step supplied')}
    for (i in 1:length(zoo.obj)){
      data.zoo = read.csv(zoo.obj[i])  #import file
      #write file
      write_feather(data.zoo,
                    paste0(save.path, Data.name, '__', 'HUC', HUC.numbers[i],
                           '_', step, '__', gsub('-', '_', Start.d), '.feather')
                    )
      
    }
  }
}
