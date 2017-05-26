#' Create File Name
#'
#' Function to create feather filenames to be called in the METsteps Shiny application.
#' @param zoo.obj List object created by METsteps::downscaleHUC10() function.
#' @export
#' @return List of filenames for zoo objects.
#' @examples
#' createMETname(zoo.obj)

createMETname <- function(zoo.obj){
  if (!('info' %in% names(zoo.obj))) stop('No information list associated with this object.')
  info <- as.data.frame(t(zoo.obj$info),
                        stringsAsFactors = F)
  info <- info[ , c('dataName', 'dataCategory', 'timeStep', 'startDate', 'endDate')]
  hucNos <- as.numeric(gsub("\\D", "", (names(zoo.obj)[grepl('HUC', names(zoo.obj))])))
  return(
    paste0(info$dataCategory, '_',
           info$dataName, '_',
           'HUC', hucNos, '_',
           info$timeStep, '_',
           info$startDate, '_',
           info$endDate, '.feather')
  )
}
