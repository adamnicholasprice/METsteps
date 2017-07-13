#' Create File Name
#'
#' Function to create feather filenames to be called in the METsteps Shiny application.
#' @param x Either list object created by METsteps::downscaleHUC10() function or results from METsteps::extractMetadata() function.
#' @param fromSequence Logical; If TRUE, create name from object output from METsteps::downscaleHUC10().  If FALSE, create name
#' from results of METsteps::extractMetadata().
#' @export
#' @return List of filenames for zoo objects.
#' @examples
#' createMETname(x)

createMETname <- function(x, fromSequence = TRUE){
  if (!('info' %in% names(x))) stop('No information list item called "info" associated with this object.')
  info <- as.data.frame(t(x$info),
                        stringsAsFactors = F)
  info <- info[ , c('dataName', 'dataCategory', 'timeStep', 'startDate', 'endDate')]
  hucNos <- as.numeric(gsub("\\D", "", (names(x)[grepl('HUC', names(x))])))
  return(
    paste0(info$dataCategory, '_',
           info$dataName, '_',
           'HUC', hucNos, '_',
           info$timeStep, '_',
           info$startDate, '_',
           info$endDate, '.feather')
  )
}
