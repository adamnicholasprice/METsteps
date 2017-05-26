#' Start tic/toc function
#'
#' Starting function for tic/toc timing sequence.
#' @export
#' @return Produces starting time argument for toc function.

tic <- function(){
  .tic.s <<- proc.time()
  invisible(.tic.s)
}


#' End tic/toc function
#'
#' Ending function for tic/toc timing sequence.  Requires the tic function to have been run first.
#' Can be run in series based on original tic function product.
#' @param LS Character; Optional leading string to be included to annotate different timesteps.  Default is NULL.
#' @param end Logical; If TRUE, end timing sequence, deletes tic() initialization.  Defaults to FALSE.
#' @param return.value Logical; If TRUE, function returns elapsed time value in numeric format for use.  Defaults to FALSE.
#' Units in seconds.
#' @export
#' @return Calculate time change since initializing tic function.

toc <- function(LS = NULL, end = FALSE, return.value = FALSE){
  if (exists('.tic.s')){
    .toc.e = proc.time()
    .elapsed = as.numeric((.toc.e - .tic.s)[3])
    if (end == TRUE){rm(.tic.s, envir = globalenv())}
    if (.elapsed < 60){
      writeLines(paste(LS, 'Elapsed Time:', round(.elapsed, 2), 'seconds'))
    }else if (.elapsed >= 60 && .elapsed < 3600){
      writeLines(paste(LS, 'Elapsed Time:', round(.elapsed/60, 2), 'minutes'))
    } else if(.elapsed >= 3600){
      writeLines(paste(LS, 'Elapsed Time:', round((.elapsed/60)/60, 2), 'hours'))
    }
    if (return.value == TRUE){
      return(.elapsed)
    }
  }else{stop('Must first run tic() function')}
}