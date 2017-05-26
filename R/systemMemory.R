#' System Memory
#'
#'Retrieve system memory information using the R base 'system' function to invoke an OS call.  Returns current amount of
#'memory in use, available free memory, and total visible memory.  Windows Only!
#'
#' @param dataPath Character; Units of memory. Options are c('KB', 'MB', 'GB').  Defaults to 'GB'.
#' @param returnAsList Logical; If TRUE, return results as list. If FALSE, return results as named vector. Defaults
#' to TRUE.
#' @param verbose Logical; If TRUE, writes units to console.  Defaults to FALSE.
#' @export
#' @return List or vector of memory use, free memory, and total memory.
#' @examples
#' systemMemory()

systemMemory <- function(units = 'GB', returnAsList = TRUE, verbose = FALSE){
  # Error handling
  stopifnot(is.character(units), 
            is.logical(returnAsList))
  units = toupper(units)
  stopifnot(units %in% c('KB', 'MB', 'GB'))
  
  # Perform OS call
  ss = system('wmic OS get TotalVisibleMemorySize,FreePhysicalMemory /Value', intern = TRUE)

  # Total Visible Memory Size
  tt = ss[grepl('TotalVisibleMemorySize', ss)]
  tt = as.numeric(substr(tt,
                         start = (regexpr('=', tt, fixed = T)) + 1,
                         stop = (regexpr('\r', tt, fixed = T)) - 1))
  
  # Free Physical Memory
  ff = ss[grepl('FreePhysicalMemory', ss)]
  ff = as.numeric(substr(ff,
                         start = (regexpr('=', ff, fixed = T)) + 1,
                         stop = (regexpr('\r', ff, fixed = T)) - 1))
  
  # Convert units
  if (units == 'KB'){
    tt = tt
    ff = ff
  }
  if (units == 'MB'){
    tt = tt/1024
    ff = ff/1024
  } else if (units == 'GB'){
    tt = tt/1048576
    ff = ff/1048576
  }
  
  # In use
  ii = tt-ff
  
  # Combine, add names, return
  oo = list(MemoryInUse = ii, FreeMemory = ff, TotalMemory = tt)
  if (returnAsList == FALSE){
    oo = unlist(oo)
  }
  if (verbose == T) {writeLines(paste0('\nUnits: ', units))}
  return(oo)
}
