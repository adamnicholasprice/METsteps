#' Time Series Intersect
#'
#' Reduce time series to overlapping time periods with start dates, end dates, and time steps (for continuous datasets).
#' @param startDates Character; Start dates. Must be same length as endDates.
#' @param endDates Character; End dates. Must be same length as startDates.
#' @param by Character; increment of sequence as found in seq.Date() function.  Length of one.
#' @export
#' @return Numeric vector.
#' @examples
#' timeOverlap()

timeOverlap <- function(startDates, endDates, by){
  if (length(startDates) != length(endDates)) stop('Length of startDates is not equal to the length of endDates')
  if (length(by) != 1) stop("'by' can not be more than length 1.")
  dsq <- vector(mode = 'list',
                length = length(startDates))
  for (i in 1:length(startDates)){
    dsq[[i]] <- seq.Date(from = as.Date(startDates[i]),
                         to = as.Date(endDates[i]),
                         by = by)
  }
  return(as.Date(x = Reduce(intersect, dsq),
                 origin = '1970-01-01'))
}