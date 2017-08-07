#' Envelope Polygons
#'
#' Creates envelope polygons between two time series datasets that have the same x-values.  Fill in missing
#' gaps with NAs.
#' @param xall The x-values used by both y vectors.
#' @param y1 Y value for dataset 1.
#' @param y2 Y value for dataset2.
#' @param col fill color.
#' @param border border color.
#' @export
#' @return Numeric vector.
#' @examples
#' envelope()

envelope <- function(xall,
                     y1,
                     y2,
                     col = 'darkgrey',
                     border = 'darkgrey'){
  
  if (!identical(is.na(y1), is.na(y2))) stop('NA values in y arguments do not match.')
  
  # split by NA
  foo <- function(y1, y2, xall){
    idx <- 1 + cumsum(is.na(y1))
    not.na <- !is.na(y1)
    return(
      list(y1 = split(y1[not.na], idx[not.na]),
           y2 = split(y2[not.na], idx[not.na]),
           xall = split(xall[not.na], idx[not.na])))
    }
  
  lss <- foo(y1, y2, xall)
  
  
  for (i in 1:length(lss$y1)){
    age <- as.Date(lss$xall[[i]])
    y1  <- lss$y1[[i]]
    y2  <- lss$y2[[i]]
    polygon(c(age, rev(age)), c(y1, rev(y2)),
            col = col, border = border)
  }
}
