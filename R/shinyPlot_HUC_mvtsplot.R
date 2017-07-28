#' Multivariate time series plot. Initiated after click
#'
#' Description goes here.
#' @param x Numeric; The values to be plotted..
#' @export
#' @return .
#' @examples
#' shinyPlot_HUC_mvts(x)

shinyPlot_HUC_mvtsplot<- function(x=subToHUC,
                                  ...){
  require(mvtsplot)
  require(RColorBrewer)
  mvtsplot::mvtsplot(x, 
                     palette = 'Set3', 
                     xtime = index(subToHUC), 
                     main ="MVTS Plot",
                     sort = 'max',
                     rowstat = 'sd',
                     group = c(1:ncol(subToHUC)),
                     gcol ='black')
}