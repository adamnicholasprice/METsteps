#' Title Goes Here
#'
#' Description goes here.  Notes:  the filename should be exactly the same as the function name, with ".R" added
#' on.  For example, this function is called "shinyPlot_HUC_Example_Plot".  In this case, your filename should be
#' "shinyPlot_HUC_Example_Plot.R".  Name formatting: all plot functions should start with "shinyPlot_".  If the plot
#' is specific to a single HUC, the name should then follow with "HUC_".  If it is for all HUC regions, change "HUC_"
#' to "all_".  Then, add on the formal name of the plot that will be visible to the user in the Shiny interface.
#' In this case, that is "Example_Plot".  Any underscores in the formal name section will be converted to spaces
#' in the UI.
#' @param x Numeric; The values to be plotted.
#' @param col Character; The plot color.
#' @export
#' @return Numeric vector.
#' @examples
#' shinyPlot_HUC_Example_Plot(x = 1:10, col = 'red')
shinyPlot_HUC_Example_Plot   <- function(x, col){
  plot(x, col = col, type = 'l')
}
