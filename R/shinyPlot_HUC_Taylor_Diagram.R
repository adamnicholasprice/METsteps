#' Taylor Diagram
#'
#' Generate Taylor diagram from 'plotrix' package.
#' @param subToHUC. Zoo object to plot.
#' @param cbPalette. Color values.
#' @export
#' @return Numeric vector.
#' @examples
#' shinyPlot_HUC_Taylor_Diagram()

shinyPlot_HUC_Taylor_Diagram <- function(default. = FALSE,
                                         subToHUC. = subToHUC,
                                         cbPalette. = cbPalette,
                                         ...){
  if (default.){
    frame()
  }else{
    if (ncol(subToHUC.) > 1){
      plotrix::taylor.diagram(ref = subToHUC.[,1],
                              model = subToHUC.[,2],
                              col = cbPalette.[2],
                              pos.cor=FALSE
                              pcex = 2,
                              mar = c(5,7,5,7))
    }else{
      frame()
      text(0.5,0.5,"Two or more datasets required for Taylor diagrams", col = 'red')
    }
    if (ncol(subToHUC) > 2){
      for (i in 3:ncol(subToHUC.)){
        plotrix::taylor.diagram(ref = subToHUC.[,1],
                                model = subToHUC.[,i],
                                add = TRUE,
                                pos.cor=FALSE
                                col = cbPalette.[i],
                                pcex = 2)
      }
    }
  }
}
