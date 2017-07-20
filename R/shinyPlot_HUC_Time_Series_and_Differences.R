#' Primary Time Series and Difference Plot after HUC Click
#'
#' Description goes here.
#' @param x Numeric; The values to be plotted.
#' @param col Character; The plot color.
#' @export
#' @return Numeric vector.
#' @examples
#' shinyPlot_HUC_Time_Series_and_Difference()
shinyPlot_HUC_Time_Series_and_Difference   <- function(default. = FALSE,
                                                       feederList. = NULL,
                                                       x = subToHUC,
                                                       col = cbPalette,
                                                       ablCol = 'darkgrey',
                                                       colDif = 'firebrick',
                                                       HCU. = HCU,
                                                       dataCategory. = dataCategory,
                                                       multiplot.cex = 1.4,
                                                       multiplot.lab = 1.4,
                                                       cex.main = 1.8,
                                                       ...){
  # Extract sliderTime. from feederList
  if (!is.null(feederList.)){
    sliderTime. <- feederList.$slider_time
  }else{
    sliderTime. <- NULL
  }
  
  # Create plot
  if (default.){
    if (is.null(sliderTime.)){
      drange <- NULL
    }else{
      drange <- lubridate::decimal_date(as.Date(sliderTime.))
    }
    layout(mat     = matrix(data  = c(1, 2, 3),
                            nrow  = 3,
                            ncol  = 1,
                            byrow = T),
           widths  = 1,
           heights = c(2.5, 1, 1))
    #-- ET direct data plot
    par(mar = c(0, 6.2, 4.1, 2.1))
    plot(x        = 1,
         type     = 'n',
         main     = '',
         xaxs     = "i",
         ylab     = '(mm)',
         xlim     = drange,
         xlab     = '',
         xaxt     = 'n',
         cex.axis = multiplot.cex,
         cex.lab  = multiplot.lab)
    #-- Difference plot
    par(mar = c(0, 6.2, 0, 2.1))
    plot(x        = 1,
         type     = 'n',
         main     = '',
         xaxs     = "i",
         ylab     = 'Diff (mm)',
         xlab     = '',
         xlim     = drange,
         xaxt     = 'n',
         yaxt     = 'n',
         cex.lab  = multiplot.lab)
    axis(side = 4,
         cex.axis = multiplot.cex)
    #--  sd plot
    par(mar = c(2, 6.2, 0, 2.1))
    plot(x        = 1,
         type     = 'n',
         main     = '',
         xaxs     = "i",
         ylab     = 'Standard\nDev',
         xlab     = 'Time',
         xlim     = drange,
         cex.axis = multiplot.cex,
         cex.lab  = multiplot.lab)
    par(mar = c(5.1, 4.1, 4.1, 2.1))
  }else{
    if (is.null(sliderTime.)){
      drange <- NULL
    }else{
      drange <- lubridate::decimal_date(as.Date(sliderTime.))
    }
    layout(mat     = matrix(data  = c(1,2,3),
                            nrow  = 3,
                            ncol  = 1,
                            byrow = T),
           widths  = 1,
           heights = c(2.5, 1, 1))
    
    #------ ET direct data plot
    # start plot with first dataset
    par(mar = c(0, 6.2, 4.1, 2.1))
    plot(x        = x[,1],
         col      = col[1],
         main     = paste('HUC', HCU.),
         xaxs     = "i",
         ylab     = paste0(dataCategory.,'\n(mm)'),
         xlim     = drange,
         ylim     = range(x, na.rm = T),
         xlab     = '',
         xaxt     = 'n',
         cex.axis = multiplot.cex,
         cex.lab  = multiplot.lab,
         cex.main = cex.main,
         lwd      = 2,
         yaxs     = 'i')
    # add remaining datasets with line() function and calculate ensemble mean
    if (ncol(x) > 1){
      for (i in 2:ncol(x)){
        lines(x    = x[,i],
              col  = cbPalette[i],
              xaxs = "i",
              lwd  = 2)
      }
      # Calculate ensemble means and add to plot
      ensembleMeans <- zoo::zoo(rowMeans(x), stats::time(x))
      points(x = ensembleMeans,
             col = 'black',
             xaxs = 'i',
             lwd = 2,
             pch = 16)
      lines(x = ensembleMeans,
            col = 'black',
            xaxs = 'i',
            lwd = 1,
            lty = 1)
    }
    abline(v   = unique(as.integer(zoo::index(x))),
           col = ablCol,
           lty = 2)
    if (ncol(x) > 1){
      legend(x      = 'bottomright',
             legend = c(dnames, 'Ens. Mean'),
             pch    = 15,
             col    = c(cbPalette[1:ncol(x)], 'black'),
             pt.cex = 2,
             cex    = 1.5)
    }else{
      legend(x      = 'bottomright',
             legend = dnames,
             pch    = 15,
             col    = cbPalette[1:ncol(x)],
             pt.cex = 2,
             cex    = 1.5)
    }
    
    
    #------ Difference Plot
    par(mar = c(0, 6.2, 0, 2.1))
    if (ncol(x) == 1){
      plot(x        = 1,
           type     = 'n',
           main     = '',
           xaxs     = 'i',
           ylab     = 'Diff (mm)',
           xlab     = '',
           xaxt     = 'n',
           yaxt     = 'n',
           xlim     = drange,
           cex.axis = multiplot.cex,
           cex.lab  = multiplot.lab)
      axis(side = 4,
           cex.axis = multiplot.cex)
      abline(v   = unique(as.integer(zoo::index(x))),
             col = ablCol,
             lty = 2)
      abline(h   = 0,
             col = ablCol,
             lty = 1)}
    if (ncol(x) == 2){
      plot(x        = x[,2] - x[,1],
           col      = colDif,
           main     = '',
           xaxs     = "i",
           ylab     = 'Diff (mm)',
           xlab     = '',
           xaxt     = 'n',
           xlim     = drange,
           cex.axis = multiplot.cex,
           cex.lab  = multiplot.lab,
           lwd      = 2)
      abline(v   = unique(as.integer(zoo::index(x))),
             col = ablCol,
             lty = 2)
      abline(h   = 0,
             col = ablCol,
             lty = 1)
    }
    if (ncol(x) > 2){
      combinations  <- t(combn(x = ncol(x),
                               m = 2))
      combFUNdif <- function(combrow, x){
        x[, combrow[2]] - x[, combrow[1]]
      }
      difmat        <- zoo::as.zoo(apply(X      = combinations,
                                         MARGIN = 1,
                                         FUN    = combFUNdif,
                                         x      = x))
      zoo::index(difmat) <- zoo::index(x)
      
      plot(x        = difmat[,1],
           col      = colDif,
           main     = '',
           xaxs     = "i",
           ylab     = 'Diff (mm)',
           xlab     = '',
           xlim     = drange,
           xaxt     = 'n',
           cex.axis = multiplot.cex,
           cex.lab  = multiplot.lab,
           lwd      = 2,
           ylim     = c(min(difmat, na.rm = T),
                        max(difmat, na.rm = T)))
      for (i in 2:ncol(difmat)){
        lines(x    = difmat[,i],
              xaxs = 'i',
              col  = colDif,
              lwd  = 2,
              lty  = i)
      }
      abline(v   = unique(as.integer(index(x))),
             col = ablCol,
             lty = 2)
      abline(h   = 0,
             col = ablCol,
             lty = 1)
    }
    
    #------ sd Plot
    if (ncol(x) > 1){
      par(mar = c(2, 6.2, 0, 2.1))
      #Calculate standard deviation
      sdZoo <- apply(X = x,
                     MARGIN = 1,
                     FUN = sd,
                     na.rm = T)
      sdZoo <- zoo::zoo(sdZoo)
      zoo::index(sdZoo) <- zoo::index(x)
      plot(x        = sdZoo,
           type     = 'l',
           main     = '',
           xaxs     = "i",
           ylab     = 'Standard\nDev',
           xlab     = 'Time',
           xlim     = drange,
           cex.axis = multiplot.cex,
           cex.lab  = multiplot.lab)
      abline(v   = unique(as.integer(index(x))),
             col = ablCol,
             lty = 2)
      abline(h   = 0,
             col = ablCol,
             lty = 1)
    }else{
      plot(x        = x,
           type     = 'n',
           main     = '',
           xaxs     = "i",
           ylab     = 'Standard\nDev',
           xlab     = 'Time',
           xlim     = drange,
           cex.axis = multiplot.cex,
           cex.lab  = multiplot.lab)
      abline(v   = unique(as.integer(index(x))),
             col = ablCol,
             lty = 2)
      abline(h   = 0,
             col = ablCol,
             lty = 1)
    }
    par(mar = c(5.1, 4.1, 4.1, 2.1))
  }
}
