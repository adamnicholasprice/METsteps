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
                                                       ptSP. = ptSP.trim,
                                                       path.obs. = path.obs,
                                                       col = cbPalette,
                                                       ablCol = 'darkgrey',
                                                       colDif = 'firebrick',
                                                       HCU. = HCU,
                                                       dataCategory. = dataCategory,
                                                       multiplot.cex = 1.4,
                                                       multiplot.lab = 1.4,
                                                       cex.main = 1.2,
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
    # layout(mat     = matrix(data  = c(1, 2, 3),
    #                         nrow  = 3,
    #                         ncol  = 1,
    #                         byrow = T),
    #        widths  = 1,
    #        heights = c(2.5, 1, 1))
    layout(mat     = matrix(data  = c(1,2,3,4),
                            nrow  = 4,
                            ncol  = 1,
                            byrow = T),
           widths  = 1,
           heights = c(2, 1, 0.5, 0.75))
    par(mar = c(0,0,0,0), oma = c(3,6,3,3))
    #-- ET direct data plot
    #par(mar = c(0, 6.2, 4.1, 2.1))
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
    mtext(text = paste('HUC'),
          line = 1,
          font = 2,
          cex = cex.main)
    mtext(text = paste0('    ','\n(mm)'),
          side = 2,
          line = 3)
    #-- Difference plot
    #par(mar = c(0, 6.2, 0, 2.1))
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
    mtext(text = 'Diff\n(mm)',
          side = 2,
          line = 3)
    #--  sd plot
    #par(mar = c(2, 6.2, 0, 2.1))
    plot(x        = 1,
         type     = 'n',
         main     = '',
         xaxs     = "i",
         ylab     = 'Standard\nDev',
         xlab     = 'Time',
         xlim     = drange,
         cex.axis = multiplot.cex,
         cex.lab  = multiplot.lab)
    mtext(text = 'Standard\nDev',
          side = 2,
          line = 3)
    #-- legend
    frame()
    #par(mar = c(5.1, 4.1, 4.1, 2.1))
  }else{
    if (is.null(sliderTime.)){
      drange <- NULL
    }else{
      drange <- lubridate::decimal_date(as.Date(sliderTime.))
    }
    layout(mat     = matrix(data  = c(1,2,3,4),
                            nrow  = 4,
                            ncol  = 1,
                            byrow = T),
           widths  = 1,
           heights = c(2, 1, 0.5, 0.75))
    par(mar = c(0,0,0,0), oma = c(3,6,3,3))
    
    #------ ET direct data plot
    # start plot with first dataset
      #par(mar = c(0, 6.2, 4.1, 2.1))
    plot(x        = x[,1],
         col      = col[1],
         main     = '',
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
    mtext(text = paste('HUC', HCU.),
          line = 1,
          font = 2,
          cex = cex.main)
    mtext(text = paste0(dataCategory.,'\n(mm)'),
          side = 2,
          line = 3)
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
    
    ## Add in point data if requested and/or available
    if (feederList.$showObs){
      if (!is.null(ptSP.)){
        if (nrow(ptSP.) > 0){
          # function for importing via OurID
          imFun <- function(id, path, ext = '.csv'){
            y <- zoo::read.zoo(file = paste0(path, id, ext))
            #zoo::index(y) <- lubridate::decimal_date(zoo::index(y))
            return(y)
          }
          tic()
          allZoo <- lapply(X = ptSP.@data$OurID,
                           FUN = imFun,
                           path = path.obs.)
          toc()
          # Combine into single zoo object
          z <- do.call("merge.zoo", allZoo)
          indd <- zoo::index(z)
          z <- rowMeans(z, na.rm = T) 
          z <- zoo::zoo(z)
          zoo::index(z) <- indd
          #convert to mm/month
          z <- z*31
          #average for each month
          zz <- aggregate(x = z,
                          by = yearmon(decimal_date(zoo::index(z))),
                          FUN = mean,
                          na.rm = T)
          points(zz, col = 'red', pch = 16, cex = 2)
          lines(zz, col = 'red', lwd = 2, lty = 1)
        } 
      }
    }
    
    # # add legend
    # if (feederList.$showObs){
    #   if (ncol(x) > 1){
    #     legend(x      = 'bottomright',
    #            legend = c(dnames, 'Ens. Mean', 'Mean Obs'),
    #            pch    = 15,
    #            col    = c(cbPalette[1:ncol(x)], 'black', 'red'),
    #            pt.cex = 2,
    #            cex    = 1.5)
    #   }else{
    #     legend(x      = 'bottomright',
    #            legend = c(dnames, 'Mean Obs'),
    #            pch    = 15,
    #            col    = c(cbPalette[1:ncol(x)], 'red'),
    #            pt.cex = 2,
    #            cex    = 1.5)
    #   }
    # }else{
    #   if (ncol(x) > 1){
    #     legend(x      = 'bottomright',
    #            legend = c(dnames, 'Ens. Mean'),
    #            pch    = 15,
    #            col    = c(cbPalette[1:ncol(x)], 'black'),
    #            pt.cex = 2,
    #            cex    = 1.5)
    #   }else{
    #     legend(x      = 'bottomright',
    #            legend = dnames,
    #            pch    = 15,
    #            col    = cbPalette[1:ncol(x)],
    #            pt.cex = 2,
    #            cex    = 1.5)
    #   }
    # }

    
    #------ Difference Plot
      #par(mar = c(0, 6.2, 0, 2.1))
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
             lty = 1)
      mtext(text = 'Diff\n(mm)',
            side = 2,
            line = 3)
      }
    if (ncol(x) == 2){
      plot(x        = x[,2] - x[,1],
           col      = colDif,
           main     = '',
           xaxs     = "i",
           ylab     = 'Diff (mm)',
           xlab     = '',
           xaxt     = 'n',
           yaxt     = 'n',
           xlim     = drange,
           cex.axis = multiplot.cex,
           cex.lab  = multiplot.lab,
           lwd      = 2)
      axis(side = 4,
           cex.axis = multiplot.cex)
      abline(v   = unique(as.integer(zoo::index(x))),
             col = ablCol,
             lty = 2)
      abline(h   = 0,
             col = ablCol,
             lty = 1)
      mtext(text = 'Diff\n(mm)',
            side = 2,
            line = 3)
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
           yaxt     = 'n',
           cex.axis = multiplot.cex,
           cex.lab  = multiplot.lab,
           lwd      = 2,
           ylim     = c(min(difmat, na.rm = T),
                        max(difmat, na.rm = T)))
      axis(side = 4,
           cex.axis = multiplot.cex)
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
      mtext(text = 'Diff\n(mm)',
            side = 2,
            line = 3)
    }
    
    #------ sd Plot
    if (ncol(x) > 1){
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
      mtext(text = 'Standard\nDev',
            side = 2,
            line = 3)
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
      mtext(text = 'Standard\nDev',
            side = 2,
            line = 3)
    }
    
    #------ legend
    plot(1, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n', xlim = c(0,1), ylim = c(0,1))
    # add legend
    if (feederList.$showObs){
      if (ncol(x) > 1){
        legend(x = 'bottom',
               legend = c(dnames, 'Ens. Mean', 'Mean Obs'),
               pch    = 15,
               col    = c(cbPalette[1:ncol(x)], 'black', 'red'),
               pt.cex = 2,
               cex    = 1.2,
               horiz  = F,
               bty    = 'n',
               ncol   = floor(length(c(dnames, 'Ens. Mean', 'Mean Obs'))/2),
               x.intersp = 1,
               y.intersp = 1,
               inset = 0 
               )
      }else{
        legend(x      = 'bottom',
               legend = c(dnames, 'Mean Obs'),
               pch    = 15,
               col    = c(cbPalette[1:ncol(x)], 'red'),
               pt.cex = 2,
               cex    = 1.2,
               horiz  = T,
               bty    = 'n',
               ncol   = floor(length(c(dnames, 'Mean Obs'))/2),
               x.intersp = 1,
               y.intersp = 1,
               inset = 0 
               )
      }
    }else{
      if (ncol(x) > 1){
        legend(x      = 'bottom',
               legend = c(dnames, 'Ens. Mean'),
               pch    = 15,
               col    = c(cbPalette[1:ncol(x)], 'black'),
               pt.cex = 2,
               cex    = 1.2,
               horiz  = T,
               bty    = 'n',
               ncol   = floor(length(c(dnames, 'Ens. Mean'))/2),
               x.intersp = 1,
               y.intersp = 1,
               inset = 0 
               )
      }else{
        legend(x      = 'bottom',
               legend = dnames,
               pch    = 15,
               col    = cbPalette[1:ncol(x)],
               pt.cex = 2,
               cex    = 1.2,
               horiz  = T,
               bty    = 'n',
               ncol   = floor(length(c(dnames))/2),
               x.intersp = 1,
               y.intersp = 1,
               inset = 0
               )
      }
    }
    
  }
}
