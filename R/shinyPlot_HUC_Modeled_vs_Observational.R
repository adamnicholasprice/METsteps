#' Modeled vs Observation vs Envelopes
#'
#' Description goes here.
#' @param x Numeric; The values to be plotted.
#' @param col Character; The plot color.
#' @export
#' @return Numeric vector.
#' @examples
#' shinyPlot_HUC_Modeled_vs_Observational()

shinyPlot_HUC_Modeled_vs_Observational   <- function(default. = FALSE,
                                                       feederList. = NULL,
                                                       x = subToHUC,
                                                       ptSP. = ptSP.trim,
                                                       timeStep. = timeStep,
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
  
  # Set xlim based on drange
  if (is.null(sliderTime.)){
    drange <- NULL
  }else{
    drange <- as.Date(ISOdate(year = sliderTime.,
                              month = c(1,12),
                              day = c(1,31)))
  }
  
  
  if (default.){
    layout(mat     = matrix(data  = c(1,2,3,4,5),
                            nrow  = 5,
                            ncol  = 1,
                            byrow = T),
           widths  = 1,
           heights = c(1,1,1,0.25,0.25))
    par(mar = c(0,0,0,0), oma = c(3,6,3,3))
    plot(1, type = 'n', xaxt = 'n', yaxt = 'n')
    mtext(text = 'HUC',
          line = 1,
          font = 2,
          cex = cex.main)
    mtext(text = paste0('Modeled','\n'),
          side = 2,
          line = 3)
    plot(1, type = 'n', xaxt = 'n', yaxt = 'n')
    mtext(text = paste0('All Obs','\n'),
          side = 2,
          line = 3)
    plot(1, type = 'n', xaxt = 'n', yaxt = 'n')
    mtext(text = paste0('Obs. Envelope','\n'),
          side = 2,
          line = 3)
    plot(1, type = 'n', xaxt = 'n', yaxt = 'n')
    mtext(text = paste0('n','\n'),
          side = 2,
          line = 3)
    plot(1, type = 'n', xaxt = 'n', yaxt = 'n')
    mtext(text = paste0('SD','\n'),
          side = 2,
          line = 3)
    
  }else{
    layout(mat     = matrix(data  = c(1,2,3,4,5),
                            nrow  = 5,
                            ncol  = 1,
                            byrow = T),
           widths  = 1,
           heights = c(1,1,1,0.25,0.25))
    par(mar = c(0,0,0,0), oma = c(3,6,3,3))
    
    # P1 - plot modeled data normally
    if (!is.null(drange)){
      y <- x[((as.Date(zoo::index(x)) >= as.Date(drange[1])) & (as.Date(zoo::index(x)) <= as.Date(drange[2]))),]
      if (is.null(dim(y))) {
        x <- zoo::as.zoo(as.data.frame(y))
        zoo::index(x) <- as.Date(zoo::index(y))
      }else{
        x <- y
      }
    }
    
    plot(x        = x[,1],
         col      = col[1],
         main     = '',
         xaxs     = "i",
         ylab     = paste0(dataCategory.,'\n(mm)'),
         #xlim     = drange,
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
    
    yrs <- as.Date(ISOdate(year = unique(lubridate::year(index(x))),
                           month = 1,
                           day = 1))
    abline(v   = yrs,
           col = ablCol,
           lty = 2)
    
    # P2 - plot observational data (all)
    if (!is.null(ptSP.)){
      if (nrow(ptSP.) > 0){
        # function for importing via OurID
        imFun <- function(id, path, ext = '.csv'){
          y <- zoo::read.zoo(file = paste0(path, id, ext))
          # if not zoo obj already...
          if (sum(y, na.rm = T) == 0){
            y <- read.csv(paste0(path, id, ext),
                          header = F,
                          stringsAsFactors = F)
            yy <- zoo::as.zoo(y[,2])
            zoo::index(yy) <- as.Date(y[,1])
            y <- yy
          }
          # fill in missing sequences with NAs
          fD <- seq.Date(from = min(zoo::index(y)),
                   to = max(zoo::index(y)),
                   by = timeStep.)
          fS <- rep(NA, length(fD))
          fS <- zoo::as.zoo(fS)
          zoo::index(fS) <- as.Date(fD)
          # merge
          yM <- merge(y, fS)
          yM <- yM[,1]
          
          #return
          return(yM)
        }
        allZoo <- lapply(X = ptSP.@data$OurID,
                         FUN = imFun,
                         path = path.obs.)
        # Calculate all relevant ranges for zoo data
        mindateZoo <- function(x){
          return(as.Date(zoo::index(x)[1]))
        }
        maxdateZoo <- function(x){
          return(as.Date(zoo::index(x)[length(x)]))
        }
        minvalZoo <- function(x){
          return(min(x, na.rm = T))
        }
        maxvalZoo <- function(x){
          return(max(x, na.rm = T))
        }
        
        # min(as.Date(unlist(lapply(X = allZoo, FUN = mindateZoo))), na.rm = T)
        # max(as.Date(unlist(lapply(X = allZoo, FUN = maxdateZoo))), na.rm = T)
        yRange <- c(floor(min(unlist(lapply(X = allZoo, FUN = minvalZoo)), na.rm = T)),
                    ceiling(max(unlist(lapply(X = allZoo, FUN = maxvalZoo)), na.rm = T)))
        
        # create plot
        plot(x        = x[,1],
             type     = 'n',
             main     = '',
             xaxs     = 'i',
             ylab     = 'Diff (mm)',
             xlab     = '',
             xaxt     = 'n',
             yaxt     = 'n',
             ylim     = yRange,
             cex.axis = multiplot.cex,
             cex.lab  = multiplot.lab)
        mtext(text = paste0('Obs','\n(mm)'),
              side = 2,
              line = 3)
        axis(side = 4,
             cex.axis = multiplot.cex)
        # Add obs data
        # for (i in 1:length(allZoo)){
        #   plot(allZoo[[i]])
        #   Sys.sleep(1)
        # }
        lapply(allZoo, lines)
        abline(v   = yrs,
               col = ablCol,
               lty = 2)
        
      } 
      
      
      # P3 - plot observational envelopes
      z <- do.call("merge.zoo", allZoo)
      if (!is.null(dim(z))){
        # Define full date sequence as NAs
        indMer <- seq.Date(min(index(z)), max(index(z)), by = 'month')
        indMer2 <- zoo::as.zoo(rep(NA, length(indMer)))
        
        # Identify min/max for full sequence
        zoo::index(indMer2) <- indMer
        zRange <- suppressWarnings(apply(z, MARGIN = 1, FUN = range, na.rm = T))
        zRange[is.infinite(zRange)] <- NA
        
        envMax <- as.zoo(zRange[2,])
        zoo::index(envMax) <- as.Date(colnames(zRange))
        envMax <- merge(envMax, indMer2)[,1]
        
        envMin <- as.zoo(zRange[1,])
        zoo::index(envMin) <- as.Date(colnames(zRange))
        envMin <- merge(envMin, indMer2)[,1]
        
        # Identify quantiles when there are > 2 measurements per date
        quant.25.75 <- function(x){
          x <- as.numeric(x)
          if (sum(!is.na(x)) > 2){
            return(as.numeric(stats::quantile(x, probs = c(0.25, 0.75), na.rm = T)))
          }else{
            return(c(NA, NA))
          }
        }
        
        envQuant <- apply(z, MARGIN = 1, FUN = quant.25.75)
        env75 <- as.zoo(envQuant[2,])
        zoo::index(env75) <- as.Date(colnames(envQuant))
        env75 <- merge(env75, indMer2)[,1]
        
        env25 <- as.zoo(envQuant[1,])
        zoo::index(env25) <- as.Date(colnames(envQuant))
        env25 <- merge(env25, indMer2)[,1]
        
        # Trim NAs
        env75 <- zoo::na.trim(env75)
        env25 <- zoo::na.trim(env25)
        
        #Calculate means of obs
        envMean <- as.zoo(rowMeans(z, na.rm = T))
        zoo::index(envMean) <- as.Date(colnames(envQuant))
        envMean <- merge(envMean, indMer2)[,1]
        #Calculate medians of obs
        envMedian <- as.zoo(apply(z, MARGIN = 1, FUN = median, na.rm = T))
        zoo::index(envMedian) <- as.Date(colnames(envQuant))
        envMedian <- merge(envMedian, indMer2)[,1]
  
        # create plot
        plot(x        = x[,1],
             type     = 'n',
             main     = '',
             xaxs     = "i",
             ylab     = '',
             xlab     = '',
             xaxt     = 'n',
             ylim     = yRange,
             cex.axis = multiplot.cex,
             cex.lab  = multiplot.lab)
        mtext(text = paste0('Obs. Envelope','\n(mm)'),
              side = 2,
              line = 3)
        # max lines
          #points(envMax, pch = 24, col = 'black', bg = 'black')
          #points(envMin, pch = 25, col = 'black', bg = 'black')
        
        # Min/Max poly
        age <- as.Date(zoo::index(envMax))
        y.high <- as.numeric(envMax)
        y.low <- as.numeric(envMin)
        METsteps::envelope(xall <- age,
                           y1 = y.high,
                           y2 = y.low,
                           col = 'lightgrey',
                           border = 'lightgrey')
        
        # 25/75 poly
        age <- as.Date(zoo::index(env75))
        y2.high <- as.numeric(env75)
        y2.low <- as.numeric(env25)
        METsteps::envelope(xall <- age,
                           y1 = y2.high,
                           y2 = y2.low,
                           col = 'darkgrey',
                           border = 'darkgrey')
        
        # plot median
        lines(envMedian, lty = 2)
  
        abline(v   = yrs,
               col = ablCol,
               lty = 2)
      }
      
      # P4 - Plot barplot of counts
      ctFun <- function(x){
        x <- as.numeric(x)
        return(sum(!is.na(x)))
      }
      cts <- apply(z, MARGIN = 1, FUN = ctFun)
      cts <- as.zoo(cts)
      index(cts) <- as.Date(zoo::index(z))
      # Create blank plot
      plot(x        = x[,1],
           type     = 'n',
           main     = '',
           xaxs     = "i",
           xaxt     = 'n',
           yaxt     = 'n',
           ylab     = '',
           xlab     = 'Time',
           ylim     = range(cts, na.rm = T),
           cex.axis = multiplot.cex,
           cex.lab  = multiplot.lab)
      mtext(text = paste0('n','\n'),
            side = 2,
            line = 3)
      axis(side = 4)
      lines(cts, type = 'h')
      abline(v   = yrs,
             col = ablCol,
             lty = 2)
      
      # P5 - Standard deviation of 
      sds <- apply(z, MARGIN = 1, FUN = sd, na.rm = T)
      sds <- as.zoo(sds)
      index(sds) <- as.Date(zoo::index(z))
      plot(x        = x[,1],
           type     = 'n',
           main     = '',
           xaxs     = "i",
           ylab     = '',
           xlab     = 'Time',
           ylim     = range(sds, na.rm = T),
           cex.axis = multiplot.cex,
           cex.lab  = multiplot.lab)
      mtext(text = paste0('SD','\n(mm)'),
            side = 2,
            line = 3)
      lines(sds, col = 'red')
      abline(v   = yrs,
             col = ablCol,
             lty = 2)
    }else{
      plot(1, type = 'n', xaxt = 'n', yaxt = 'n')
      mtext(text = paste0('All Obs','\n'),
            side = 2,
            line = 3)
      text(x = mean(par()$usr[1:2]),
           y = mean(par()$usr[3:4]),
           labels = 'No observational data supplied',
           col = 'red',
           cex = 1.25)
      plot(1, type = 'n', xaxt = 'n', yaxt = 'n')
      mtext(text = paste0('Obs. Envelope','\n'),
            side = 2,
            line = 3)
      plot(1, type = 'n', xaxt = 'n', yaxt = 'n')
      mtext(text = paste0('n','\n'),
            side = 2,
            line = 3)
      plot(1, type = 'n', xaxt = 'n', yaxt = 'n')
      mtext(text = paste0('SD','\n'),
            side = 2,
            line = 3)
    }
  }
  
}








