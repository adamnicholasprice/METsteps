#' Mean Percentile and ECDF Plots
#'
#' Description goes here.
#' @param x Numeric; The values to be plotted.
#' @param col Character; The plot color.
#' @export
#' @return Numeric vector.
#' @examples
#' shinyPlot_HUC_Time_Series_and_Difference()

shinyPlot_HUC_Mean_Percentile_and_ECDF   <- function(default. = FALSE,
                                                     subToHUC. = subToHUC,
                                                     dataCategory. = dataCategory,
                                                     ablCol = 'darkgrey',
                                                     cbPalette. = cbPalette,
                                                     dnames. = dnames,
                                                     ...){
  if (default.){
    # par(mfrow = c(1, 2))
    # par(mar = c(1, 2, 2.5, 2))
    layout(mat     = matrix(data  = c(1,2,3,3),
                            nrow  = 2,
                            byrow = T),
           heights = c(0.85, 0.15))
    par(mar = c(1, 2, 3.5, 2))
    plot(x    = 1,
         type = 'n',
         ylim = c(0,100),
         xlim = c(1,12),
         xaxs = 'i',
         xaxt = 'n',
         ylab = 'units',
         xlab = '',
         main = '25-75th Percentile Monthly Ranges')
    axis(side      = 1,
         at     = 1:12,
         labels = c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep'))
    plot(x    = 1,
         type = 'n',
         ylim = c(0, 1),
         xlim = c(0,50),
         xaxs = 'i',
         ylab = 'Fn(x)',
         xlab = 'x',
         main = 'Empirical Cumulative Distribution Function')
    abline(h   = c(0,1),
           col = 'grey',
           lty = 2)
    par(mfrow = c(1,2))
  }else{
    layout(mat     = matrix(data  = c(1,2,3,3),
                            nrow  = 2,
                            byrow = T),
           heights = c(0.85, 0.15))
    par(mar = c(1, 2, 3.5, 2))
    # Calculate mean values
    ensembleMeans <- zoo::zoo(rowMeans(subToHUC.))
    zoo::index(ensembleMeans) <- zoo::index(subToHUC.)
    subToHUC.2 <- cbind(subToHUC., ensembleMeans)
    #Monthly quantile envelope plots
    monthlylist.y <- vector(mode   = "list",
                            length = ncol(subToHUC.2))
    medList <- monthlylist.y
    
    for (i in 1:ncol(subToHUC.2)){
      list.month <- vector(mode = 'list',
                           length = 12)
      data.vec   <- subToHUC.2[,i]
      months.all <- lubridate::month((lubridate::date_decimal(index(data.vec)+0.00001)))
      
      for (j in 1:12){
        curmon          <- data.vec[months.all == j]
        list.month[[j]] <- curmon
      }
      quants.25        <- unlist(lapply(X     = list.month,
                                        FUN   = stats::quantile,
                                        probs = 0.25,
                                        na.rm = T))
      quants.75         <- unlist(lapply(X     = list.month,
                                         FUN   = stats::quantile,
                                         probs = 0.75,
                                         na.rm = T))
      quants            <- rbind(quants.25, quants.75)
      medList[[i]]      <- unlist(lapply(X     = list.month,
                                         FUN   = median,
                                         na.rm = T))
      #Remove list
      if (exists('list.month')) rm(list.month)
      #reorder to start with oct
      quants             <- quants[, c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9)]
      medList[[i]]       <- medList[[i]][c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9)]
      xvals              <- c(1:12, 12, rev(1:12), 1)
      
      monthlylist.y[[i]] <- c(quants[1,], quants[2,12], rev(quants[2,]), quants[2,1])
    }
    
    ymaxx <- max(unlist(lapply(X   = monthlylist.y,
                               FUN = base::max)))
    plot(x    = 1,
         type = 'n',
         ylim = c(0, ymaxx),
         xlim = c(1, 12),
         xaxs = 'i',
         xaxt = 'n',
         ylab = dataCategory.,
         xlab = '',
         main = '25-75th Percentile Monthly Ranges')
    abline(v   = 2:11,
           col = ablCol,
           lty = 2)
    axis(side   = 1,
         at     = 1:12,
         labels = c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep'))
    
    for (i in 1:ncol(subToHUC.2)){
      if (i != ncol(subToHUC.2)){
        colour <- cbPalette.[i]
      }else{
        colour = 'darkgrey'
      }
      
      polygon(x      = xvals,
              y      = monthlylist.y[[i]],
              col    = scales::alpha(colour = colour,
                                     alpha  = 0.5),
              border = NA)
      lines(x   = medList[[i]],
            lty = 2,
            lwd = 0.5)
      points(x   = medList[[i]],
             pch = 16,
             cex = 0.75)
    }
    #ecdf plot
    plot(x    = ecdf(as.numeric(subToHUC.2[,1])),
         col  = cbPalette.[1],
         main = 'Empirical Cumulative Distribution Function')
    if (ncol(subToHUC.2) > 1){
      for (i in 2:ncol(subToHUC.2)){
        if (i != ncol(subToHUC.2)){
          colour <- cbPalette.[i]
        }else{
          colour = 'darkgrey'
        }
        plot(x   = ecdf(as.numeric(subToHUC.2[,i])),
             col = colour,
             add = T)
      }
    }
    #Add common legend
    par(mar = c(2, 1, 1, 1))
    plot(x    = 1,
         type = 'n',
         axes = FALSE,
         xlab = '',
         ylab = '',
         bty = 'n')
    legend(x      = 'bottom',
           legend = c(dnames., 'Ensemble Mean'),
           inset  = 0,
           horiz  = TRUE,
           pch    = 15,
           col    = c(cbPalette.[1:(ncol(subToHUC.2)-1)], 'darkgrey'),
           pt.cex = 2,
           cex    = 1,
           bty = 'n')
  }
}
