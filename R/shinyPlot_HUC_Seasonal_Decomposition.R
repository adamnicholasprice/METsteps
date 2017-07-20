#'Decomposition Plot
#'Decomposes time series into seasonal, irregular, and trend components.
#'Source: R. B. Cleveland, W. S. Cleveland, J.E. McRae, and I. Terpenning (1990) STL: A Seasonal-Trend Decomposition Procedure Based on Loess. Journal of Official Statistics, 6, 3--73.
#' Author: Flannery Dolan, Date Created: 07-17-17, Last Edited: 07-18-17
#' @param subToHUC. Numeric; .
#' @export
#' @return Seasonal decomposition plot.
#' @examples
#' shinyPlot_HUC_Seasonal_Decomposition()


shinyPlot_HUC_Seasonal_Decomposition <- function(default.  = FALSE,
                                                 feederList. = NULL,
                                                 subToHUC. = subToHUC,
                                                 timeStep. = timeStep,
                                                 cbPalette. = cbPalette,
                                                 ...){
  if (!is.null(feederList.)){
    sliderTime. <- feederList.$slider_time
  }else{
    sliderTime. <- NULL
  }
  if (is.null(sliderTime.)){
    drange <- NULL
  }else{
    drange <- lubridate::decimal_date(as.Date(sliderTime.))
  }
  
  if (default.){
    par(mfrow = c(4,1))
    par(mar = c(0, 0, 0, 0), oma = c(6, 6, 4, 3))
    defaultTS <- ts(data = 1:11,
                    start = 2000,
                    end = 2010,
                    frequency = 1)
    #data
    plot(defaultTS,
         type = 'n',
         xaxt = 'n',
         yaxt = 'n',
         xaxs = 'i')
    mtext(paste('Decomposed Time Series'),
          side = 3,
          line = 1, font = 2)
    mtext('data', side = 2, line = 3)
    axis(side = 2,
         tck = -0.01,
         cex.axis = 1.2)
    axis(side = 4,
         tck = -0.01,
         labels = FALSE)
    #seasonal
    plot(defaultTS,
         type = 'n',
         xaxt = 'n',
         yaxt = 'n',
         xaxs = 'i')
    mtext('seasonal', side = 2, line = 3)
    axis(side = 4,
         tck = -0.01,
         cex.axis = 1.2)
    axis(side = 2,
         tck = -0.01,
         labels = FALSE)
    #trend
    plot(defaultTS,
         type = 'n',
         xaxt = 'n',
         yaxt = 'n',
         xaxs = 'i')
    mtext('trend', side = 2, line = 3)
    axis(side = 2,
         tck = -0.01,
         cex.axis = 1.2)
    axis(side = 4,
         tck = -0.01,
         labels = FALSE)
    #remainder
    plot(defaultTS,
         type = 'n',
         yaxt = 'n',
         xaxt = 'n',
         xaxs = 'i')
    mtext('remainder', side = 2, line = 3)
    mtext('time', side = 1, line = 3)
    axis(side = 1,
         tck = -0.01,
         cex.axis = 1.2)
    axis(side = 4,
         tck = -0.01,
         cex.axis = 1.2)
    axis(side = 2,
         tck = -0.01,
         labels = FALSE)
  }else{
    ts_dates  <- zoo::index(subToHUC.)
    listSTL <- vector(mode = 'list', length = ncol(subToHUC.))
    
    for (i in 1:ncol(subToHUC.)){
      ts_values <- as.numeric(subToHUC.[,i])
      names(listSTL)[i] <- colnames(subToHUC.)[i]
      if(timeStep. =='month'){
        ts_monthly <- ts(data      = ts_values,
                         frequency = 12,
                         start     = c(floor(ts_dates[1]), round((ts_dates[1]-floor(ts_dates[1]))*12)+1))}
      else if(timeStep. =='day'){
        ts_monthly <- ts(data      = ts_values,
                         frequency = 365,
                         start     = c(floor(ts_dates[1]), round((ts_dates[1]-floor(ts_dates[1]))*365)+1))}
      else if(timeStep. =='week'){
        ts_monthly <- ts(data      = ts_values,
                         frequency = 52,
                         start     = c(floor(ts_dates[1]),round((ts_dates[1]-floor(ts_dates[1]))*52)+1))}
      else if(timeStep. =='year'){
        ts_monthly <- ts(data      = ts_values,
                         frequency = 1,
                         start     = c(floor(ts_dates[1]), 1))}
      else{
        print('Invalid timeStep')
      }
      
      
      stl_obj <- stl(x        = ts_monthly,
                     s.window = "periodic")
      decoData <- stl_obj$time.series
      saveData <- cbind(ts_values, decoData)
      colnames(saveData) <- c('data', colnames(decoData))
      listSTL[[i]] <- saveData
    }
    dataRange <- function(x){
      return(c( ceiling(max(x[,1], na.rm = T)),
                floor(min(x[,1], na.rm = T)) ))
    }
    seasonalRange <- function(x){
      return(c( ceiling(max(x[,2], na.rm = T)),
                floor(min(x[,2], na.rm = T)) ))
    }
    trendRange <- function(x){
      return(c( ceiling(max(x[,3], na.rm = T)),
                floor(min(x[,3], na.rm = T)) ))
    }
    remainderRange <- function(x){
      return(c( ceiling(max(x[,4], na.rm = T)),
               floor(min(x[,4], na.rm = T)) ))
    }
    
    
    
    par(mfrow = c(4,1))
    par(mar = c(0, 0, 0, 0), oma = c(6, 6, 4, 3))
    #data
    dataRvals <- range(unlist(lapply(listSTL, dataRange)))
    plot(listSTL[[1]][,1],
         type = 'n',
         ylim = dataRvals,
         xaxt = 'n',
         yaxt = 'n',
         xaxs = 'i',
         xlim = drange)
    mtext(paste('Decomposed Time Series for', paste0(colnames(subToHUC.), collapse = ', ')),
          side = 3,
          line = 1, font = 2)
    mtext('data', side = 2, line = 3)
    axis(side = 2,
         tck = -0.01,
         cex.axis = 1.2)
    axis(side = 4,
         tck = -0.01,
         labels = FALSE)
    for (i in 1:length(listSTL)){
      lines(listSTL[[i]][,1], col = cbPalette.[i])
    }
    #seasonal
    seasonalRvals <- range(unlist(lapply(listSTL, seasonalRange)))
    plot(listSTL[[1]][,2],
         type = 'n',
         ylim = seasonalRvals,
         xaxt = 'n',
         yaxt = 'n',
         xaxs = 'i',
         xlim = drange)
    mtext('seasonal', side = 2, line = 3)
    axis(side = 4,
         tck = -0.01,
         cex.axis = 1.2)
    axis(side = 2,
         tck = -0.01,
         labels = FALSE)
    for (i in 1:length(listSTL)){
      lines(listSTL[[i]][,2], col = cbPalette.[i])
    }
    #trend
    trendRvals <- range(unlist(lapply(listSTL, trendRange)))
    plot(listSTL[[1]][,3],
         type = 'n',
         ylim = trendRvals,
         xaxt = 'n',
         yaxt = 'n',
         xaxs = 'i',
         xlim = drange)
    mtext('trend', side = 2, line = 3)
    axis(side = 2,
         tck = -0.01,
         cex.axis = 1.2)
    axis(side = 4,
         tck = -0.01,
         labels = FALSE)
    for (i in 1:length(listSTL)){
      lines(listSTL[[i]][,3], col = cbPalette.[i])
    }
    #remainder
    remainderRvals <- range(unlist(lapply(listSTL, remainderRange)))
    plot(listSTL[[1]][,4],
         type = 'n',
         ylim = remainderRvals,
         yaxt = 'n',
         xaxt = 'n',
         xaxs = 'i',
         xlim = drange)
    for (i in 1:length(listSTL)){
      lines(listSTL[[i]][,4], col = cbPalette.[i],
            type = 'h')
    }
    abline(h=0)
    mtext('remainder', side = 2, line = 3)
    mtext('time', side = 1, line = 3)
    axis(side = 1,
         tck = -0.01,
         cex.axis = 1.2)
    axis(side = 4,
         tck = -0.01,
         cex.axis = 1.2)
    axis(side = 2,
         tck = -0.01,
         labels = FALSE)
    
    # 
    # ###################################################################
    # ts_dates  <- zoo::index(subToHUC.)
    # ts_values <- as.numeric(subToHUC.[,1])
    # ts_name   <- colnames(subToHUC.)[1]
    # if(timeStep. =='month'){
    #   ts_monthly <- ts(data      = ts_values,
    #                    frequency = 12,
    #                    start     = c(floor(ts_dates[1]), round((ts_dates[1]-floor(ts_dates[1]))*12)+1))}
    # else if(timeStep. =='day'){
    #   ts_monthly <- ts(data      = ts_values,
    #                    frequency = 365,
    #                    start     = c(floor(ts_dates[1]), round((ts_dates[1]-floor(ts_dates[1]))*365)+1))}
    # else if(timeStep. =='week'){
    #   ts_monthly <- ts(data      = ts_values,
    #                    frequency = 52,
    #                    start     = c(floor(ts_dates[1]),round((ts_dates[1]-floor(ts_dates[1]))*52)+1))}
    # else if(timeStep. =='year'){
    #   ts_monthly <- ts(data      = ts_values,
    #                    frequency = 1,
    #                    start     = c(floor(ts_dates[1]), 1))}
    # else{
    #   print('Invalid timeStep')
    # }    
    # stl_obj <- stl(x        = ts_monthly,
    #                s.window = "periodic")
    # plot(stl_obj, main = paste0('Time Series Decomposition of ', ts_name))
    #subToHUC<-zoo(ts_values,ts_dates)
  }
}