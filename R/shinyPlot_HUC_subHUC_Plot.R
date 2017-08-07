#' Time Series of subHUCs Within Selected Region
#'
#' Description goes here.
#' @param x Numeric; The values to be plotted.
#' @param col Character; The plot color.
#' @export
#' @return Numeric vector.
#' @examples
#' shinyPlot_HUC_Time_Series_and_Difference()
shinyPlot_HUC_subHUC_Plot   <- function(default. = FALSE,
                                        feederList. = NULL,
                                        subToHUC. = subToHUC,
                                        subHUC10. = subHUC10,
                                        logPlotSubset. = logPlotSubset,
                                        subsetMonths. = subsetMonths,
                                        subsetbyMonthsFun. = subsetbyMonthsFun,
                                        cbPalette. = cbPalette,
                                        maphuc. = maphuc,
                                        dataCategory. = dataCategory,
                                        ablCol = 'darkgrey',
                                        ...){
  
  # default. = FALSE,
  # subToHUC. = subToHUC,
  # subHUC10. = subHUC10,
  # sliderTime. = NULL,
  # resample. = TRUE,
  # logPlotSubset. = logPlotSubset,
  # subsetMonths. = subsetMonths,
  # subsetbyMonthsFun. = subsetbyMonthsFun,
  # cbPalette. = cbPalette,
  # alpha. = 1,
  # maphuc. = maphuc,
  # dataCategory. = dataCategory,
  # ablCol = 'darkgrey'
  
  # Extract sliderTime. from feederList
  if (!is.null(feederList.)){
    resample. = feederList.$sample_subHUCs
    alpha.    = feederList.$alpha_slider
    sliderTime. = feederList.$slider_time
  }else{
    resample. <- NULL
    alpha. <- NULL
    sliderTime. <- NULL
  }
  
  if (default.){
    par(mar = c(5.1, 4.1, 4.1, 1.4))
    if (is.null(sliderTime.)){
      drange <- NULL
    }else{
      # drange <- lubridate::decimal_date(as.Date(sliderTime.))
      drange <- as.Date(ISOdate(year = sliderTime.,
                                month = c(1,12),
                                day = c(1,31)))
    }
    plot(x    = 1,
         type = 'n',
         main = paste('Subset to HUC ##'),
         xaxs = "i",
         ylab = 'units',
         xlab = 'Time',
         xlim = drange)
    par(mar = c(5.1, 4.1, 4.1, 2.1))
  }else{
    
    par(mar = c(5.1, 4.1, 4.1, 1.4))
    if (is.null(sliderTime.)){
      drange <- NULL
    } else{
      #drange <- lubridate::decimal_date(as.Date(sliderTime.))
      drange <- as.Date(ISOdate(year = sliderTime.,
                                month = c(1,12),
                                day = c(1,31)))
    }
    #subset reactive Data by matching the extensions of the list names to the selected HUC level
    data.lineHUCS <<- subHUC10.  #<<<---- might need to go into "return" ---->>>
    cols2keep <- 1:ncol(data.lineHUCS[[1]])
    
    #If more than 100 lines to plot, randomly sample down to 100
    if (resample.){
      if (length(cols2keep) > 50){
        if (ncol(subToHUC.) <= 2){
          cols2keep <- sample(x    = cols2keep,
                              size = 50)
          samptext  <- 'Data resample.d to n=50'
        }else{
          cols2keep <- sample(x    = cols2keep,
                              size = 25)
          samptext <- 'Data resample.d to n=25'
        }
      }else{
        samptext <- ''
      }
    }else{
      samptext <- ''
    }
    
    # Subset to months/seasons if required
    if (logPlotSubset.){
      if (length(subsetMonths.) > 1){
        data.lineHUCS <- lapply(X   = data.lineHUCS,
                                FUN = subsetbyMonthsFun.,
                                mts = subsetMonths.)
      }else if (!is.na(subsetMonths.)){
        data.lineHUCS <- lapply(X   = data.lineHUCS,
                                FUN = subsetbyMonthsFun.,
                                mts = subsetMonths.)
      }
    }
    # Subset zoo object to dates
    redDate <- function(x, drange){
      y <- x[((as.Date(zoo::index(x)) >= as.Date(drange[1])) & (as.Date(zoo::index(x)) <= as.Date(drange[2]))),]
      if (is.null(dim(y))) {
        x <- zoo::as.zoo(as.data.frame(y))
        zoo::index(x) <- as.Date(zoo::index(y))
      }else{
        x <- y
      }
      return(x)
    }
    data.lineHUCS <- lapply(X = data.lineHUCS,
                            FUN = redDate,
                            drange = drange)

    #collect max values from list
    ylmax <- max(unlist(lapply(X = data.lineHUCS,
                               FUN = max,
                               na.rm = T)))
    
    progmax <- (length(data.lineHUCS)-1)*length(cols2keep)
    shiny::withProgress(message = 'Plotting in progress',
                 detail  = 'Please wait...',
                 value   = 0, {
                   #Get max for ylim
                   ymaxx <- max(unlist(lapply(X   = data.lineHUCS,
                                              FUN = function(x) {
                                                return(max(x[,cols2keep], na.rm = T))
                                              })))
                   plot(x    = (data.lineHUCS[[1]])[,cols2keep[1]],
                        type = 'l',
                        col  = scales::alpha(colour = cbPalette.[1],
                                             alpha  = alpha.),
                        lwd  = 1,
                        lty  = 1,
                        main = paste('HUC subsetted to HUC',
                                     as.character(maphuc.)),
                        xaxs = "i",
                        yaxs = "i",
                        ylab = dataCategory.,
                        xlab = 'Time',
                        #xlim = drange,
                        ylim = c(0, ymaxx)) #input$ylimmax_slider
                   # abline(v   = unique(as.integer(index(subToHUC.))),
                   #        col = ablCol,
                   #        lty = 2)
                   yrs <- as.Date(ISOdate(year = unique(lubridate::year(index(subToHUC.))),
                                          month = 1,
                                          day = 1))
                   abline(v   = yrs,
                          col = ablCol,
                          lty = 2)
                   if (length(cols2keep) > 1){
                     for (j in 2:length(cols2keep)){
                       lines(x   = (data.lineHUCS[[1]])[,cols2keep[j]],
                             col = scales::alpha(colour = cbPalette.[1],
                                                 alpha  = alpha.),
                             lwd = 2)
                       incProgress(1/progmax)
                     }
                   }
                   if (length(data.lineHUCS) > 1){
                     for (i in 2:length(data.lineHUCS)){
                       for (j in 1:length(cols2keep)){
                         lines(x   = (data.lineHUCS[[i]])[,cols2keep[j]],
                               col = scales::alpha(colour = cbPalette.[i],
                                                   alpha  = alpha.),
                               lwd = 2)
                         incProgress(1/progmax)
                       }
                     }
                     legend(x = 'topleft',
                            legend = samptext,
                            bty = 'n')
                   }
                 })
  }
}
