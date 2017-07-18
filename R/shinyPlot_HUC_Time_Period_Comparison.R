#' Dataset Time Period Comparison
#'
#' Plot displaying the temporal overlap of the selected datasets.
#' @param x Numeric; The values to be plotted.
#' @param col Character; The plot color.
#' @export
#' @return Numeric vector.
#' @examples
#' shinyPlot_HUC_Time_Series_and_Difference()
shinyPlot_HUC_Time_Period_Comparison <- function(default. = FALSE,
                                                 dnames. = dnames,
                                                 maphuc. = maphuc,
                                                 timeStep. = timeStep,
                                                 fileInfo. = fileInfo,
                                                 ...){
  if (default.){
    par(mar=c(1,2.5,2.1,2.1))
    defRange <- seq.Date(as.Date('2000-01-01'),
                        as.Date('2010-01-01'),
                        'month')
    plot(x = defRange,
         y = 1:length(defRange),
         type = 'n',
         xlab = '',
         ylab = '',
         xaxt = 'n',
         yaxt = 'n',
         xaxs = 'i',
         yaxs = 'i')
    rect(xleft   = par("usr")[1],
         ybottom = par("usr")[3],
         xright  = par("usr")[2],
         ytop    = par("usr")[4],
         col     = 'lightgrey',
         border  = 'lightgrey')
    mtext(text = 'Datasets',
          side = 2,
          line = 1,
          font = 2)
    mtext(text = 'Temporal Ranges of Datasets',
          side = 3,
          font = 2,
          cex = 1.5,
          line = 0.9)
  }else{
    subInfo <- fileInfo %>%
      filter(dataName %in% dnames.) %>%
      filter(HUC %in% maphuc.) %>%
      filter(timeStep %in% timeStep.)
    allDates <- c(as.Date(subInfo$startDate), as.Date(subInfo$endDate))
    dRange <- c(min(allDates), max(allDates))
    seqDrange <- seq.Date(from = as.Date(dRange[1]),
                          to   = as.Date(dRange[2]),
                          by   = timeStep.)
    # Multiplot params
    #par(mfrow = c(2,1))
    cols <- rep(x = RColorBrewer::brewer.pal(n    = 8,
                                             name = 'Dark2'),
                times = 3)
    #splitfun
    timeBlocks <- function(y, fileCount){
      x         <- split(y, ceiling(seq_along(y)/(length(y)/fileCount)))
      minBlocks <- lapply(x, min)
      maxBlocks <- lapply(x, max)
      return(mapply(FUN      = c,
                    minBlocks,
                    maxBlocks,
                    SIMPLIFY = F))
    }
    # Overlapping time ranges
    seqFun <- function(dd){
      return(seq.Date(from = as.Date(dd[1,1]),
                      to   = as.Date(dd[1,2]),
                      by   = 'month'))
    }
    
    iRanges <- subInfo[,c('startDate', 'endDate')]
    iRanges <- split(iRanges, seq(nrow(iRanges)))
    iRanges <- range(as.Date(Reduce(intersect, lapply(X   = iRanges,
                                                      FUN = seqFun)),
                             origin = lubridate::origin))
    # Plot rectangles
    par(mar=c(1,2.5,2.1,2.1))
    plot(x = seqDrange,
         y = 1:length(seqDrange),
         type = 'n',
         xlab = '',
         ylab = '',
         xaxt = 'n',
         yaxt = 'n',
         xaxs = 'i',
         yaxs = 'i')
    rect(xleft   = par("usr")[1],
         ybottom = par("usr")[3],
         xright  = par("usr")[2],
         ytop    = par("usr")[4],
         col     = 'lightgrey',
         border  = 'lightgrey')
    mtext(text = 'Datasets',
          side = 2,
          line = 1,
          font = 2)
    mtext(text = 'Temporal Ranges of Datasets',
          side = 3,
          font = 2,
          cex = 1.5,
          line = 0.9)
    inputBlocks <- timeBlocks(y         = 1:length(seqDrange),
                              fileCount = nrow(subInfo))
    
    for (i in 1:nrow(subInfo)){
      dataName  <- subInfo$dataName[i]
      startDate <- as.Date(subInfo$startDate[i])
      endDate   <- as.Date(subInfo$endDate[i])
      yRanges   <- inputBlocks[[i]]
      rect(xleft   = startDate,
           ybottom = yRanges[1],
           xright  = endDate,
           ytop    = yRanges[2],
           col     = scales::muted(cols[i]),
           border  = scales::muted(cols[i]))
      text(x = median(seq.Date(from = startDate,
                               to   = endDate,
                               by   = 'month')),
           y = mean(yRanges[1]:yRanges[2]),
           labels = dataName,
           font = 2,
           cex = 1.5,
           col = 'white')
    }
    abline(v = iRanges, col = 'red', lwd = 3, lty = 3)
    mtext(iRanges[1], at = iRanges[1])
    mtext(iRanges[2], at = iRanges[2])
    
  }
}
# # Render plot of date rectangles
# output$timeperiodPlot <- renderPlot({
#   allDates <- as.Date(c(inputMetadata$startDate,
#                         inputMetadata$endDate,
#                         outputMetadata$startDate,
#                         outputMetadata$endDate))
#   dRange <- c(min(allDates), max(allDates))
#   seqDrange <- seq.Date(from = dRange[1],
#                         to   = dRange[2],
#                         by   = input$timeStep)
#   # Multiplot params
#   par(mfrow = c(2,1))
#   cols <- rep(x = RColorBrewer::brewer.pal(n    = 8,
#                                            name = 'Dark2'),
#               times = 3)
#   #splitfun
#   timeBlocks <- function(y, fileCount){
#     x         <- split(y, ceiling(seq_along(y)/(length(y)/fileCount)))
#     minBlocks <- lapply(x, min)
#     maxBlocks <- lapply(x, max)
#     return(mapply(FUN      = c,
#                   minBlocks,
#                   maxBlocks,
#                   SIMPLIFY = F))
#   }
#   
#   # Overlapping time ranges
#   seqFun <- function(dd){
#     return(seq.Date(from = as.Date(dd[1,1]),
#                     to   = as.Date(dd[1,2]),
#                     by   = 'month'))
#   }
#   
#   iRanges <- rbind(inputMetadata[,c('startDate', 'endDate')],
#                    outputMetadata[,c('startDate', 'endDate')])
#   iRanges <- split(iRanges, seq(nrow(iRanges)))
#   print(dim(iRanges[[1]]))
#   iRanges <- range(as.Date(Reduce(intersect, lapply(X   = iRanges,
#                                                     FUN = seqFun))))
#   
#   # Plot 1:  Inputs
#   #par(mar=c(2.1,2.1,2.1,2.1))
#   par(mar=c(0.0,2.1,2.1,2.1))
#   plot(x = seqDrange,
#        y = 1:length(seqDrange),
#        type = 'n',
#        xlab = '',
#        ylab = '',
#        xaxt = 'n',
#        yaxt = 'n',
#        xaxs = 'i',
#        yaxs = 'i')
#   rect(xleft   = par("usr")[1],
#        ybottom = par("usr")[3],
#        xright  = par("usr")[2],
#        ytop    = par("usr")[4],
#        col     = 'lightgrey',
#        border  = 'lightgrey')
#   mtext(text = 'Inputs',
#         side = 2,
#         line = 1,
#         font = 2)
#   mtext(text = 'Temporal Ranges of Datasets',
#         side = 3,
#         font = 2,
#         cex = 1.5,
#         line = 0.9)
#   inputBlocks <- timeBlocks(y         = 1:length(seqDrange),
#                             fileCount = length(inputFiles))
#   
#   for (i in 1:length(inputFiles)){
#     dataName  <- inputMetadata$dataName[i]
#     startDate <- as.Date(inputMetadata$startDate[i])
#     endDate   <- as.Date(inputMetadata$endDate[i])
#     yRanges   <- inputBlocks[[i]]
#     rect(xleft   = startDate,
#          ybottom = yRanges[1],
#          xright  = endDate,
#          ytop    = yRanges[2],
#          col     = scales::muted(cols[i]),
#          border  = scales::muted(cols[i]))
#     text(x = median(seq.Date(from = startDate,
#                              to   = endDate,
#                              by   = 'month')),
#          y = mean(yRanges[1]:yRanges[2]),
#          labels = dataName,
#          font = 2,
#          cex = 1.5,
#          col = 'white')
#   }
#   abline(v = iRanges, col = 'red', lwd = 3, lty = 3)
#   mtext(iRanges[1], at = iRanges[1])
#   mtext(iRanges[2], at = iRanges[2])
#   
#   
#   # Plot 2: Outputs
#   #par(mar=c(3.1,2.1,1.1,2.1))
#   par(mar=c(3.1,2.1,0.0,2.1))
#   plot(x = seqDrange,
#        y = 1:length(seqDrange),
#        type = 'n',
#        xlab = '',
#        ylab = 'Outputs',
#        yaxt = 'n',
#        xaxs = 'i',
#        yaxs = 'i')
#   rect(xleft   = par("usr")[1],
#        ybottom = par("usr")[3],
#        xright  = par("usr")[2],
#        ytop    = par("usr")[4],
#        col     = 'lightgrey',
#        border  = 'lightgrey')
#   mtext(text = 'Outputs',
#         side = 2,
#         line = 1,
#         font = 2)
#   outputBlocks <- timeBlocks(y         = 1:length(seqDrange),
#                              fileCount = length(outputFiles))
#   for (i in 1:length(outputFiles)){
#     dataName  <- outputMetadata$dataName[i]
#     startDate <- as.Date(outputMetadata$startDate[i])
#     endDate   <- as.Date(outputMetadata$endDate[i])
#     yRanges   <- outputBlocks[[i]]
#     rect(xleft   = startDate,
#          ybottom = yRanges[1],
#          xright  = endDate,
#          ytop    = yRanges[2],
#          col     = scales::muted(cols[i+length(inputFiles)]),
#          border  = scales::muted(cols[i+length(inputFiles)]))
#     text(x = median(seq.Date(from = startDate,
#                              to   = endDate,
#                              by   = 'month')),
#          y = mean(yRanges[1]:yRanges[2]),
#          labels = dataName,
#          font = 2,
#          cex = 1.5,
#          col = 'white')
#   }
#   abline(h = length(seqDrange), col = 'black', lwd = 3)
#   abline(v = iRanges, col = 'red', lwd = 3, lty = 3)
#   