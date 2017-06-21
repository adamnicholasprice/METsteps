server <- function(input, output){
  # Define Data options
  output$equation_box <- renderUI({
    fileInfo.sub     <- fileInfo %>%
      filter(timeStep == input$timeStep) %>%
      filter(HUC == input$HUC)
    uniqueCategories <- unique(fileInfo.sub$dataCategory)
    
    # Define selectInput function
    boxcompSelection <- function(cat, fInfo){
      return(
        box(
          selectInput(inputId  = paste0('eqnComp_', cat),
                      label    = cat,
                      choices  = c(NA, fInfo %>%
                                     filter(dataCategory == cat) %>%
                                     distinct(dataName)),
                      width    = '100%'),
          radioButtons(inputId = paste0('eqnComp_', cat, '_sign'),
                       label   = 'I/O',
                       choices = c('Input', 'Output'),
                       inline  = T),
          width = 2,
          status = 'info')
      )
    }
    
    fluidRow(lapply(X     = uniqueCategories,
                    FUN   = boxcompSelection,
                    fInfo = fileInfo.sub))
  })
  
  # PUSH water balance components to equation!
  wbComps.push <- observeEvent(input$calculateEqn, {
    # Input objects
    fileInfo.sub     <- fileInfo %>%
      filter(timeStep == input$timeStep)
    uniqueCategories <- unique(fileInfo.sub$dataCategory)
    
    inputIdName      <- paste0('eqnComp_', uniqueCategories)
    callInputfun     <- function(x) input[[x]]
    dataName         <- unlist(lapply(X   = inputIdName,
                                      FUN = callInputfun))
    # IO objects
    inputIdSigns     <- paste0('eqnComp_', uniqueCategories, '_sign')
    signsIn          <- unlist(lapply(X   = inputIdSigns,
                                      FUN = callInputfun))
    # Combine corresponding parts
    productInfo      <- data.frame(dataName         = dataName,
                                   timeStep         = input$timeStep,
                                   HUC              = input$HUC,
                                   signsIn          = signsIn,
                                   stringsAsFactors = FALSE)
    productInfo      <- productInfo %>%
      filter(dataName != 'NA')
    # Separate into inputs and outputs
    inputInfo        <- productInfo %>%
      filter(signsIn == 'Input')
    inputNames       <- unique(inputInfo$dataName)
    
    outputInfo       <- productInfo %>%
      filter(signsIn == 'Output')
    outputNames      <- unique(outputInfo$dataName)
    
    if (!('Input' %in% unique(productInfo$signsIn)) || !('Output' %in% unique(productInfo$signsIn))){
      output$tab1errorText <- renderText({
        'Calculation requires at least ONE input and ONE output'
      })
      # Clear text
      output$tab1infoText  <- renderText({})
      # Return to default plot
      output$timeperiodPlot <- renderPlot({
        par(mfrow = c(2,1))
        par(mar=c(2.1,2.1,2.1,2.1))
        seqDrange = seq.Date(from = as.Date('1980-01-01'),
                             to   = as.Date(Sys.time()),
                             by   = 'month')
        plot(x = seqDrange,
             y = 1:length(seqDrange),
             type = 'n',
             xlab = '',
             ylab = '',
             yaxt = 'n',
             xaxs = 'i',
             yaxs = 'i')
        rect(xleft   = par("usr")[1],
             ybottom = par("usr")[3],
             xright  = par("usr")[2],
             ytop    = par("usr")[4],
             col     = 'lightgrey',
             border  = 'lightgrey')
        mtext(text = 'Inputs',
              side = 2,
              line = 1,
              font = 2)
        mtext(text = 'Temporal Ranges of Datasets',
              side = 3,
              font = 2,
              cex = 1.5,
              line = 0.75)
        par(mar=c(3.1,2.1,1.1,2.1))
        plot(x = seqDrange,
             y = 1:length(seqDrange),
             type = 'n',
             xlab = '',
             ylab = 'Outputs',
             yaxt = 'n',
             xaxs = 'i',
             yaxs = 'i')
        rect(xleft   = par("usr")[1],
             ybottom = par("usr")[3],
             xright  = par("usr")[2],
             ytop    = par("usr")[4],
             col     = 'lightgrey',
             border  = 'lightgrey')
        mtext(text = 'Outputs',
              side = 2,
              line = 1,
              font = 2)
        par(mfrow = c(1,1))
        par(mar=c(5.1,4.1,4.1,2.1))
      })
      # Return to default tables
      output$tab1tableInputs <- renderTable({
        data.frame(inputFiles = NA,
                   startDates = NA,
                   endDates   = NA)
      })
      output$tab1tableOutputs <- renderTable({
        data.frame(outputFiles = NA,
                   startDates  = NA,
                   endDates    = NA)
      })
    }else {
      # Render text under 'Calculate Water Balance Button
      output$tab1infoText    <- renderText({
        paste0('Calculating for inputs (', paste(unlist(inputNames), collapse = ', '), ')',
               ' and outputs (', paste(unlist(outputNames), collapse = ', '), '), where residuals = Inputs - Outputs')
      })
      output$tab1errorText   <- renderText({})
      
      # Calculate water balance
      #data return function
      dataReturn <- function(ioInfo){
        fnames <- fileInfo %>%
          filter(dataName %in% ioInfo$dataName) %>%
          filter(timeStep %in% ioInfo$timeStep) %>%
          filter(HUC      %in% ioInfo$HUC) %>%
          distinct(fnames)
        fnames <- unlist(fnames)
        if (length(fnames) < 1) fnames <- NA
        return(as.vector(fnames))
      }
      inputFiles  <- dataReturn(inputInfo)
      outputFiles <- dataReturn(outputInfo)
      
      # Get dataset dates
      inputMetadata  <- extractMetadata(inputFiles)
      outputMetadata <- extractMetadata(outputFiles)
      
      # Render filenames under button
      tblIN <- renderTable({
        data.frame(inputFiles = inputFiles,
                   startDates = inputMetadata$startDate,
                   endDates   = inputMetadata$endDate)
      })
      tblOUT <- renderTable({
        data.frame(outputFiles = outputFiles,
                   startDates  = outputMetadata$startDate,
                   endDates    = outputMetadata$endDate)
      })
      output$tab1tableInputs <- tblIN
      output$tab2tableInputs <- tblIN
      
      output$tab1tableOutputs <- tblOUT
      output$tab2tableOutputs <- tblOUT
      
      # Render plot of date rectangles
      output$timeperiodPlot <- renderPlot({
        allDates <- as.Date(c(inputMetadata$startDate,
                              inputMetadata$endDate,
                              outputMetadata$startDate,
                              outputMetadata$endDate))
        dRange <- c(min(allDates), max(allDates))
        seqDrange <- seq.Date(from = dRange[1],
                              to   = dRange[2],
                              by   = input$timeStep)
        # Multiplot params
        par(mfrow = c(2,1))
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
        
        iRanges <- rbind(inputMetadata[,c('startDate', 'endDate')],
                         outputMetadata[,c('startDate', 'endDate')])
        iRanges <- split(iRanges, seq(nrow(iRanges)))
        print(dim(iRanges[[1]]))
        iRanges <- range(as.Date(Reduce(intersect, lapply(X   = iRanges,
                                                          FUN = seqFun))))
        
        # Plot 1:  Inputs
        #par(mar=c(2.1,2.1,2.1,2.1))
        par(mar=c(0.0,2.1,2.1,2.1))
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
        mtext(text = 'Inputs',
              side = 2,
              line = 1,
              font = 2)
        mtext(text = 'Temporal Ranges of Datasets',
              side = 3,
              font = 2,
              cex = 1.5,
              line = 0.9)
        inputBlocks <- timeBlocks(y         = 1:length(seqDrange),
                                  fileCount = length(inputFiles))
        
        for (i in 1:length(inputFiles)){
          dataName  <- inputMetadata$dataName[i]
          startDate <- as.Date(inputMetadata$startDate[i])
          endDate   <- as.Date(inputMetadata$endDate[i])
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
        
        
        # Plot 2: Outputs
        #par(mar=c(3.1,2.1,1.1,2.1))
        par(mar=c(3.1,2.1,0.0,2.1))
        plot(x = seqDrange,
             y = 1:length(seqDrange),
             type = 'n',
             xlab = '',
             ylab = 'Outputs',
             yaxt = 'n',
             xaxs = 'i',
             yaxs = 'i')
        rect(xleft   = par("usr")[1],
             ybottom = par("usr")[3],
             xright  = par("usr")[2],
             ytop    = par("usr")[4],
             col     = 'lightgrey',
             border  = 'lightgrey')
        mtext(text = 'Outputs',
              side = 2,
              line = 1,
              font = 2)
        outputBlocks <- timeBlocks(y         = 1:length(seqDrange),
                                   fileCount = length(outputFiles))
        for (i in 1:length(outputFiles)){
          dataName  <- outputMetadata$dataName[i]
          startDate <- as.Date(outputMetadata$startDate[i])
          endDate   <- as.Date(outputMetadata$endDate[i])
          yRanges   <- outputBlocks[[i]]
          rect(xleft   = startDate,
               ybottom = yRanges[1],
               xright  = endDate,
               ytop    = yRanges[2],
               col     = scales::muted(cols[i+length(inputFiles)]),
               border  = scales::muted(cols[i+length(inputFiles)]))
          text(x = median(seq.Date(from = startDate,
                                   to   = endDate,
                                   by   = 'month')),
               y = mean(yRanges[1]:yRanges[2]),
               labels = dataName,
               font = 2,
               cex = 1.5,
               col = 'white')
        }
        abline(h = length(seqDrange), col = 'black', lwd = 3)
        abline(v = iRanges, col = 'red', lwd = 3, lty = 3)
        
        # Reset plot parameters
        par(mfrow = c(1,1))
        par(mar=c(5.1,4.1,4.1,2.1))
      })
      
      # 
      # Make sure dates overlap
      interval2 <- function(dates){
        return(
          lubridate::interval(start = as.Date(dates[1, 2]),
                              end   = as.Date(dates[1, 3])))
      }
      mashDates <- data.frame(fnames = c(inputFiles, outputFiles),
                              rbind(inputMetadata[,c('startDate', 'endDate')],
                                    outputMetadata[,c('startDate', 'endDate')]),
                              stringsAsFactors = FALSE)
      mashDates.ls <- split(x = mashDates,
                            f = seq(nrow(mashDates)))
      lsIntervals <- lapply(X   = mashDates.ls,
                            FUN = interval2)
      
      overlapTest <- (outer(X   = lsIntervals,
                            Y   = lsIntervals,
                            FUN = Vectorize(lubridate::int_overlaps)))
      
      if (sum(overlapTest == FALSE) > 0){
        output$tab1errorText <- renderText({
          'Error: Not all dates ranges of selected datasets overlap'
        })
        output$tab1infoText <- renderText({})
      } else {
        # Load input files
        inObj_no <- vector()
        for (i in 1:length(inputFiles)){
          tempName   <- paste0('inObj', i)
          inObj_no   <- c(inObj_no, tempName)
          im         <- data.frame(feather::read_feather(file.path(path.feather,
                                                                   inputFiles[i])))
          im.MD      <- extractMetadata(inputFiles[i])
          ind        <- lubridate::decimal_date(date = seq.Date(from       = as.Date(im.MD$startDate),
                                                                by         = im.MD$timeStep,
                                                                length.out = nrow(im)))
          im2        <- zoo::as.zoo(im)
          index(im2) <- ind
          assign(x     = tempName,
                 value = im2)
        }
        # Load output files
        outObj_no <- vector()
        for (i in 1:length(outputFiles)){
          tempName   <- paste0('outObj', i)
          outObj_no  <- c(outObj_no, tempName)
          im         <- data.frame(feather::read_feather(file.path(path.feather,
                                                                   outputFiles[i])))
          im.MD      <- extractMetadata(outputFiles[i])
          ind        <- lubridate::decimal_date(date = seq.Date(from       = as.Date(im.MD$startDate),
                                                                by         = im.MD$timeStep,
                                                                length.out = nrow(im)))
          im2        <- zoo::as.zoo(im)
          index(im2) <- ind
          assign(x     = tempName,
                 value = im2)
        }
        print(inObj_no)
        print(outObj_no)
        
        # Trim to overlapping date range
        #Put all data into a list, with names() being the data's name
        list.all       <- mget(c(inObj_no, outObj_no))
        #Collect indexes from each dataset (lapply index function) and find common values (reduce intersection)
        common.indices <- Reduce(f = intersect,
                                 x = lapply(X   = list.all,
                                            FUN = index))
        trimToIndices  <- function(datlist){
          datlist        <- datlist[(index(datlist) %in% common.indices),]
          index(datlist) <- common.indices
          return(datlist)
        }
        trimObjects    <- lapply(X   = list.all,
                                 FUN = trimToIndices)
        # Divide into lists of inputs and outputs again
        inputsTrimmed <- trimObjects[1:length(inObj_no)]
        outputsTrimmed <- trimObjects[(length(inObj_no) + 1):length(trimObjects)]
        
        # Sum inputs and outputs
        inputsSummer <- Reduce('+', inputsTrimmed)
        outputsSummer <- Reduce('+', outputsTrimmed)
        
        # Calculate remainders
        remainder <- inputsSummer - outputsSummer
        stValue   <- as.vector(colMeans(remainder, na.rm = T))
        
        # Import shapefile
        inShape2              <- get(paste0('polyHUC', input$HUC))
        # Find column name with 'HUC' in it
        pos                  <- regexpr(pattern = "HUC",
                                        text    = colnames(inShape2@data))
        # Rename column simply 'HUC'
        colnames(inShape2@data)[which(pos == 1)] <- 'HUC'
        
        # Magnitudes for color schema HUC.pb.mean.max
        mag <- max(c(max(stValue,
                         na.rm = T),
                     abs(min(stValue,
                             na.rm = T))))
        colVec <- inferno(3)
        colVec <- c(scales::muted('red'), 'white', scales::muted('green'))
        colorPal2 <- colorNumeric(palette = colVec,
                                  domain  = c(-mag, mag))
        # Add polygons to map
        minDate <- as.Date(lubridate::date_decimal(min(common.indices)))
        maxDate <- as.Date(lubridate::date_decimal(max(common.indices)))
        output$wbMapTitle <- renderText({
          paste0('Inputs (',
                 paste0(inputMetadata$dataName, collapse = ' + '),
                 ') - Outputs (',
                 paste0(outputMetadata$dataName, collapse = ' + '),
                 '), over time period ',
                 minDate,
                 ' to ',
                 maxDate
          )
        })
        leafletProxy(mapId = 'wbMap',
                     data  = inShape2) %>%
          clearShapes() %>%
          addPolygons(data            = inShape2,
                      weight          = 1,
                      color           = ~colorPal2(as.numeric(stValue)),
                      fillColor       = ~colorPal2(as.numeric(stValue)),
                      fillOpacity     = 1,
                      label           = ~stringr::str_c('HUC: ', HUC,',    ',
                                                        'State(s): ', STATES, ',  ',
                                                        'Statistic', ': ', round(stValue,2)),
                      layerId          = ~HUC,
                      smoothFactor     = 0.5,
                      highlightOptions = highlightOptions(color        = '#00ff00',
                                                          opacity      = 1,
                                                          weight       = 2,
                                                          fillOpacity  = 1,
                                                          bringToFront = T,
                                                          sendToBack   = T)
          ) %>%
          addPolylines(data   = states,
                       weight = 1,
                       color  = 'gray100') %>%
          addLegend("bottomleft",
                    pal = colorPal2,
                    values = as.numeric(stValue),
                    title = 'Residuals (mm)',
                    opacity = 1)
      }
    }
  })
  
  # Render default text
  output$tab1infoText  <- renderText({
    'Please select datasets for water balance equation'
  })
  output$tab1errorText <- renderText({})
  output$wbMapTitle    <- renderText({
    'Select datasets on first tab'
  })
  # Render default tables
  defaultPlot <- renderTable({
    data.frame(inputFiles = NA,
               startDates = NA,
               endDates   = NA)
  })
  output$tab1tableInputs  <- defaultPlot
  output$tab1tableOutputs <- defaultPlot
  output$tab2tableInputs  <- defaultPlot
  output$tab2tableOutputs <- defaultPlot
  
  
  # Render default time range plot
  output$timeperiodPlot <- renderPlot({
    par(mfrow = c(2,1))
    par(mar=c(2.1,2.1,2.1,2.1))
    seqDrange = seq.Date(from = as.Date('1980-01-01'),
                         to   = as.Date(Sys.time()),
                         by   = 'month')
    plot(x = seqDrange,
         y = 1:length(seqDrange),
         type = 'n',
         xlab = '',
         ylab = '',
         yaxt = 'n',
         xaxs = 'i',
         yaxs = 'i')
    rect(xleft   = par("usr")[1],
         ybottom = par("usr")[3],
         xright  = par("usr")[2],
         ytop    = par("usr")[4],
         col     = 'lightgrey',
         border  = 'lightgrey')
    mtext(text = 'Inputs',
          side = 2,
          line = 1,
          font = 2)
    mtext(text = 'Temporal Ranges of Datasets',
          side = 3,
          font = 2,
          cex = 1.5,
          line = 0.75)
    par(mar=c(3.1,2.1,1.1,2.1))
    plot(x = seqDrange,
         y = 1:length(seqDrange),
         type = 'n',
         xlab = '',
         ylab = 'Outputs',
         yaxt = 'n',
         xaxs = 'i',
         yaxs = 'i')
    rect(xleft   = par("usr")[1],
         ybottom = par("usr")[3],
         xright  = par("usr")[2],
         ytop    = par("usr")[4],
         col     = 'lightgrey',
         border  = 'lightgrey')
    mtext(text = 'Outputs',
          side = 2,
          line = 1,
          font = 2)
    par(mfrow = c(1,1))
    par(mar=c(5.1,4.1,4.1,2.1))
  })
  
  # Render empty leaflet map
  output$wbMap <- renderLeaflet({
    leaflet(
      options =
        leafletOptions(
          worldCopyJump = FALSE,
          crs           = leafletCRS(crsClass    = "L.Proj.CRS",
                                     code        = 'EPSG:2163',
                                     proj4def    = '+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs',
                                     resolutions = c(65536, 32768, 16384, 8192, 4096, 2048,1024, 512, 256, 128))
        )) %>%
      fitBounds(lng1 = bounds[1],
                lat1 = bounds[2],
                lng2 = bounds[3],
                lat2 = bounds[4]) %>%
      addScaleBar(position = 'bottomright') %>%
      addPolylines(data   = states,
                   weight = 1,
                   color  = 'gray100')
  })
  
  
}