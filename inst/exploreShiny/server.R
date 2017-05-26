
#->Server
#===============
server <- function(input, output){
  # TAB1 = Change checkbox inputs by category selection
  output$products_available <- renderUI({
    fileInfo.sub <- fileInfo %>%
      filter(dataCategory == input$category_select) %>%
      filter(timeStep     == input$tstep_select)
    checkboxGroupInput(inputId  = 'data_select',
                       label    = 'Select datasets to compare',
                       choices  = unique(fileInfo.sub$dataName),
                       selected = c('MOD16-A2', 'SSEBop'))
  })
  
  # TAB2 - Select components of water balance equation
  output$equation_box <- renderUI({
    fileInfo.sub     <- fileInfo %>%
                          filter(timeStep == input$eqn_timeStep) %>%
                          filter(HUC == input$eqn_HUC)
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
  
  # TAB2 - PUSH water balance components to equation!
  wbComps.push <- observeEvent(input$calculateEqn, {
    # Input objects
    fileInfo.sub     <- fileInfo %>%
                          filter(timeStep == input$eqn_timeStep)
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
                                   timeStep         = input$eqn_timeStep,
                                   HUC              = input$eqn_HUC,
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
      output$tab2text         <- renderText({
        'Calculation requires at least one INPUT and one OUTPUT'
      })
      output$tab2tableInputs  <- renderTable({})
      output$tab2tableOutputs <- renderTable({})
    }else {
      # Render text under 'Calculate Water Balance Button
      output$tab2text    <- renderText({
        paste0('Calculating for inputs (', paste(unlist(inputNames), collapse = ', '), ')',
               ' and outputs (', paste(unlist(outputNames), collapse = ', '), '), where residuals = Inputs - Outputs')
      })
      
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
      output$tab2tableInputs <- renderTable({
        data.frame(inputFiles = inputFiles,
                   startDates = inputMetadata$startDate,
                   endDates   = inputMetadata$endDate)
      })
      output$tab2tableOutputs <- renderTable({
        data.frame(outputFiles = outputFiles,
                   startDates  = outputMetadata$startDate,
                   endDates    = outputMetadata$endDate)
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
        output$tab2text <- renderText({
          'Error: Not all dates ranges of selected datasets overlap'
        })
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
        
        # Trim to overlappign date range
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
        inShape2              <- get(paste0('polyHUC', input$eqn_HUC))
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
        colorPal2 <- colorNumeric(palette = colVec,
                                 domain  = c(-mag, mag))
        # Add polygons to map
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
                       color  = 'gray100')
      }
      }
  })
  
  # TAB2 - Render empty leaflet map
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
  
  
  
  # TAB1 - PUSH the data selections via Button
  dnames.push <- eventReactive(input$go, {    #Update for selected data   BUTTON1
    input$data_select
  })
  maphuc.push <- eventReactive(input$go, {    #Update for selected HUC region   BUTTON1
    input$map_HUC_select
  })
  stat.push   <- eventReactive(input$go, {
    input$stat_select
  })
  season.push <- eventReactive(input$go, {
    input$season
  })
  
  # TAB1 - Change available statistics based on number of datasets chosen  (2 datasets:  usual stats, 3+ datasets:  sd, cv, etc)
  output$stats_available <- renderUI({
    if (length(input$data_select) == 1){
      selectInput(inputId = 'stat_select',
                  label   = 'Select Statistic:',
                  choices = list('Direct Values' = 'DV'))
    }else if (length(input$data_select) == 2){
      selectInput(inputId = 'stat_select',
                  label   = 'Select Statistic:',
                  choices = list('NSE' = 'NSE', 'RMSE' = 'RMSE', 'PBIAS' = 'PBIAS', 'DIFFERENCE' = 'DIF',
                                 'Cor - Kendall' = 'COR.K', 'Cor - Spearman' = 'COR.S', 'Peak Timing' = 'PEAK',
                                 'Ks test' = 'KS'))
    }else{
      selectInput(inputId = 'stat_select',
                  label   = 'Select Statistic:',
                  choices = list('Coefficient of Variation' = 'CV', 'Standard Deviation' = 'SD'))
    }
  })
  
  # TAB1 - Import Selected Data (reaction to Button click) - returns a list, each item is all selected data for all HUC regions (2,4,6,8,10)
  Data <- reactive({
    #Read all data files selected in dnames.push
    names.all      <- vector()   #Vector of names of data created
    for (i in 1:length(dnames.push())){   #import all files (in .feather format) in loop
      ##for (i in 1:length(dnames.push)){
      j               <- maphuc.push()
      #Create and assign names for current dataset
      name.cur        <- paste0(dnames.push()[i], '.', j)
      ##name.cur = paste0(dnames.push[i], '.', j)
      names.all       <- c(names.all, name.cur)   #record name of object to be created
      
      info.cur        <- fileInfo[fileInfo$HUC == j,]
      info.cur        <- info.cur[info.cur$dataName == dnames.push()[i],]
      ##info.cur = info.cur[info.cur$data.name == dnames.push[i],]
      
      #File name
      fname.cur       <- info.cur$fnames
      #Import file
      file.in         <- as.data.frame(feather::read_feather(path = file.path(path.feather, fname.cur)))
      #Define time-series for file (in as.Date format)
      ind             <- lubridate::decimal_date(date = seq.Date(from       = info.cur$startDate,
                                                                 by         = info.cur$timeStep,
                                                                 length.out = nrow(file.in)))
      file.out        <- zoo::as.zoo(file.in)
      index(file.out) <- ind
      assign(x     = name.cur,
             value = file.out)
    }
    
    #Put all data into a list, with names() being the data's name
    list.all       <- mget(names.all)
    #Collect indexes from each dataset (lapply index function) and find common values (reduce intersection)
    common.indices <- Reduce(f = intersect,
                             x = lapply(X   = list.all,
                                        FUN = index))
    trimToIndices  <- function(datlist){
      datlist        <- datlist[(index(datlist) %in% common.indices),]
      index(datlist) <- common.indices
      return(datlist)
    }
    Data           <- lapply(X   = list.all,
                             FUN = trimToIndices)
    Data
  })
  
  # TAB1 - Import selected HUC region (reaction to Button click) - returns a shapefile of selected HUC region
  inShape <- reactive({
    #Import shapefile
    inShape              <- get(paste0('polyHUC', maphuc.push()))
    size                 <- format(x     = object.size(inShape),
                                   units = 'auto')
    
    #Find column name with 'HUC' in it
    pos                  <- regexpr(pattern = "HUC",
                                    text    = colnames(inShape@data))
    #Rename column simply 'HUC'
    colnames(inShape@data)[which(pos == 1)] <- 'HUC'
    #return this shapefile
    inShape
  })
  
  # TAB1 - Isolate only data relevant to currently selected HUC region - Returns a list, each item is all selected data for selected HUC region
  subData <- reactive({
    #HUC selected (HUC of Interest)
    HOI         <- maphuc.push()
    #List all data objects created
    dnames      <- names(Data())
    #Find which include the HOI
    binary.locs <- grepl(pattern = as.character(HOI),
                         x       = raster::extension(dnames))
    #subset the data list by binary.locs (above)
    subData     <- Data()[binary.locs]
    subData
  })
  
  # TAB1 - Set bounds on time period x-axis slider
  output$time_available <- renderUI({
    times.dec <- unique(as.integer(index(subData()[[1]])))
    vrange    <- c(min(times.dec), max(times.dec))
    if (length(times.dec) > 10){vrange <- c((max(times.dec)-10), max(times.dec))}
    
    dateRangeInput(inputId = 'slider_time',
                   label   = 'Time Range for x-axis',
                   min     = as.Date(lubridate::date_decimal(min(times.dec))),
                   max     = as.Date(lubridate::date_decimal(max(times.dec))),
                   start   = as.Date(lubridate::date_decimal(vrange[1])),
                   end     = as.Date(lubridate::date_decimal(vrange[2]))
    )
  })
  
  # TAB1 - Apply selected statistic to subData
  statistic <- reactive({
    METsteps::tic()
    subData <- subData()  #1
    
    #Sort to ascending
    resortFUN <- function(x){
      x <- x[, order(as.integer(colnames(x)))]
    }
    subData <- lapply(X = subData,
                      FUN = resortFUN)
    
    
    print('subData:')
    print(shead(subData))
    #Split data into list: each list item is an individual HUC, with each column being a different dataset from subData
    splitnames <- vector()
    for (i in 1:length(subData)){     #Split each dataset into list:  each item is a HUC region
      objname.cur <- paste0('split', i)    #name for current temporary object
      splitnames  <- c(splitnames, objname.cur)    #Save name to vector (will use after loop)
      suppressWarnings(
        assign(x     = objname.cur,
               value = split(as.matrix(subData[[i]]), c(col(subData[[i]])))))
    }
    
    #Bind recursively via Map function
    cb <- get(splitnames[1])     #Base to cbind other data onto
    if (length(splitnames) > 1){   #As long as there are more than 1 datasets, loop the Map function to cbind remaining data
      for (i in 2:length(splitnames)){
        cb <- suppressWarnings(
          Map(cbind, cb, get(splitnames[i]))
        )
      }
    }
    
    #Assign index
    indd.cb <- index(subData[[1]])
    cb <- lapply(X = cb,
                 FUN = function(foo){
                   foo <- as.zoo(foo)
                   index(foo) <- indd.cb
                   return(foo)
                 })
    
    #Adjust for season
    if (season.push() != 'All'){
      seasonfun <- function(foo){
        return(foo[lubridate::month(zoo::yearmon(index(foo))) %in% get(season.push()), ])
      }
      cb <- lapply(cb, seasonfun)
    }
    #Apply statistical function as selected in UI
    STAT <- stat.push()
    
    if (length(cb) > 2000 || (length(cb) > 18 && NCOL(cb[[1]]) > 2)){
      #Initialize cluster
      if (STAT != 'DV'){
        if (exists('cl') == FALSE){
          no_cores <- parallel::detectCores()-1
          cl <- parallel::makeCluster(no_cores)
        }else{
          if (is.null(cl)){
            no_cores <- parallel::detectCores()-1
            cl <- parallel::makeCluster(no_cores)
          }
        }
      }else{
        cl <- NULL
      }
      # Single-dataset stats
      if (STAT == 'DV'){   statOUT <- as.numeric(unlist(lapply(X = cb, FUN = mean)))}
      # Two-dataset stats
      if (STAT == 'NSE'){  statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = NSEfun))); statOUT[statOUT<0] <- 0}
      if (STAT == 'RMSE'){ statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = RMSEfun)))}
      if (STAT == 'PBIAS'){statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = PBIASfun)))}
      if (STAT == 'COR.K'){statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = COR.Kfun)))}
      if (STAT == 'COR.S'){statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = COR.Sfun)))}
      if (STAT == 'DIF'){  statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = DIFfun)))}
      if (STAT == 'PEAK'){ statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = PEAKfun)))}
      if (STAT == 'KS'){   statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = KSfun)))}
      # Three or more-dataset stats
      if (STAT == 'CV'){   statOUT <- as.numeric(unlist(parLapply(cl=cl, X = cb, fun = CVfun)))}
      if (STAT == 'SD'){   statOUT <- as.numeric(unlist(parLapply(cl=cl, X = cb, fun = SDfun)))}
      # Replace Inf with NA
      statOUT[is.infinite(statOUT)] <- NA
      
      #Close cluster
      if (exists('cl')){
        if (is.null(cl) == F) parallel::stopCluster(cl)
        rm(cl)
      }
    }else{
      # Single-dataset stats
      if (STAT == 'DV'){   statOUT <- as.numeric(unlist(lapply(X = cb, FUN = mean)))}
      # Two-dataset stats
      if (STAT == 'NSE'){  statOUT <- as.numeric(unlist(lapply(X = cb, FUN = NSEfun))); statOUT[statOUT<0] <- 0}
      if (STAT == 'RMSE'){ statOUT <- as.numeric(unlist(lapply(X = cb, FUN = RMSEfun)))}
      if (STAT == 'PBIAS'){statOUT <- as.numeric(unlist(lapply(X = cb, FUN = PBIASfun)))}
      if (STAT == 'COR.K'){statOUT <- as.numeric(unlist(lapply(X = cb, FUN = COR.Kfun)))}
      if (STAT == 'COR.S'){statOUT <- as.numeric(unlist(lapply(X = cb, FUN = COR.Sfun)))}
      if (STAT == 'DIF'){  statOUT <- as.numeric(unlist(lapply(X = cb, FUN = DIFfun)))}
      if (STAT == 'PEAK'){ statOUT <- as.numeric(unlist(lapply(X = cb, FUN = PEAKfun)))}
      if (STAT == 'KS'){   statOUT <- as.numeric(unlist(lapply(X = cb, FUN = KSfun)))}
      # Three or more-dataset stats
      if (STAT == 'CV'){   statOUT <- as.numeric(unlist(lapply(X = cb, FUN = CVfun)))}
      if (STAT == 'SD'){   statOUT <- as.numeric(unlist(lapply(X = cb, FUN = SDfun)))}
      # Replace Inf with NA
      statOUT[is.infinite(statOUT)] <- NA
    }
    
    METsteps::toc(end = T)
    statOUT
  })
  
  # TAB1 - Colorpalette for polygon fill
  colorpal <- reactive({
    #magnitudes for color schema HUC.pb.mean.max
    mag <- max(c(max(statistic(),
                     na.rm = T),
                 abs(min(statistic(),
                         na.rm = T))))
    if (input$colors == 'Viridis') colVec <- viridis(3)
    if (input$colors == 'Magma')   colVec <- magma(3)
    if (input$colors == 'Inferno') colVec <- inferno(3)
    if (input$colors == 'Plasma')  colVec <- plasma(3)
    colorNumeric(palette = colVec,
                 domain  = c(-mag, mag))
  })
  
  
  # TAB1 - Blank Plots
  output$mymap <- renderLeaflet({
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
  
  # TAB1 - Render >> blank plots where they will be filled later by click
  multiplot.cex <- 1.8
  multiplot.lab <- 1.8
  output$text2 <- renderText({'                  '})
  output$plot1 <- renderPlot({
    layout(mat     = matrix(data  = c(1, 2, 3),
                            nrow  = 3,
                            ncol  = 1,
                            byrow = T),
           widths  = 1,
           heights = c(2.5, 1, 1))
    #-- ET direct data plot
    par(mar = c(0, 6.2, 4.1, 2.1))
    if (is.null(input$slider_time)){
      drange <- NULL
    }else{
      drange <- lubridate::decimal_date(as.Date(input$slider_time))
    }
    plot(x        = 1,
         type     = 'n',
         main     = '',
         xaxs     = "i",
         ylab     = 'AET',
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
         ylab     = 'Difference',
         xlab     = '',
         xlim     = drange,
         xaxt     = 'n',
         cex.axis = multiplot.cex,
         cex.lab  = multiplot.lab)
    #-- Percent difference plot
    par(mar = c(2, 6.2, 0, 2.1))
    plot(x        = 1,
         type     = 'n',
         main     = '',
         xaxs     = "i",
         ylab     = 'Percent\nDifference',
         xlab     = 'Time',
         xlim     = drange,
         cex.axis = multiplot.cex,
         cex.lab  = multiplot.lab)
    par(mar = c(5.1, 4.1, 4.1, 2.1))
  })
  output$plot2 <- renderPlot({
    par(mar = c(5.1, 4.1, 4.1, 1.4))
    if (is.null(input$slider_time)){
      drange <- NULL
    }else{
      drange <- lubridate::decimal_date(as.Date(input$slider_time))
    }
    plot(x    = 1,
         type = 'n',
         main = paste('Subset to HUC ##'),
         xaxs = "i",
         ylab = 'AET',
         xlab = 'Time',
         xlim = drange)
    par(mar = c(5.1, 4.1, 4.1, 2.1))
  })
  output$plot3 <- renderPlot({
    par(mfrow = c(1, 2))
    plot(x    = 1,
         type = 'n',
         ylim = c(0,100),
         xlim = c(1,12),
         xaxs = 'i',
         xaxt = 'n',
         ylab = 'AET',
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
  })
  
  
  # TAB1 - Observations
  # TAB1 - Observation >> fill (aka create) polygons
  observe({
    print('inShape():')
    print(head((inShape())$HUC))
    withProgress(message = 'Calculation in progress',
                 detail  = 'Please wait...',
                 style   = 'old',
                 value   = 0, {
                   pal <- colorpal()
                   leafletProxy(mapId = 'mymap',
                                data  = inShape()) %>%
                     clearShapes() %>%
                     addPolygons(data            = inShape(),
                                 weight          = 1,
                                 color           = ~pal(as.numeric(statistic())),
                                 fillColor       = ~pal(as.numeric(statistic())),
                                 fillOpacity     = 1,
                                 label           = ~stringr::str_c('HUC: ', HUC,',    ',
                                                                   'State(s): ', STATES, ',  ',
                                                                   'Statistic', ': ', round(statistic(),2)),
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
                                  color  = 'gray100')
                 })
    
    # %>%
    # addLegend('mymap', position = 'topleft', pal = pal,
    #           values = as.numeric(HUC.pb.mean()),
    #           opacity = 1,
    #           title = input$stat_select,
    #           #labFormat = labelFormat(suffix = '%'),
    #           layerId = 'legend_fill')
  })
  
  # TAB1 - Click polygon
  observe({
    click <- input$mymap_shape_click
    if (is.null(click)) return()
    
    #Huc Clicked Upon
    HCU <- click$id
    if (is.na(suppressWarnings(as.numeric(HCU))) == FALSE){
      #Create update polygon (last polygon clicked on has highlighted border)
      clickpoly <- inShape()[inShape()@data$HUC == HCU,]
      leafletProxy(mapId = 'mymap',
                   data  = inShape()) %>%
        addPolygons(data    = clickpoly,
                    weight  = 2,
                    color   = '#00ff00',
                    fill    = F,
                    opacity = 1,
                    layerId = 'clickhighlight')
      
      #Subset data to required HUC
      subToHUCfun <- function(foo){
        #find column for huc clicked on
        COI <- which(as.numeric(gsub(pattern     = 'X',
                                     replacement = '',
                                     x           = colnames(foo))) == as.numeric(HCU))
        if (length(COI) > 0){   #This if statement protects the program from crashes when non-polygons are clicked on
          isod        <- foo[,COI]
          index(isod) <- index(foo)
          return(isod)
        }
      }
      
      if (as.numeric(HCU) %in% as.numeric(colnames(subData()[[1]]))){
        subToHUC        <- lapply(X   = subData(),
                                  FUN = subToHUCfun)
        
        presIndex       <- index(subToHUC[[1]])   #Record index to reapply after unlisting
        subToHUC        <- zoo::as.zoo(matrix(data = unlist(subToHUC),
                                              nrow = length(subToHUC[[1]])))
        subToHUC[is.infinite(subToHUC)] <- NA
        index(subToHUC) <- presIndex
        
        
        #Generate ET plot
        #cbPalette <- c("#56B4E9", "#F0E442", "#CC79A7", "#0072B2", "#D55E00")
        cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
        abl.col <- 'darkgrey'
        output$plot1 <- renderPlot({
          layout(mat     = matrix(data  = c(1,2,3),
                                  nrow  = 3,
                                  ncol  = 1,
                                  byrow = T),
                 widths  = 1,
                 heights = c(2.5, 1, 1))
          if (is.null(input$slider_time)){
            drange <- NULL
          } else{
            drange <- lubridate::decimal_date(as.Date(input$slider_time))
          }
          #-- ET direct data plot
          par(mar = c(0, 6.2, 4.1, 2.1))
          plot(x        = subToHUC[,1],
               col      = cbPalette[1],
               main     = paste('HUC',
                                as.character(HCU)),
               xaxs     = "i",
               ylab     = 'AET',
               xlim     = drange,
               #ylim     = c(0, max(subToHUC, na.rm = T)),
               # MCH!!
               ylim     = c(min(subToHUC, na.rm = T), max(subToHUC, na.rm = T)),
               xlab     = '',
               xaxt     = 'n',
               cex.axis = multiplot.cex,
               cex.lab  = multiplot.lab,
               cex.main = 1.8,
               lwd      = 2,
               yaxs     = 'i')
          if (ncol(subToHUC) > 1){
            for (i in 2:ncol(subToHUC)){
              lines(x    = subToHUC[,i],
                    col  = cbPalette[i],
                    xaxs = "i",
                    lwd  = 2)
            }
          }
          abline(v   = unique(as.integer(index(subToHUC))),
                 col = abl.col,
                 lty = 2)
          legend(x      = 'bottomright',
                 legend = input$data_select,
                 pch    = 15,
                 col    = cbPalette[1:ncol(subToHUC)],
                 pt.cex = 2,
                 cex    = 1.5)
          #-- Difference plot
          par(mar = c(0, 6.2, 0, 2.1))
          col.dif = "firebrick"
          abl.col = 'darkgrey'
          if (ncol(subToHUC) == 1){
            plot(x        = 1,
                 type     = 'n',
                 main     = '',
                 xaxs     = 'i',
                 ylab     = 'Difference',
                 xlab     = '',
                 xlim     = drange,
                 xaxt     = 'n',
                 cex.axis = multiplot.cex,
                 cex.lab  = multiplot.lab)
            abline(v   = unique(as.integer(index(subToHUC))),
                   col = abl.col,
                   lty = 2)
            abline(h   = 0,
                   col = abl.col,
                   lty = 1)}
          if (ncol(subToHUC) == 2){
            plot(x        = subToHUC[,2] - subToHUC[,1],
                 col      = col.dif,
                 main     = '',
                 xaxs     = "i",
                 ylab     = 'Difference',
                 xlab     = '',
                 xlim     = drange,
                 xaxt     = 'n',
                 cex.axis = multiplot.cex,
                 cex.lab  = multiplot.lab,
                 lwd      = 2)
            abline(v   = unique(as.integer(index(subToHUC))),
                   col = abl.col,
                   lty = 2)
            abline(h   = 0,
                   col = abl.col,
                   lty = 1)
          }
          if (ncol(subToHUC) > 2){
            combinations  <- t(combn(x = ncol(subToHUC),
                                     m = 2))
            combFUNdif <- function(combrow){
              subToHUC[, combrow[2]] - subToHUC[, combrow[1]]
            }
            difmat        <- as.zoo(apply(X      = combinations,
                                          MARGIN = 1,
                                          FUN    = combFUNdif))
            index(difmat) <- index(subToHUC)
            
            plot(x        = difmat[,1],
                 col      = col.dif,
                 main     = '',
                 xaxs     = "i",
                 ylab     = 'Difference',
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
                    col  = col.dif,
                    lwd  = 2,
                    lty  = i)
            }
            abline(v   = unique(as.integer(index(subToHUC))),
                   col = abl.col,
                   lty = 2)
            abline(h   = 0,
                   col = abl.col,
                   lty = 1)
          }
          #-- Percent difference plot
          # par(mar=c(2,6.2,0,2.1))
          # if (ncol(subToHUC) == 1){plot(1, type = 'n', main = '',xaxs="i", ylab = 'Percent\nDifference', xlab = 'Time', xlim = input$slider_time,
          #                               cex.axis = multiplot.cex, cex.lab = multiplot.lab, lwd = 2)}
          # if (ncol(subToHUC) == 2){
          #   plot(((subToHUC[,2]-subToHUC[,1])/subToHUC[,1])*100, col = col.dif, main = '',
          #        xaxs="i", ylab = 'Percent\nDifference', xlab = 'Time', xlim = input$slider_time,
          #        cex.axis = multiplot.cex, cex.lab = multiplot.lab, lwd = 2)
          # }
          # if (ncol(subToHUC) > 2){
          #   combFUNperc <- function(combrow){return(((subToHUC[,combrow[2]] - subToHUC[,combrow[1]])/subToHUC[,combrow[1]])*100)}
          #   percmat = apply(combinations, 1, combFUNperc)
          #   percmat[is.infinite(percmat)] = NA
          #   percmat[percmat == 'Inf'] = NA
          #   percmat[percmat == '-Inf'] = NA
          #   percmat = as.zoo(percmat)
          #   index(percmat) = index(subToHUC)
          # 
          #   
          #   print(c(min(percmat, na.rm = T), max(percmat, na.rm = T)))
          #   plot(percmat[,1], col = col.dif, main = '',
          #        xaxs="i", ylab = 'Percent\nDifference', xlab = 'Time', xlim = input$slider_time,
          #        cex.axis = multiplot.cex, cex.lab = multiplot.lab, lwd = 2, ylim = c(min(percmat, na.rm = T), max(percmat, na.rm = T)))
          #   for (i in 2:ncol(percmat)){
          #     lines(percmat[,i], xaxs = 'i', col = col.dif, lwd = 2, lty = i)
          #   }
          # }
          # abline(v=unique(as.integer(index(subToHUC))), col = abl.col, lty = 2)
          # abline(h=0, col = abl.col, lty = 1)
          par(mar = c(5.1, 4.1, 4.1, 2.1))
        })
        
        #Generate ET plot for specified HUC level for clicked HUC
        ####!!!!!! Import subHUC levels
        subHUC10 <- vector(mode   = 'list',
                           length = length(dnames.push()))
        for (i in 1:length(dnames.push())){
          #Subset info
          fnames.sub         <- fileInfo %>%
            dplyr::filter(dataName == dnames.push()[i]) %>%
            dplyr::filter(HUC      == as.numeric(input$HUC_select))
          path.f             <- file.path(path.feather,
                                          fnames.sub$fnames)
          #Import only relevant columns
          cNames             <- names((feather::feather_metadata(path = path.f))$type)
          cNames.sub         <- which(as.numeric(substr(x     = cNames,
                                                        start = 1,
                                                        stop  = nchar(HCU))) == as.numeric(HCU))
          #Temp file
          tempHUC10          <- as.data.frame(feather::read_feather(path    = path.f,
                                                                    columns = cNames.sub))
          #Define time-series for file (in as.Date format)
          ind                <- lubridate::decimal_date(seq.Date(from       = fnames.sub$startDate,
                                                                 by         = fnames.sub$timeStep,
                                                                 length.out = nrow(tempHUC10)))
          tempHUC10          <- zoo::as.zoo(tempHUC10)
          index(tempHUC10)   <- ind
          #Save to list
          subHUC10[[i]]      <- tempHUC10
          names(subHUC10)[i] <- fnames.sub$data.name
        }
        
        output$plot2 <- renderPlot({
          par(mar = c(5.1, 4.1, 4.1, 1.4))
          if (is.null(input$slider_time)){
            drange <- NULL
          } else{
            drange <- lubridate::decimal_date(as.Date(input$slider_time))
          }
          #subset reactive Data by matching the extensions of the list names to the selected HUC level
          ###data.lineHUCS = Data()[(gsub('.', '', extension(names(Data())), fixed = T) == as.character(input$HUC_select))]
          data.lineHUCS <- subHUC10
          ####data.lineHUCS = Data[(gsub('.', '', extension(names(Data)), fixed = T) == as.character(input$HUC_select))]
          ###cols2keep = which(as.numeric(substr(colnames(data.lineHUCS[[1]]), start = 1, stop = nchar(HCU))) == as.numeric(HCU))
          cols2keep <- 1:ncol(data.lineHUCS[[1]])
          
          #If more than 100 lines to plot, randomly sample down to 100
          if (length(cols2keep) > 50){
            if (ncol(subToHUC) <= 2){
              cols2keep <- sample(x    = cols2keep,
                                  size = 50)
              samptext  <- 'Data resampled to n=50'
            }else{
              cols2keep <- sample(x    = cols2keep,
                                  size = 25)
              samptext <- 'Data resampled to n=25'
            }
          }else{
            samptext <- ''
          }
          #collect max values from list
          ylmax <- max(unlist(lapply(X = data.lineHUCS,
                                     FUN = max,
                                     na.rm = T)))
          
          progmax <- (length(data.lineHUCS)-1)*length(cols2keep)
          withProgress(message = 'Plotting in progress',
                       detail  = 'Please wait...',
                       value   = 0, {
                         #Get max for ylim
                         ymaxx <- max(unlist(lapply(X   = data.lineHUCS,
                                                    FUN = function(x) {
                                                      return(max(x[,cols2keep], na.rm = T))
                                                    })))
                         plot(x    = (data.lineHUCS[[1]])[,cols2keep[1]],
                              type = 'l',
                              col  = scales::alpha(colour = cbPalette[1],
                                                   alpha  = input$alpha_slider),
                              lwd  = 1,
                              lty  = 1,
                              main = paste('HUC subsetted to HUC',
                                           as.character(input$HUC_select)),
                              xaxs = "i",
                              yaxs = "i",
                              ylab = 'AET',
                              xlab = 'Time',
                              xlim = drange,
                              ylim = c(0, ymaxx)) #input$ylimmax_slider
                         abline(v   = unique(as.integer(index(subToHUC))),
                                col = abl.col,
                                lty = 2)
                         if (length(cols2keep) > 1){
                           for (j in 2:length(cols2keep)){
                             lines(x   = (data.lineHUCS[[1]])[,cols2keep[j]],
                                   col = scales::alpha(colour = cbPalette[1],
                                                       alpha  = input$alpha_slider),
                                   lwd = 2)
                             incProgress(1/progmax)
                           }
                         }
                         if (length(data.lineHUCS) > 1){
                           for (i in 2:length(data.lineHUCS)){
                             for (j in 1:length(cols2keep)){
                               lines(x   = (data.lineHUCS[[i]])[,cols2keep[j]],
                                     col = scales::alpha(colour = cbPalette[i],
                                                         alpha  = input$alpha_slider),
                                     lwd = 2)
                               incProgress(1/progmax)
                             }
                           }
                           legend(x = 'topleft',
                                  legend = samptext,
                                  bty = 'n')
                         }
                       })
          #start.t = proc.time()
          # if (length(data.lineHUCS) > 1){
          #   plot((data.lineHUCS[[1]])[,cols2keep[1]], type = 'l', col = alpha(cbPalette[1], input$alpha_slider), lwd = 1, lty = 1,
          #        main = paste('HUC subsetted to HUC', as.character(input$HUC_select)), xaxs="i", yaxs="i",
          #         ylab = 'AET', xlab = 'Time', xlim = input$slider_time, ylim = c(0, input$ylimmax_slider))
          #   if (length(cols2keep) > 1){
          #     for (j in 2:length(cols2keep)){lines((data.lineHUCS[[1]])[,cols2keep[j]], col = alpha(cbPalette[1], input$alpha_slider), lwd = 2)}
          #   }
          #   for (i in 2:length(data.lineHUCS)){
          #     for (j in 1:length(cols2keep)){
          #       lines((data.lineHUCS[[i]])[,cols2keep[j]], col = alpha(cbPalette[i], input$alpha_slider), lwd = 2)
          #     }
          #   }
          #   legend('topleft', samptext, bty = 'n')
          # }
          #end.t = proc.time()
        })
        
        #Generate PErcentile and ECDF plots
        output$plot3 <- renderPlot({
          #par(mfrow = c(1,2))
          layout(mat     = matrix(data  = c(1,2,3,3),
                                  nrow  = 2,
                                  byrow = T),
                 heights = c(0.85, 0.15))
          par(mar = c(1, 2, 1.2, 2))
          #Monthly quantile envelope plots
          monthlylist.y <- vector(mode   = "list",
                                  length = ncol(subToHUC))
          for (i in 1:ncol(subToHUC)){
            list.month <- vector(mode = 'list',
                                 length = 12)
            data.vec   <- subToHUC[,i]
            months.all <- lubridate::month((lubridate::date_decimal(index(data.vec)+0.00001)))
            
            for (j in 1:12){
              curmon          <- data.vec[months.all == j]
              list.month[[j]] <- curmon
            }
            quants.25        <- unlist(lapply(X     = list.month,
                                              FUN   = quantile,
                                              probs = 0.25,
                                              na.rm = T))
            quants.75         <- unlist(lapply(X     = list.month,
                                               FUN   = quantile,
                                               probs = 0.75,
                                               na.rm = T))
            quants            <- rbind(quants.25, quants.75)
            meds              <- unlist(lapply(X     = list.month,
                                               FUN   = median,
                                               na.rm = T))
            #Remove list
            if (exists('list.month')) rm(list.month)
            #reorder to start with oct
            quants             <- quants[, c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9)]
            meds               <- meds[c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9)]
            xvals              <- c(1:12, 12, rev(1:12), 1)
            
            monthlylist.y[[i]] <- c(quants[1,], quants[2,12], rev(quants[2,]), quants[2,1])
          }
          
          ymaxx <- max(unlist(lapply(X   = monthlylist.y,
                                     FUN = max)))
          plot(x    = 1,
               type = 'n',
               ylim = c(0, ymaxx),
               xlim = c(1, 12),
               xaxs = 'i',
               xaxt = 'n',
               ylab = 'AET',
               xlab = '',
               main = '25-75th Percentile Monthly Ranges')
          abline(v   = 2:11,
                 col = abl.col,
                 lty = 2)
          axis(side   = 1,
               at     = 1:12,
               labels = c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep'))
          for (i in 1:ncol(subToHUC)){
            polygon(x      = xvals,
                    y      = monthlylist.y[[i]],
                    col    = scales::alpha(colour = cbPalette[i],
                                           alpha  = 0.5),
                    border = NA)
            lines(x   = meds,
                  lty = 2)
            points(x   = meds,
                   pch = 16)
          }
          #ecdf plot
          plot(x    = ecdf(as.numeric(subToHUC[,1])),
               col  = cbPalette[1],
               main = 'Empirical Cumulative Distribution Function')
          if (ncol(subToHUC) > 1){
            for (i in 2:ncol(subToHUC)){
              plot(x   = ecdf(as.numeric(subToHUC[,i])),
                   col = cbPalette[i],
                   add = T)
            }
          }
          #Add common legend
          plot(x    = 1,
               type = 'n',
               axes = FALSE,
               xlab = '',
               ylab = '')
          legend(x      = 'center',
                 legend = input$data_select,
                 inset  = 0,
                 horiz  = TRUE,
                 pch    = 15,
                 col    = cbPalette[1:ncol(subToHUC)],
                 pt.cex = 2,
                 cex    = 1)
        })
      }
      
      
      # 
      # #Generate ET plot for specified HUC level for clicked HUC
      # output$plot2 <- renderPlot({
      #   #Identify sub HUC regions
      #   #subHUC.d1 = get( paste0('HUC.d1.', input$HUC_select) )
      #   #subHUC.d2 = get( paste0('HUC.d2.', input$HUC_select) )
      #   subHUC.d1 = get( paste0('HUC.d1.', input$HUC_select) )
      #   subHUC.d2 = get( paste0('HUC.d2.', input$HUC_select) )
      #   #Subset subHUC regions by HUC clicked upon (HCU)
      #   subnames1 = colnames(subHUC.d1); subnames1 = (gsub("X", "", subnames1))
      #   subnames2 = colnames(subHUC.d2); subnames2 = (gsub("X", "", subnames2))
      #   subHUC.d1.new = subHUC.d1[,which(as.numeric(substr((gsub('^.', '', colnames(subHUC.d1))), 1, input$map_HUC_select)) == as.numeric(HCU))]
      #   subHUC.d2.new = subHUC.d2[,which(as.numeric(substr((gsub('^.', '', colnames(subHUC.d2))), 1, input$map_HUC_select)) == as.numeric(HCU))]
      # 
      #   col.one = "#56B4E9"
      #   col.two = "#E69F00"
      # 
      #   #Plot dataset 1
      #   plot(subHUC.d1.new[,1], type = 'l', col = alpha(col.one, input$slider_alpha), lwd = input$slider_lwd, lty = 1,
      #        main = paste('HUC subsetted to HUC', as.character(input$HUC_select)), xaxs="i",
      #        ylab = 'AET', xlab = 'Time', ylim = c(0, max(subHUC.d1.new, subHUC.d2.new, na.rm = T)), xlim = input$slider_time)
      #   if (!(is.null(dim(subHUC.d1.new)))){
      #     for (KK in 2:ncol(subHUC.d1.new)){lines(subHUC.d1.new[,KK], col = alpha(col.one, input$slider_alpha), lwd = input$slider_lwd)}
      #   }
      #   #Plot dataset 2
      #   if (!(is.null(dim(subHUC.d2.new)))){
      #     for (KK in 1:ncol(subHUC.d2.new)){lines(subHUC.d2.new[,KK], col = alpha(col.two, input$slider_alpha), lwd = input$slider_lwd)}
      #   } else {lines(subHUC.d2.new, col = alpha(col.two, input$slider_alpha), lwd = input$slider_lwd)}
      #   legend('bottomright', data.names, pch = 15, col = c("#56B4E9", "#E69F00"), pt.cex = 2, cex = 1.5)
      # })
    }
  })
  
}

