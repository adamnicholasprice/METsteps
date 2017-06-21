
#->Server
#===============
server <- function(input, output){
  ##### SidePanel reactive inputs
  # List available datasets
  output$products_available <- renderUI({
    fileInfo.sub <- fileInfo %>%
      filter(dataCategory == input$category_select) %>%
      filter(timeStep     == input$tstep_select)
    checkboxGroupInput(inputId  = 'data_select',
                       label    = 'Select datasets to compare',
                       choices  = unique(fileInfo.sub$dataName),
                       selected = c('MOD16-A2', 'SSEBop'))
  })
  # Change available statistics based on number of datasets chosen  (2 datasets:  usual stats, 3+ datasets:  sd, cv, etc)
  output$stats_available <- renderUI({
    if (length(input$data_select) == 1){
      selectInput(inputId = 'stat_select',
                  label   = 'Select Statistic:',
                  choices = list('Direct Values' = 'DV'))
    }else if (length(input$data_select) == 2){
      selectInput(inputId = 'stat_select',
                  label   = 'Select Statistic:',
                  choices = list('Difference' = 'DIF', 'NSE' = 'NSE', 'RMSE' = 'RMSE', 'PBIAS' = 'PBIAS',
                                 'Cor - Kendall' = 'COR.K', 'Cor - Spearman' = 'COR.S', 'Peak Timing' = 'PEAK',
                                 'Ks test' = 'KS'))
    }else{
      selectInput(inputId = 'stat_select',
                  label   = 'Select Statistic:',
                  choices = list('Coefficient of Variation' = 'CV', 'Standard Deviation' = 'SD'))
    }
  })
  
  ##### Generate Default plots on Main Panel
  # Render empty leaflet map
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
  # Render blank plots lineplots
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
         ylab     = 'units',
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
         ylab = 'units',
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
  })
  # Note that no plots have been created yet
  plotsCreated <- F
  output$plotsCreated <- reactive({
    FALSE
  })
  outputOptions(output, "plotsCreated", suspendWhenHidden = FALSE) 
  
  
  ##### Reactive statistical processing and plotting after clicking update button
  deComps.push <- observeEvent(input$go, {
    #-------- Reset default plots if previously generated ones exist
    if (plotsCreated){
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
      proxy <- leafletProxy("mymap")
      proxy %>% clearControls()
      # Render blank plots lineplots
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
             ylab     = 'units',
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
             ylab = 'units',
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
      })
    }
    plotsCreated <<- T
    output$plotsCreated <- reactive({
      TRUE
    })
    
    #-------- Define user-selected inputs as independent variables
    # Names of datasets
    dnames    <<- input$data_select
    # Selected HUC region
    maphuc    <<- input$map_HUC_select
    # Selected timestep
    timeStep  <<- input$tstep_select
    # Selected statistic
    stat      <<- input$stat_select
    # Selected color scheme
    colScheme <<- input$colors
    
    #-------- Load and format datasets
    # Load all relevant files in loop
    names.all <- vector()  #Vector of names of data created
    for (i in 1:length(dnames)){   #import all files (in .feather format) in loop
      #Create and assign names for current dataset
      name.cur        <- paste0(dnames[i], '.', maphuc)
      #Record name of object to be created
      names.all       <- c(names.all, name.cur)   
      #Subset metadata to current file
      info.cur        <- fileInfo[fileInfo$HUC == maphuc,]
      info.cur        <- info.cur[info.cur$dataName == dnames[i],]
      info.cur        <- info.cur[info.cur$timeStep == timeStep,]
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
    
    if (length(list.all) > 1){
      #Collect indexes from each dataset (lapply index function) and find common values (reduce intersection)
      common.indices <- Reduce(f = intersect,
                               x = lapply(X   = list.all,
                                          FUN = index))
      #!!!!!!!! ADD FILTER HERE IS DATASETS DO NOT OVERLAP !!!!!!!!!
      #Trim to indices function
      trimToIndices  <- function(datlist){
        datlist        <- datlist[(index(datlist) %in% common.indices),]
        index(datlist) <- common.indices
        return(datlist)
      }
      #Lapply function to trim datasets to common indices
      Data           <<- lapply(X   = list.all,
                                FUN = trimToIndices)
    }else{
      Data <<- list.all
    }
    
    #-------- Load selected HUC region as shapefile
    #Import shapefile
    inShape              <<- get(paste0('polyHUC', maphuc))
    size                 <- format(x     = object.size(inShape),
                                   units = 'auto')
    #Find column name with 'HUC' in it
    pos                  <- regexpr(pattern = "HUC",
                                    text    = colnames(inShape@data))
    #Rename column simply 'HUC'
    colnames(inShape@data)[which(pos == 1)] <- 'HUC'
    
    #-------- Isolate data for selected HUC region (deprecated but included for continuity of code)
    #Find which include the HOI
    binary.locs <- grepl(pattern = as.character(maphuc),
                         x       = raster::extension(names.all))
    #Subset the data list by binary.locs (above)
    subData     <<- Data[binary.locs]
    
    #Set bounds on x-axis time period control
    output$time_available <- renderUI({
      times.dec <- unique(as.integer(index(subData[[1]])))
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
    
    #-------- Apply selected statistic to subData
    #Make sure HUCs are in ascending order
    resortFUN <- function(x){
      x <- x[, order(as.integer(colnames(x)))]
    }
    subData <- lapply(X = subData,
                      FUN = resortFUN)
    #Split data into list:
    # each list item is an individual HUC, with each column being a different dataset from subData
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
    #Apply statistical function as selected in UI
    if (length(cb) > 2000 || (length(cb) > 18 && NCOL(cb[[1]]) > 2)){
      #Initialize cluster
      if (stat != 'DV'){
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
      if (stat == 'DV'){   statOUT <- as.numeric(unlist(lapply(X = cb, FUN = mean)))}
      # Two-dataset stats
      if (stat == 'NSE'){  statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = NSEfun))); statOUT[statOUT<0] <- 0}
      if (stat == 'RMSE'){ statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = RMSEfun)))}
      if (stat == 'PBIAS'){statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = PBIASfun)))}
      if (stat == 'COR.K'){statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = COR.Kfun)))}
      if (stat == 'COR.S'){statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = COR.Sfun)))}
      if (stat == 'DIF'){  statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = DIFfun)))}
      if (stat == 'PEAK'){ statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = PEAKfun)))}
      if (stat == 'KS'){   statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = KSfun)))}
      # Three or more-dataset stats
      if (stat == 'CV'){   statOUT <- as.numeric(unlist(parLapply(cl=cl, X = cb, fun = CVfun)))}
      if (stat == 'SD'){   statOUT <- as.numeric(unlist(parLapply(cl=cl, X = cb, fun = SDfun)))}
      # Replace Inf with NA
      statOUT[is.infinite(statOUT)] <- NA
      
      #Close cluster
      if (exists('cl')){
        if (is.null(cl) == F) parallel::stopCluster(cl)
        rm(cl)
      }
    }else{
      # Single-dataset stats
      if (stat == 'DV'){   statOUT <- as.numeric(unlist(lapply(X = cb, FUN = mean)))}
      # Two-dataset stats
      if (stat == 'NSE'){  statOUT <- as.numeric(unlist(lapply(X = cb, FUN = NSEfun))); statOUT[statOUT<0] <- 0}
      if (stat == 'RMSE'){ statOUT <- as.numeric(unlist(lapply(X = cb, FUN = RMSEfun)))}
      if (stat == 'PBIAS'){statOUT <- as.numeric(unlist(lapply(X = cb, FUN = PBIASfun)))}
      if (stat == 'COR.K'){statOUT <- as.numeric(unlist(lapply(X = cb, FUN = COR.Kfun)))}
      if (stat == 'COR.S'){statOUT <- as.numeric(unlist(lapply(X = cb, FUN = COR.Sfun)))}
      if (stat == 'DIF'){  statOUT <- as.numeric(unlist(lapply(X = cb, FUN = DIFfun)))}
      if (stat == 'PEAK'){ statOUT <- as.numeric(unlist(lapply(X = cb, FUN = PEAKfun)))}
      if (stat == 'KS'){   statOUT <- as.numeric(unlist(lapply(X = cb, FUN = KSfun)))}
      # Three or more-dataset stats
      if (stat == 'CV'){   statOUT <- as.numeric(unlist(lapply(X = cb, FUN = CVfun)))}
      if (stat == 'SD'){   statOUT <- as.numeric(unlist(lapply(X = cb, FUN = SDfun)))}
      # Replace Inf with NA
      statOUT[is.infinite(statOUT)] <- NA
    }
    
    #-------- Create color palette for filling polygons
    #magnitude of largest absolute value
    mag <- max(abs(statOUT), na.rm = T)
    
    if (input$colorCheckBox == T){
      #Check to make sure user inputs are valid colors
      areColors <- function(x) {
        sapply(x, function(X) {
          tryCatch(is.matrix(col2rgb(X)), 
                   error = function(e) FALSE)
        })
      }
      if (length(input$midcol) == 1){
        colVec <- c(input$minCol, input$midCol, input$maxCol)
      }else{
        colVec <- c(input$minCol, input$maxCol)
      }
      
      if (sum(areColors(colVec)) != length(colVec)){
        output$colorError  <- renderText({paste0('Manually selected color(s) ',
                                                 paste(colVec[which(areColors(colVec) == FALSE)], collapse = ', '),
                                                 ' are not recognized by R'
        )
        })
        output$plotsCreated <- reactive({
          FALSE
        })
        return()
      }
    }else{
      if (colScheme == 'Viridis') colVec <- viridis(3)
      if (colScheme == 'Magma')   colVec <- magma(3)
      if (colScheme == 'Inferno') colVec <- inferno(3)
      if (colScheme == 'Plasma')  colVec <- plasma(3)
    }
    colorpal <- leaflet::colorNumeric(palette = colVec,
                                      domain  = c(-mag, mag))
    
    #-------- Add polygons to map and fill with colorpal function
    withProgress(message = 'Calculation in progress',
                 detail  = 'Please wait...',
                 style   = 'old',
                 value   = 0, {
                   pal <- colorpal
                   leafletProxy(mapId = 'mymap',
                                data  = inShape) %>%
                     clearShapes() %>%
                     addPolygons(data            = inShape,
                                 weight          = 1,
                                 color           = ~pal(as.numeric(statOUT)),
                                 fillColor       = ~pal(as.numeric(statOUT)),
                                 fillOpacity     = 1,
                                 label           = ~stringr::str_c('HUC: ', HUC,',    ',
                                                                   'State(s): ', STATES, ',  ',
                                                                   'Statistic', ': ', round(statOUT,2)),
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
                               pal = pal,
                               values = as.numeric(statOUT),
                               title = stat,
                               opacity = 1)
                 })
  })
  
  ##### Reactive Create New Map
  newMap <- observeEvent(input$goNewMap, {
    output$plotsCreated <- reactive({
      FALSE
    })
  })
  
  
  
  ##### Response to clicking (selecting) a polygon interactively
  observe({
    click <- input$mymap_shape_click
    
    #If click value is 'NULL' (when clicked between polygons) dont return any plots. Otherwise, continue.
    if (is.null(click) == FALSE){
      #Get data category
      dataCategory <- unique((fileInfo[fileInfo$dataName %in% dnames,])$dataCategory)
      #HUC Clicked Upon
      HCU <- click$id
      if (is.na(suppressWarnings(as.numeric(HCU))) == FALSE){
        #Create update polygon (last polygon clicked on has highlighted border)
        clickpoly <- inShape[inShape@data$HUC == HCU,]
        leafletProxy(mapId = 'mymap',
                     data  = inShape) %>%
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
        
        if (as.numeric(HCU) %in% as.numeric(colnames(subData[[1]]))){
          subToHUC        <- lapply(X   = subData,
                                    FUN = subToHUCfun)
          
          presIndex       <- index(subToHUC[[1]])   #Record index to reapply after unlisting
          subToHUC        <- zoo::as.zoo(matrix(data = unlist(subToHUC),
                                                nrow = length(subToHUC[[1]])))
          subToHUC[is.infinite(subToHUC)] <- NA
          index(subToHUC) <- presIndex
          colnames(subToHUC) <- dnames
          
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
                 ylab     = dataCategory,
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
                   legend = dnames,
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
                   ylab     = 'Difference (mm)',
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
                   ylab     = 'Difference (mm)',
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
                   ylab     = 'Difference (mm)',
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
            par(mar = c(5.1, 4.1, 4.1, 2.1))
          })
          
          #Generate ET plot for specified HUC level for clicked HUC
          ####!!!!!! Import subHUC levels
          subHUC10 <- vector(mode   = 'list',
                             length = length(dnames))
          for (i in 1:length(dnames)){
            #Subset info
                # fnames.sub         <- fileInfo %>%
                #   dplyr::filter(dataName == dnames[i]) %>%
                #   dplyr::filter(HUC      == as.numeric(input$HUC_select)) %>%
                #   dplyr::filter(timeStep == timeStep) %>%
                #   distinct(fnames)
                # yfun <- function(x){
                #   fileInfo$fnames[(fileInfo$dataName == dnames[i] & fileInfo$HUC == 8 & fileInfo$timeStep == timeStep)]}
                # 
            fnames.sub <- fileInfo[(fileInfo$dataName == dnames[i] & fileInfo$HUC == as.numeric(input$HUC_select) & fileInfo$timeStep == timeStep),]
            
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
            data.lineHUCS <- subHUC10
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
                                             as.character(maphuc)),
                                xaxs = "i",
                                yaxs = "i",
                                ylab = dataCategory,
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
                 ylab = dataCategory,
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
                   legend = dnames,
                   inset  = 0,
                   horiz  = TRUE,
                   pch    = 15,
                   col    = cbPalette[1:ncol(subToHUC)],
                   pt.cex = 2,
                   cex    = 1)
          })
          
          #Generate Taylor plots
          # Create checkbox
          output$datasets_for_Taylor <- renderUI({
            radioButtons(inputId = 'taylor_Observational',
                         label = 'Define observational dataset',
                         choices = dnames,
                         inline = T)
          })
          observe({
            if (!is.null(dim(subToHUC))){
              obsCol <<- which(colnames(subToHUC) == input$taylor_Observational)
              simCol <<- (1:ncol(subToHUC))[-obsCol]
              inOrder <<- c(obsCol, simCol)
              allData <<- as.matrix(subToHUC)
              allData <<- allData[,inOrder]
              # define colors
              ttColors <<- cbPalette[1:ncol(subToHUC)]
              ttColors <<- ttColors[inOrder]
              
              output$taylorPlotclick <- renderPlot({
                METsteps::taylor(allData   = allData,
                                 dataNames = colnames(allData),
                                 dataColors = ttColors)
              })
            }
          })
          
        }
      }
    }
  })
}

