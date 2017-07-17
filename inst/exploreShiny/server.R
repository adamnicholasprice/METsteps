
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
  # List available statistics
  output$stats_available <- renderUI({
    if (length(input$data_select) == 1){
      selectInput(inputId = 'stat_select',
                  label   = 'Select Statistic:',
                  choices = METsteps::shinyFunExtract(numD = 'one'))
    }else if (length(input$data_select) == 2){
      selectInput(inputId = 'stat_select',
                  label   = 'Select Statistic:',
                  choices = METsteps::shinyFunExtract(numD = 'two'))
    }else{
      selectInput(inputId = 'stat_select',
                  label   = 'Select Statistic:',
                  choices = METsteps::shinyFunExtract(numD = 'poly'))
    }
  })
  # Change available seasonal/monthly subsetting based on subset_Option
  output$subsetoutput <- renderUI({
    if (input$subset_Option == TRUE){
      selectInput(inputId  = 'seasMon_select',
                  label    = 'Select Season',
                  choices  = c('None', 'Fall', 'Winter', 'Spring', 'Summer'),
                  selected = 'None')
    }else if (input$subset_Option == FALSE){
      selectInput(inputId  = 'seasMon_select',
                  label    = 'Select Month',
                  choices  = c('None', base::month.abb),
                  selected = 'None')
    }else{}
  })
  #  default highlight individual HUC - just so no movement when loading maps
  output$light_SingleHUC <- renderUI({
    selectizeInput(inputId = 'lightHUC', 
                   label = 'Highlight Specific HUC',
                   choices = c(NA, NA, NA))
  })
  # default x-axis choices - just so no movement when loading maps
  # default plots
  output$time_available <- renderUI({
    if (exists('subData')){
      times.dec <- unique(as.integer(index(subData[[1]])))
    }else{
      times.dec <- seq(2000,2010)
    }
    
    vrange    <- c(min(times.dec), max(times.dec))
    if (length(times.dec) > 10){vrange <- c((max(times.dec)-10), max(times.dec))}
    
    dateRangeInput(inputId = 'slider_time',
                   label   = 'Time Range for x-axis',
                   min = paste0(vrange[1], '-01-01'),
                   max = paste0(vrange[2], '-01-01'))
  })
  output$plot1_input <- renderUI({
    div(style = 'height:50px;',
        selectInput(inputId = 'plot1_select',
                    label = 'Select Plot:',
                    width = '30%',
                    choices = METsteps::shinyPlotExtract(numD = 'HUC'),
                    selected = "shinyPlot_HUC_Time_Series_and_Difference"))
  })
  output$plot2_input <- renderUI({
    div(style = 'height:50px;',
        selectInput(inputId = 'plot2_select',
                    label = 'Select Plot:',
                    width = '30%',
                    choices = METsteps::shinyPlotExtract(numD = 'HUC'),
                    selected = "shinyPlot_HUC_subHUC_Plot"))
  })
  output$plot3_input <- renderUI({
    div(style = 'height:50px;',
        selectInput(inputId = 'plot3_select',
                    label = 'Select Plot:',
                    width = '30%',
                    choices = METsteps::shinyPlotExtract(numD = 'HUC'),
                    selected = "shinyPlot_HUC_Mean_Percentile_and_ECDF"))
  })
  # function to produce extra inputs as required by certain plots
  uiOptionsFun <- function(x){
    if (!is.null(x)){
      if (x == "shinyPlot_HUC_subHUC_Plot"){
        div(style = 'height:1px;',
            checkboxInput(inputId = 'sample_subHUCs',
                          label = 'Sample sub-HUCs to decrease render time?',
                          value = TRUE)
        )
      }
    }
  }
  # Add extra inputs as necessary according to uiOptionsFun
  output$uiOptionsforPlot1 <- renderUI({uiOptionsFun(input$plot1_select)})
  output$uiOptionsforPlot2 <- renderUI({uiOptionsFun(input$plot2_select)})
  output$uiOptionsforPlot3 <- renderUI({uiOptionsFun(input$plot3_select)})
  
  ##### Generate Default plots on Main Panel
  # Render empty leaflet map
  output$text2 <- renderText({'All months included'})
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
  output$plot1 <- renderPlot({
    if (is.null(input$plot1_select)){
      NULL
    }else{
      get(input$plot1_select)(default = T)
    }
  })
  output$plot2 <- renderPlot({
    if (is.null(input$plot2_select)){
      NULL
    }else{
      get(input$plot2_select)(default = T)
    }
    #METsteps::shinyPlot_HUC_subHUC_Plot(default. = T)
  })
  output$plot3 <- renderPlot({
    if (is.null(input$plot3_select)){
      NULL
    }else{
      get(input$plot3_select)(default = T)
    }
    #shinyPlot_HUC_Mean_Percentile_and_ECDF(default. = T)
  })
  # Note that no plots have been created yet
  plotsCreated <- F
  output$plotsCreated <- reactive({
    FALSE
  })
  outputOptions(output, "plotsCreated", suspendWhenHidden = FALSE)
  
  
  
  ##### Reactive statistical processing and map plotting after clicking update button
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
      output$plot1 <- renderPlot({
        if (is.null(input$plot1_select)){
          NULL
        }else{
          get(input$plot1_select)(default = T)
        }
        #METsteps::shinyPlot_HUC_Time_Series_and_Difference(default = T)
      })
      
      output$plot2 <- renderPlot({
        if (is.null(input$plot2_select)){
          NULL
        }else{
          get(input$plot2_select)(default = T)
        }
        #METsteps::shinyPlot_HUC_subHUC_Plot(default. = T)
      })
      output$plot3 <- renderPlot({
        if (is.null(input$plot3_select)){
          NULL
        }else{
          get(input$plot3_select)(default = T)
        }
        #METsteps::shinyPlot_HUC_Mean_Percentile_and_ECDF(default. = T)
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
    # Selected season/month
    if (input$seasMon_select == 'None'){
      subsetMonths <<- NA
      output$text2  <- renderText({
        'All months included'
      })
    }else if (input$seasMon_select %in% c('Fall', 'Winter', 'Spring', 'Summer')){
      subsetMonths <<- get(input$seasMon_select)
      output$text2  <- renderText({
        paste('Subsetted to:', paste0(base::month.name[subsetMonths], collapse = ', '))
      })
    }else{
      subsetMonths <<- which(base::month.abb == input$seasMon_select)
      output$text2  <- renderText({
        paste('Subsetted to:', paste0(base::month.name[subsetMonths], collapse = ', '))
      })
    }
    # Plot subsetted data?
    logPlotSubset <<- input$plot_seasMon_subset
    
    
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
    
    # If logPlotSubset = T, subset all data prior any stats work i.e. only subsetted data will be plotted in line plots
    subsetbyMonthsFun <- function(x, mts){
      return(x[which(lubridate::month(lubridate::date_decimal(index(x))) %in% mts), ])
    }
    
    if (length(subsetMonths) > 1){
      subData <- lapply(X   = subData,
                        FUN = subsetbyMonthsFun,
                        mts = subsetMonths)
    }else if (!is.na(subsetMonths)){
      subData <- lapply(X   = subData,
                        FUN = subsetbyMonthsFun,
                        mts = subsetMonths)
    }
    
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
    output$light_SingleHUC <- renderUI({
      highChoices <- as.character(colnames(subData[[1]]))
      highChoices[nchar(highChoices) < max(nchar(highChoices))] = paste0('0', highChoices[nchar(highChoices) < max(nchar(highChoices))])
      highChoices = sort(highChoices)
      selectizeInput(inputId = 'lightHUC',
                     label = 'Highlight Specific HUC',
                     choices = highChoices,
                     multiple = TRUE)
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
    
    #Apply statistical function as selected in UI !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (length(cb) > 2000 || (length(cb) > 18 && NCOL(cb[[1]]) > 2)){
      #Initialize cluster
      if (stat != 'shinyFun_one_Mean'){
        if (exists('cl') == FALSE){
          no_cores <- parallel::detectCores()-1
          cl <- parallel::makeCluster(no_cores)
        }else{
          if (is.null(cl)){
            no_cores <- parallel::detectCores()-1
            cl <- parallel::makeCluster(no_cores)
          }
        }
      }else{cl <- NULL}
      # Single-dataset stats
      if (stat == 'shinyFun_one_Mean'){
        statOUT <- as.numeric(unlist(lapply(X = cb, FUN = get(stat))))
      }else{
        statOUT <- as.numeric(unlist(parLapply(cl = cl, X = cb, fun = get(stat))))
        }
      #Close cluster
      if (exists('cl')){
        if (is.null(cl) == F) parallel::stopCluster(cl)
        rm(cl)
      }
    }else{statOUT <- as.numeric(unlist(lapply(X = cb, FUN = get(stat))))}
    # Replace Inf with NA
    statOUT[is.infinite(statOUT)] <- NA
    
    posAndneg <- (length(unique(sign(range(unlist(cb))))) > 1)
    
    if ((stat == 'CV') && (posAndneg)){
      output$textWarning <- renderText({'Warning: CV statistic is invalid for datasets with both positive and negative values'})
    }else{
      output$textWarning <- renderText({''})
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
  
  
  
  ##### Response to clicking "highlight individual HUC regions" button
  manHighIds <<- vector()
  highlight.manual <- observeEvent(input$highlightHUC, {
    HUCtoLight <- input$lightHUC
    manHighlightpoly <- inShape[inShape@data$HUC %in% HUCtoLight,]

    for (i in 1:nrow(manHighlightpoly)){
      manId <- paste0('manHigh', floor(runif(1)*1000))
      manHighIds <<- c(manHighIds, manId)
      leafletProxy(mapId = 'mymap',
                   data  = inShape) %>%
        addPolygons(data    = manHighlightpoly[i,],
                    weight  = 2,
                    color   = 'red',
                    fill    = F,
                    opacity = 1,
                    layerId = manId)
    }
  })
  
  
  
  ##### Response to clicking "clear highlight" button
  remove.highlight <- observeEvent(input$removeManHighlights, {
    if (length(manHighIds) > 0){
      leafletProxy(mapId = 'mymap',
                   data  = inShape) %>%
        removeShape(layerId = manHighIds)
      manHighIds <<- vector()
    }else{}
  })
  
  
  
  ##### Response to clicking (selecting) a polygon interactively  <- this is where most plotting functions will go
  observe({
    click <- input$mymap_shape_click
    
    #If click value is 'NULL' (when clicked between polygons) dont return any plots. Otherwise, continue.
    if (is.null(click) == FALSE){
      #Get data category
      dataCategory <<- unique((fileInfo[fileInfo$dataName %in% dnames,])$dataCategory)
      #HUC Clicked Upon
      HCU <<- click$id
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
          subToHUC        <<- lapply(X   = subData,
                                    FUN = subToHUCfun)
          
          if (logPlotSubset){
            if (length(subsetMonths) > 1){
              subToHUC <<- lapply(X   = subToHUC,
                                 FUN = subsetbyMonthsFun,
                                 mts = subsetMonths)
            }else if (!is.na(subsetMonths)){
              subToHUC <<- lapply(X   = subToHUC,
                                 FUN = subsetbyMonthsFun,
                                 mts = subsetMonths)
            }
          }
          
          presIndex       <- index(subToHUC[[1]])   #Record index to reapply after unlisting
          subToHUC        <<- zoo::as.zoo(matrix(data = unlist(subToHUC),
                                                nrow = length(subToHUC[[1]])))
          subToHUC[is.infinite(subToHUC)] <<- NA
          index(subToHUC) <<- presIndex
          colnames(subToHUC) <<- dnames
          
          #Generate ET plot
          #cbPalette <- c("#56B4E9", "#F0E442", "#CC79A7", "#0072B2", "#D55E00")
          cbPalette <<- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
          abl.col <- 'darkgrey'
          output$plot1 <- renderPlot({
            # List of reactive inputs - saves values to list, which can then be fed into plotting functions
            feederList <- list(sample_subHUCs = input$sample_subHUCs,
                               alpha_slider = input$alpha_slider,
                               slider_time = input$slider_time)
            # plotting function
            get(input$plot1_select)(feederList. = feederList)
            #METsteps::shinyPlot_HUC_Time_Series_and_Difference(feederList. = feederList)
          })
          
          #Generate ET plot for specified HUC level for clicked HUC
          ####!!!!!! Import subHUC levels
          subHUC10 <<- vector(mode   = 'list',
                              length = length(dnames))
          for (i in 1:length(dnames)){
            fnames.sub <- fileInfo[(fileInfo$dataName == dnames[i] & fileInfo$HUC == as.numeric(input$HUC_select) & fileInfo$timeStep == timeStep),]
            if (nrow(fnames.sub) == 0){
              fnames.sub <- fileInfo[(fileInfo$dataName == dnames[i] & fileInfo$HUC == 8 & fileInfo$timeStep == timeStep),]
              
            }
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
            subHUC10[[i]]      <<- tempHUC10
            names(subHUC10)[i] <<- fnames.sub$data.name
          }
          
          output$plot2 <- renderPlot({
            # List of reactive inputs - saves values to list, which can then be fed into plotting functions
            feederList <- list(sample_subHUCs = input$sample_subHUCs,
                           alpha_slider = input$alpha_slider,
                           slider_time = input$slider_time)
            # plotting function
            get(input$plot2_select)(feederList. = feederList)
            #METsteps::shinyPlot_HUC_subHUC_Plot(feederList. = feederList)
          })
          
          #Generate PErcentile and ECDF plots
          output$plot3 <- renderPlot({
            # List of reactive inputs - saves values to list, which can then be fed into plotting functions
            feederList <- list(sample_subHUCs = input$sample_subHUCs,
                               alpha_slider = input$alpha_slider,
                               slider_time = input$slider_time)
            # plotting function
            get(input$plot3_select)(feederList. = feederList)
            #METsteps::shinyPlot_HUC_Mean_Percentile_and_ECDF()
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
              output$ShirleysPlot <- renderPlot({
                plot(1:10, type = 'l')
              })
            }
          })
          
        }
      }
    }
  })
}

