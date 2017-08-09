
#->Server
#===============
server <- function(input, output){
  ##### SidePanel reactive inputs
  # List available datasets
  output$products_available <- renderUI({
    fileInfo.sub <- fileInfo %>%
      filter(dataCategory == input$category_select) %>%
      filter(timeStep     == input$tstep_select) %>%
      distinct(dataName)
    ht <- (50 + 23*(nrow(fileInfo.sub)))
    if (nrow(fileInfo.sub) == 0){
      div(style = paste0('height:', ht, 'px; color:red;'),
          checkboxGroupInput(inputId  = 'data_select',
                             label    = 'No datasets for selected Component and Time Step',
                             choices  = fileInfo.sub$dataName,
                             selected = c('MOD16-A2', 'SSEBop')))
    }else{
      div(style = paste0('height:', ht, 'px'),
          checkboxGroupInput(inputId  = 'data_select',
                             label    = 'Select datasets to compare',
                             choices  = fileInfo.sub$dataName))
    }
    #50px for 1 dataset
    #100px for 3 datasets
    #200px for 7 datasets
    #480 for 10 datasets
  })
  #Time series UIinput
  output$trangeUI <- renderUI({
    if (is.null(input$data_select)){
      div(style='height:80px;',
          sliderInput(inputId = 'time_select',
                      label = 'Custom time range for statistics:',
                      min = 2000,
                      max = 2010,
                      value = c(2000,2010),
                      step = 1,
                      round = T)
      )
    }else{
      fnames <<- fileInfo %>%
        filter(dataName %in% input$data_select) %>%
        filter(timeStep %in% input$tstep_select) %>%
        filter(HUC %in% input$map_HUC_select) %>%
        distinct(fnames)
      fnames <<- unlist(fnames)
      fdates <<- extractMetadata(fnames)
      frange <<- lubridate::year(range(METsteps::timeOverlap(startDates = fdates$startDate,
                                                             endDates = fdates$endDate,
                                                             by = unique(fdates$timeStep))))
      if (sum(is.na(frange)) != 0){
        div(style = 'height:50px;color:red;',
            checkboxGroupInput(inputId  = 'time_select',
                               label    = 'Not all selected time series overlap',
                               choices  = NULL))
      }else{
        div(style='height:80px;',
            sliderInput(inputId = 'time_select',
                        label = 'Custom time range for statistics:',
                        min = frange[1],
                        max = frange[2],
                        value = frange,
                        step = 1,
                        round = T))
      }
    }
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
  output$time_available <- renderUI({
    if (exists('subData')){
      times.all <- as.Date(zoo::index(subData[[1]]))
      vrange <- c(lubridate::year(times.all[1]),
                  lubridate::year(as.Date(tail(times.all, n = 1)))
      )
      # if (length(times.dec) > 10){vrange <- c((max(times.dec)-10), max(times.dec))}
      if (length(seq(vrange[1],vrange[2])) > 10) vrange <- c((vrange[2] - 10), vrange[2])
      # subset to years
      times.new <- times.all[lubridate::year(times.all) %in% seq(vrange[1], vrange[2], 1)]
      dateRangeInput(inputId = 'slider_time',
                     label = 'Time Range for x-axis',
                     min = min(times.all),
                     max = max(times.all),
                     start = min(times.new),
                     end = max(times.new))
      # #times.dec <- unique(as.integer(index(subData[[1]])))
      # vrange <- range(lubridate::year(zoo::index(subData[[1]])))
      # if (length(seq(vrange[1],vrange[2])) > 10) vrange <- c((vrange[2] - 10), vrange[2])
      # dateRangeInput(inputId = 'slider_time',
      #                label = 'Time Range for x-axis',
      #                min = as.Date(zoo::index(subData[[1]])[1]),
      #                max = as.Date(zoo::index(subData[[1]])[length(zoo::index(subData[[1]]))]),
      #                start = as.Date(paste0(vrange[1], '-01-01')),
      #                end = as.Date(paste0(vrange[2], '-12-01'))
      # )
    }else{
      #times.dec <- seq(2000,2010)
      dateRangeInput(inputId = 'slider_time',
                     label   = 'Time Range for x-axis',
                     min = as.Date('2000-01-01'),
                     max = as.Date('2010-01-01'))
    }
    
    #vrange    <- c(min(times.dec), max(times.dec))
    # if (length(times.dec) > 10){vrange <- c((max(times.dec)-10), max(times.dec))}
    
    # dateRangeInput(inputId = 'slider_time',
    #                label   = 'Time Range for x-axis',
    #                min = paste0(vrange[1], '-01-01'),
    #                max = paste0(vrange[2], '-01-01'))
    
    
    
    ##############
    
    
    
  })
  # add highlight polygons with obs data check box input
  # output$highlightCheck <- renderUI({
  #   div(style = 'height:10px;',
  #       checkboxInput(inputId = 'showPolysWithObs',
  #                     label = 'Highlight polygons with observational data',
  #                     value = FALSE))
  # })
  # default plots
  output$plot1_input <- renderUI({
    div(style = 'height:50px;',
        selectInput(inputId = 'plot1_select',
                    label = 'Select Plot:',
                    width = '100%',
                    choices = METsteps::shinyPlotExtract(numD = 'HUC'),
                    selected = "shinyPlot_HUC_Time_Series_and_Difference"))
  })
  output$plot2_input <- renderUI({
    # tags$head(
    #   tags$style(type="text/css",
    #              "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
    # ),
    div(style = 'height:50px;',
        selectInput(inputId = 'plot2_select',
                    label = 'Select Plot:',
                    #label = '',
                    width = '100%',
                    choices = METsteps::shinyPlotExtract(numD = 'HUC'),
                    selected = "shinyPlot_HUC_subHUC_Plot"))
  })
  output$plot3_input <- renderUI({
    div(style = 'height:50px;',
        selectInput(inputId = 'plot3_select',
                    label = 'Select Plot:',
                    width = '100%',
                    choices = METsteps::shinyPlotExtract(numD = 'HUC'),
                    selected = "shinyPlot_HUC_Mean_Percentile_and_ECDF"))
  })
  # function to produce extra inputs as required by certain plots
  uiOptionsFun <- function(x, dnames. = dnames){
    if (!is.null(x)){
      if (x == "shinyPlot_HUC_subHUC_Plot"){
        div(#style = 'height:50px;',
          radioButtons(inputId = 'sample_subHUCs',
                        label = 'Sample sub-HUCs to decrease render time?',
                        choices = list(Yes = TRUE, No = FALSE),
                       selected = TRUE,
                        width = '100%',
                       inline = TRUE)
        )
      }else if (x == "shinyPlot_HUC_Taylor_Diagram"){
        if (!exists('dnames.')){dnames. = NA}
        div(style = 'height:50px;',
            selectInput(inputId = 'select_ref',
                        label = "Select 'reference' product:",
                        width = '70%',
                        choices = dnames.)
        )
      }else if (x == "shinyPlot_HUC_Time_Series_and_Difference"){
        #div(style = 'height:50px;',
            checkboxInput(inputId = 'showObs',
                        label = "Show observational data?",
                        value = TRUE)
        #)
      }
    }
  }
  
  # Add extra inputs as necessary according to uiOptionsFun
  output$uiOptionsforPlot1 <- renderUI({uiOptionsFun(x = input$plot1_select, dnames. = NA)})
  output$uiOptionsforPlot2 <- renderUI({uiOptionsFun(x = input$plot2_select, dnames. = NA)})
  output$uiOptionsforPlot3 <- renderUI({uiOptionsFun(x = input$plot3_select, dnames. = NA)})
  
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
    if (length(input$plot2_select) > 0){
      if (is.null(input$plot2_select)){
        NULL
      }else{
        get(input$plot2_select)(default = T)
      }
      #METsteps::shinyPlot_HUC_subHUC_Plot(default. = T)
      }
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
  plotsCreated <<- F
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
        if (length(input$plot2_select) > 0){
          if (is.null(input$plot2_select)){
            NULL
          }else{
            get(input$plot2_select)(default = T)
          }
          #METsteps::shinyPlot_HUC_subHUC_Plot(default. = T)
        }
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
    
    # add highlight polygons with obs data checkbox input
    output$highlightCheck <- renderUI({
      div(style = 'height:10px;',
          checkboxInput(inputId = 'showPolysWithObs',
                        label = 'Highlight polygons with observational data',
                        value = FALSE))
    })
    
    # remove 'clickpoly' if it was created earlier
    if (exists('clickpoly')) rm(clickpoly, envir = globalenv())
    
    #-------- Define user-selected inputs as independent variables
    # Names of datasets
    dnames    <<- input$data_select
    # Data Category
    dataCategory <<- input$category_select
    dataCat <<- dataCategory
    # Selected HUC region
    maphuc    <<- input$map_HUC_select
    # Selected timestep
    timeStep  <<- input$tstep_select
    timeStep.ct <<- timeStep
    # Selected statistic
    stat      <<- input$stat_select
    # Selected color scheme
    colScheme <<- input$colors
    # data year ranges
    tLim      <<- input$time_select
    # Selected season/month
    if (input$seasMon_select == 'None'){
      subsetMonths <<- NA
      output$text2  <- renderText({
        paste0('Datasets: ',
               METsteps::pasteSentence(x = dnames, period = T),
               '  All months included (years ',
               tLim[1],
               ' to ',
               tLim[2],
               ').')
      })
    }else if (input$seasMon_select %in% c('Fall', 'Winter', 'Spring', 'Summer')){
      subsetMonths <<- get(input$seasMon_select)
      output$text2  <- renderText({
        paste0('Datasets: ',
              METsteps::pasteSentence(x = dnames, period = T),
              '  Subsetted to: ',
              METsteps::pasteSentence(base::month.name[subsetMonths]),
              ' (years ', tLim[1], ' to ', tLim[2], ').'
              )
      })
    }else{
      subsetMonths <<- which(base::month.abb == input$seasMon_select)
      output$text2  <- renderText({
        paste0('Datasets: ',
               METsteps::pasteSentence(x = dnames, period = T),
               '  Subsetted to: ',
               METsteps::pasteSentence(base::month.name[subsetMonths]),
               ' (years ', tLim[1], ' to ', tLim[2], ').'
               )
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
      # info.cur        <- fileInfo[fileInfo$HUC == maphuc,]
      # info.cur        <- info.cur[info.cur$dataName == dnames[i],]
      # info.cur        <- info.cur[info.cur$timeStep == timeStep,]
      info.cur <<- fileInfo %>%
        filter(HUC == maphuc) %>%
        filter(dataName == dnames[i]) %>%
        filter(timeStep == timeStep.ct) %>%
        filter(dataCategory == dataCat)
      #File name
      fname.cur       <<- info.cur$fnames
      #Import file
      file.in         <- as.data.frame(feather::read_feather(path = file.path(path.feather, fname.cur)))
      #Define time-series for file (in as.Date format)
      # ind             <- lubridate::decimal_date(date = seq.Date(from       = info.cur$startDate,
      #                                                            by         = info.cur$timeStep,
      #                                                            length.out = nrow(file.in)))
      ind             <- seq.Date(from       = info.cur$startDate,
                                  by         = info.cur$timeStep,
                                  length.out = nrow(file.in))
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
        datlist        <- datlist[(as.numeric(index(datlist)) %in% common.indices),]
        index(datlist) <- as.Date(common.indices)
        return(datlist)
      }
      #Lapply function to trim datasets to common indices
      Data           <<- lapply(X   = list.all,
                                FUN = trimToIndices)
      # trim data from years outside tLim
      trimTotLim <- function(x, tLim){
        return(
          #x[(lubridate::year(lubridate::date_decimal(zoo::index(x)))) %in% seq(tLim[1], tLim[2], 1), ]
          x[which(lubridate::year(index(x)) %in% seq(tLim[1], tLim[2], 1)), ]
        )
      }
      Data <<- lapply(X = Data,
                     FUN = trimTotLim,
                     tLim = tLim)
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
    subsetbyMonthsFun <<- function(x, mts){
      #return(x[which(lubridate::month(lubridate::date_decimal(index(x))) %in% mts), ])
      return(x[which(lubridate::month(index(x)) %in% mts), ])
    }
    
    if (length(subsetMonths) > 1){
      subData <<- lapply(X   = subData,
                        FUN = subsetbyMonthsFun,
                        mts = subsetMonths)
    }else if (!is.na(subsetMonths)){
      subData <<- lapply(X   = subData,
                        FUN = subsetbyMonthsFun,
                        mts = subsetMonths)
    }
    
    #Set bounds on x-axis time period control
    output$time_available <- renderUI({
      times.all <- as.Date(zoo::index(subData[[1]]))
      vrange <- c(lubridate::year(times.all[1]),
                  lubridate::year(as.Date(tail(times.all, n = 1)))
                  )
      # if (length(times.dec) > 10){vrange <- c((max(times.dec)-10), max(times.dec))}
      if (length(seq(vrange[1],vrange[2])) > 10) vrange <- c((vrange[2] - 10), vrange[2])
      # subset to years
      times.new <- times.all[lubridate::year(times.all) %in% seq(vrange[1], vrange[2], 1)]
      dateRangeInput(inputId = 'slider_time',
                     label = 'Time Range for x-axis',
                     min = min(times.all),
                     max = max(times.all),
                     start = min(times.new),
                     end = max(times.new))
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
    
    # Manipulate point data information
    if (!is.null(ptmeta)){
      ptmeta2 <<- ptmeta %>%
        filter(dataCategory == dataCat) %>%
        filter(TimeStep == 'year')
      print('22')
      #filter out data outside of ranges in Shiny UI
      if (nrow(ptmeta2) > 0){
        #dLim <- range(lubridate::year(lubridate::date_decimal(zoo::index(Data[[1]]))))
        dLim <- range(lubridate::year(zoo::index(Data[[1]])))
        drFun <- function(obs, modRange){
          if (length(modRange) != 2) stop('drFun() incorrect.')
          obsSeq <- seq(from = lubridate::year(unlist(obs[7])),
                        to = lubridate::year(unlist(obs[8])),
                        by = 1)
          if ((sum(obsSeq %in% seq(from = modRange[1],
                                   to   = modRange[2],
                                   by   = 1))) > 0){
            return(TRUE)
          }else{
            return(FALSE)
          }
        }
        obsKeep <- apply(X = ptmeta2,
                         MARGIN = 1,
                         FUN = drFun,
                         modRange = dLim)
        ptmeta2  <<- ptmeta2[obsKeep,]
      }
      print('11')
    }else{
      ptmeta2 <<- NULL
    }
    
    # Manipulate point data information (2)
    if (!is.null(ptmeta)){
      # filter by category
      ptmeta3 <<- ptmeta %>%
        filter(dataCategory == dataCat)
      # generate UI
      output$allObsData <- renderUI({
        checkboxGroupInput(inputId = 'obsNames_select',
                           label = 'Select Observational Data',
                           choices = unique(ptmeta3$Source))
      })
      print(input$obsNames_select)
      # ptmeta3 <<- ptmeta3 %>%
      #   filter(Source %in% input$obsNames_select)
      # # Aggregate check
      # output$questionAggregate <- renderUI({
      #   checkboxInput(inputId = 'aggregateObs',
      #                 label = "Aggregate higher res data when available?",
      #                 value = TRUE)
      # })
      
      
    }
    
    # loop through all polygons and count number of observations within
    # POI <- polyHUC4
    # ct <- data.frame(HUC = POI@data$HUC4, obsCount = NA)
    # for (i in 1:nrow(POI)){
    #   tt <- ptmeta[,c('OurID', 'Lat', 'Lon')]
    #   sp::coordinates(tt) <- ~Lon+Lat
    #   projection(tt) <- projection(POI)
    #   tt <- tt[which(!is.na((sp::over(tt, POI[i,]))[,1])),]
    #   ct$obsCount[i] <- nrow(tt)
    # }
    # 
    # tt <- ptmeta[,c('OurID', 'Lat', 'Lon')]
    # sp::coordinates(tt) <- ~Lon+Lat
    # projection(tt) <- projection(POI)
    # tt <- tt[which(!is.na((sp::over(tt, POI[i,]))[,1])),]
    # ct$obsCount[i] <- nrow(tt)
    # 
    
    
    
    popupFun <<- function(x){
      paste0('Our ID: ', unlist(x[1]), '<br/>',
             'Source: ', unlist(x[4]), '<br/>',
             'Source ID: ', unlist(x[5]), '<br/>',
             'Time Step: ', unlist(x[6]), '<br/>',
             'First Obs: ', unlist(x[7]), '<br/>',
             'Last Obs: ', unlist(x[8]), '<br/>',
             'Measurement Type: ', unlist(x[8]), '<br/>',
             'Misc Info: ', unlist(x[12]), '<br/>')
    }
    if (!is.null(ptmeta2)){
      popupText <<- apply(X = ptmeta2,
                          MARGIN = 1,
                          FUN = popupFun)
    }
    markerMasterColors <- c('red', 'orange', 'green', 'blue', 'purple', 'white',
                            'darkred', 'beige', 'darkgreen', 'darkblue', 'darkpurple', 'gray',
                            'lightred', 'lightgreen', 'lightblue', 'pink', 'lightgray',
                            'cadetblue', 'black')
    markerColors <- markerMasterColors[as.integer(factor(ptmeta2$Source, unique(ptmeta2$Source)))]
    #ptmeta <<- ptmeta[,c('OurID', 'Lat', 'Lon')]
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
    if (input$showMarkers && (!is.null(ptmeta2))){
      if (nrow(ptmeta2) > 0){
        leafletProxy(mapId = 'mymap',
                     data  = inShape) %>%
          addAwesomeMarkers(lng = ptmeta2$Lon,
                            lat = ptmeta2$Lat,
                            popup = popupText,
                            icon = makeAwesomeIcon(icon = 'flag', markerColor = markerColors)
          )
      }
    }
  })
  
  ##### Remove/Add point data according to input$showMarkers
  observe({
    if (!is.null(ptmeta2)){
      if (nrow(ptmeta2) > 0){
        if (input$showMarkers == FALSE){
          leafletProxy(mapId = 'mymap',
                       data  = inShape) %>%
            clearMarkers()
        }else if (input$showMarkers && plotsCreated){
          leafletProxy(mapId = 'mymap',
                       data  = inShape) %>%
            addMarkers(lng   = ptmeta2$Lon,
                       lat   = ptmeta2$Lat,
                       popup = popupText)
        } 
      } 
    }
  })
  
  ##### Add/Remove polygon with obs data highlights according to input$showPolysWithObs
  observe({
    if (!is.null(ptmeta2)){
      if (nrow(ptmeta2) > 0){
        if (!is.null(input$showPolysWithObs)){
          if (input$showPolysWithObs && plotsCreated){
            tt2                     <- ptmeta2
            sp::coordinates(tt2)    <- ~Lon+Lat
            raster::projection(tt2) <- raster::projection(inShape)
            inShape2                <- inShape
            inShape2                <- inShape2[,grepl(pattern = 'HUC', x = colnames(inShape2@data))]
            colnames(inShape2@data) <- 'HUC'
            kp                      <- na.omit(unique((over(tt2, inShape2))[,1]))
            inShape3                <<- inShape2[(inShape2@data$HUC %in% kp), ]
            
            # add highlights to map
            hoIds <<- vector()
            for (i in 1:nrow(inShape3)){
              hoId  <- paste0('ho', floor(runif(1)*1000))
              hoIds <<- c(hoIds, hoId)
              leafletProxy(mapId = 'mymap',
                           data  = inShape3) %>%
                addPolygons(data         = inShape3[i,],
                            weight       = 2,
                            color        = 'lightblue',
                            fill         = F,
                            opacity      = 1,
                            smoothFactor = 1,
                            layerId      = hoId)
            }
            if (exists('clickpoly')){
              leafletProxy(mapId = 'mymap') %>%
                addPolygons(data    = clickpoly,
                            weight  = 2,
                            color   = '#00ff00',
                            fill    = F,
                            opacity = 1,
                            layerId = 'clickhighlight')
            }
          }else{
            if (exists('hoIds')){
              leafletProxy(mapId = 'mymap') %>%
                removeShape(layerId = hoIds)
              hoIds <<- vector()
            }
          } 
        } 
      } 
    }
  })
  
  ##### Reactive Create New Map
  newMap <- observeEvent(input$goNewMap, {
    output$plotsCreated <- reactive({
      FALSE
    })
  })
  
  
  ##### Response to clicking "highlight individual HUC regions" button
  manHighIds <<- vector()
  highlight.manual   <- observeEvent(input$highlightHUC, {
    HUCtoLight       <- input$lightHUC
    manHighlightpoly <- inShape[inShape@data$HUC %in% HUCtoLight,]

    for (i in 1:nrow(manHighlightpoly)){
      manId      <- paste0('manHigh', floor(runif(1)*1000))
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
    # Add extra inputs as necessary according to uiOptionsFun
    if (exists('dnames')){
      output$uiOptionsforPlot1 <- renderUI({uiOptionsFun(x = input$plot1_select, dnames. = dnames)})
      output$uiOptionsforPlot2 <- renderUI({uiOptionsFun(x = input$plot2_select, dnames. = dnames)})
      output$uiOptionsforPlot3 <- renderUI({uiOptionsFun(x = input$plot3_select, dnames. = dnames)})
    }
    
    #If click value is 'NULL' (when clicked between polygons) dont return any plots. Otherwise, continue.
    if (is.null(click) == FALSE){
      #Get data category
      #dataCategory <<- unique((fileInfo[fileInfo$dataName %in% dnames,])$dataCategory)
      # dataCategory <<- input$category_select
      # dataCat <<- dataCategory
      #HUC Clicked Upon
      HCU <<- click$id
      if (is.na(suppressWarnings(as.numeric(HCU))) == FALSE){
        #Create update polygon (last polygon clicked on has highlighted border)
        clickpoly <<- inShape[inShape@data$HUC == HCU,]
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
            subsetbyMonthsFun <<- function(x, mts){
              #return(x[which(lubridate::month(lubridate::date_decimal(index(x))) %in% mts), ])
              return(x[which(lubridate::month(index(x)) %in% mts), ])
            }
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

          #### Identify point data within polygon
          if (!is.null(ptmeta2)){
            if (nrow(ptmeta2) > 0){
              ptSP <- ptmeta2[,c('OurID', 'Lat', 'Lon')]
              sp::coordinates(ptSP) <- ~Lon+Lat
              projection(ptSP) <- projection(clickpoly)
              ptSP.trim <<- ptSP[which(!is.na((sp::over(ptSP, clickpoly))[,1])),] 
            }else{
              ptSP.trim <<- NULL
            }
          }else{
            ptSP.trim <<- NULL
          }
          
          #Generate ET plot
          #cbPalette <- c("#56B4E9", "#F0E442", "#CC79A7", "#0072B2", "#D55E00")
          cbPalette <<- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
          abl.col <- 'darkgrey'
          output$plot1 <- renderPlot({
            # List of reactive inputs - saves values to list, which can then be fed into plotting functions
            feederList <- list(sample_subHUCs = input$sample_subHUCs,
                               alpha_slider = input$alpha_slider,
                               slider_time = input$slider_time,
                               showObs = input$showObs)
            print(input$slider_time)
            # plotting function
            get(input$plot1_select)(feederList. = feederList)
          })
          
          #Generate ET plot for specified HUC level for clicked HUC
          ####!!!!!! Import subHUC levels
          subHUC10 <<- vector(mode   = 'list',
                              length = length(dnames))
          for (i in 1:length(dnames)){
            #fnames.sub <- fileInfo[(fileInfo$dataName == dnames[i] & fileInfo$HUC == as.numeric(input$HUC_select) & fileInfo$timeStep == timeStep),]
            fnames.sub <- fileInfo %>%
              filter(dataName == dnames[i]) %>%
              filter(HUC == as.numeric(input$HUC_select)) %>%
              filter(timeStep == timeStep.ct) %>%
              filter(dataCategory == dataCat)
            if (nrow(fnames.sub) == 0){
              #fnames.sub <- fileInfo[(fileInfo$dataName == dnames[i] & fileInfo$HUC == 8 & fileInfo$timeStep == timeStep),]
              fnames.sub <- fileInfo %>%
                filter(dataName == dnames[i]) %>%
                filter(HUC == 8) %>%
                filter(timeStep == timeStep.ct) %>%
                filter(dataCategory == dataCat)
              
            }
            path.f             <<- file.path(path.feather,
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
            # ind                <- lubridate::decimal_date(seq.Date(from       = fnames.sub$startDate,
            #                                                        by         = fnames.sub$timeStep,
            #                                                        length.out = nrow(tempHUC10)))
            ind                <- seq.Date(from       = fnames.sub$startDate,
                                           by         = fnames.sub$timeStep,
                                           length.out = nrow(tempHUC10))
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
                               slider_time = input$slider_time,
                               showObs = input$showObs)
            # plotting function
            get(input$plot2_select)(feederList. = feederList)
            #METsteps::shinyPlot_HUC_subHUC_Plot(feederList. = feederList)
          })
          
          #Generate PErcentile and ECDF plots
          output$plot3 <- renderPlot({
            # List of reactive inputs - saves values to list, which can then be fed into plotting functions
            feederList <- list(sample_subHUCs = input$sample_subHUCs,
                               alpha_slider = input$alpha_slider,
                               slider_time = input$slider_time,
                               showObs = input$showObs)
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

