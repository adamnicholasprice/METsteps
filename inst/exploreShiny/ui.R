#http://rpubs.com/bhaskarvk/proj4leaflet

#->UI
#===============
ui <- dashboardPage(
  dashboardHeader(title = "Controls", disable = FALSE),
  dashboardSidebar(
    conditionalPanel(
      condition = "output.plotsCreated == false",
      div(style = 'height:10px',
        title <- "Input Controls"),
      div(style='height:60px;',
          selectInput(inputId  = 'category_select',
                      label    = 'Component',
                      choices  = unique(fileInfo$dataCategory),
                      selected = 'AET')),
      div(style='height:60px;',
          selectInput(inputId  = 'tstep_select',
                      label    = 'Time Step',
                      choices  = unique(fileInfo$timeStep),
                      selected = 'month')),
      uiOutput(outputId = 'products_available'),
      div(style='height:60px;',
          selectInput(inputId  = 'map_HUC_select',
                      label    = 'Map HUC level',
                      choices  = list('HUC 2' = 2, 'HUC 4' = 4, 'HUC 6' = 6, 'HUC 8' = 8),
                      selected = 2)),
      uiOutput(outputId = 'trangeUI'),
      div(style='height:55px;',
          uiOutput(outputId = "stats_available")),
      div(style='height:50px;',
          checkboxInput(inputId = 'colorCheckBox',
                        label    = 'Manual Color Scheme?',
                        value    = FALSE)),
      conditionalPanel(
        condition = 'input.colorCheckBox == false',
        div(style='height:65px;',
            selectInput(inputId  = "colors",
                        label    = "Colors",
                        choices  = c('Viridis', 'Magma', 'Inferno', 'Plasma'),
                        selected = 'Viridis'))
      ),
      conditionalPanel(
        condition = 'input.colorCheckBox == true',
        div(style="display:inline-block; color:#FF0000",
            tags$label("low.", `for` = "minCol"), 
            tags$input(id = "minCol", type = "text", value = '', class = 'input-small')),
        div(style="display:inline-block; color:#FF0000",
            tags$label("mid.", `for` = "midCol"), 
            tags$input(id = "midCol", type = "text", value = '', class = 'input-small')),
        div(style="display:inline-block; color:#FF0000",
            tags$label("high", `for` = "maxCol"), 
            tags$input(id = "maxCol", type = "text", value = '', class = 'input-small'))
      ),
      div(style='height:50px;',
          radioButtons(inputId = 'subset_Option',
                       label = 'Subset Option',
                       choices = list('Season' = TRUE,
                                      'Month'  = FALSE),
                       inline = T)),
      div(style='height:60px;',
          uiOutput(outputId = 'subsetoutput')),
      checkboxInput(inputId = 'plot_seasMon_subset',
                    label = 'Plot subsetted data?',
                    value = TRUE),
      actionButton(inputId = 'go',
                   label   = "Create Map",
                   width   = '87%'),
      textOutput(outputId = 'colorError')
    ),
    conditionalPanel(
      condition = "output.plotsCreated",
      title <- "Map Controls",
      uiOutput(outputId = 'light_SingleHUC'),
      actionButton(inputId = 'highlightHUC',
                   label   = "Highlight",
                   width   = '87%'),
      actionButton(inputId = 'removeManHighlights',
                   label   = "Clear Highlights",
                   width   = '87%'),
      uiOutput(outputId = 'maptime_available')
    ),
    conditionalPanel(
      condition = "output.plotsCreated",
      title <- 'Line Plot Controls',
      uiOutput(outputId = 'time_available'),
      sliderInput(inputId = 'alpha_slider',
                  label   = 'Line Opacity', 
                  min     = 0.0,
                  max     = 1.0,
                  value   = 0.5),
      selectInput(inputId  = 'HUC_select',
                  label    = 'HUC4plot',
                  choices  = list('HUC 08' = 8, 'HUC 10' = 10),
                  selected = 10),
      sliderInput(inputId = 'ylimmax_slider',
                  label   = 'Y-axis maximum',
                  min     = 1,
                  max     = 1000,
                  value   = 250),
      actionButton(inputId = 'goNewMap',
                   label   = "Create Different Map",
                   width   = '87%')
    ),
    conditionalPanel(
      condition = "output.plotsCreated",
      div(style = 'height:3px;',
          title <- 'Observational Pt Data Controls'),
      div(style='height:50px;',
          checkboxInput(inputId = 'aggregateObs',
                        label = "Aggregate higher res data when available?",
                        value = FALSE)
      ),
      uiOutput(outputId = 'allObsData'),
      actionButton(inputId = 'mapObs',
                   label   = "Map observational data",
                   width   = '87%'),
      actionButton(inputId = 'rmmapObs',
                   label   = 'Clear observational data',
                   width   = '87%')
    ),
    disable = FALSE
  ),
  #######################################################################
  dashboardBody(
    title = "",
    id    = "tabset1",
    width = 12,
    tabPanel(title = "Component Exploration",
             fluidRow(
               box(
                 textOutput(outputId    = "text2"),
                 div(style = 'color:red;',
                     textOutput(outputId    = "textWarning")),
                 leafletOutput(outputId = "mymap"),
                 fluidRow(
                   # column(4, div(style = 'height:10px;',
                   #               checkboxInput(inputId = 'showMarkers',
                   #                             label = 'Show available point data',
                   #                             value = TRUE))),
                   column(8, uiOutput(outputId = 'highlightCheck'))
                 ),
                 div(style = 'height:1px;',
                     tags$hr()),
                 fluidRow(
                   column(4, uiOutput(outputId = 'plot3_input')),
                   column(5, uiOutput(outputId = 'uiOptionsforPlot3')),
                   column(2, 
                          div(style = 'height:12px', p()),
                          actionButton(inputId = 'ExportPlot3',
                                       label = 'Figure',
                                       icon = icon("download"),
                                       width = '100%'),
                          offset = 1)
                 ),
                 plotOutput(outputId    = 'plot3')),
               box(
                 fluidRow(
                   column(4, uiOutput(outputId = 'plot1_input')),
                   column(5, uiOutput(outputId = 'uiOptionsforPlot1')),
                   column(2, 
                          div(style = 'height:12px', p()),
                          actionButton(inputId = 'ExportPlot1',
                                       label = 'Figure',
                                       icon = icon('download'),
                                       width = '100%'),
                          offset = 1)
                 ),
                 plotOutput(outputId = 'plot1'),
                 div(style = 'height:1px;',
                     tags$hr()),
                 fluidRow(
                   column(4, uiOutput(outputId = 'plot2_input')),
                   column(5, uiOutput(outputId = 'uiOptionsforPlot2')),
                   column(2, 
                          div(style = 'height:12px', p()),
                          actionButton(inputId = 'ExportPlot2',
                                       label = 'Figure',
                                       icon = icon('download'),
                                       width = '100%'),
                          offset = 1)
                 ),
                 #uiOutput(outputId = 'plot2_input'),
                 #uiOutput(outputId = 'uiOptionsforPlot2'),  #Not currently in use
                 plotOutput(outputId = 'plot2')
                 
                 
                 
                   # tabsetPanel(
                   #   tabPanel(title = 'Sub-HUCs',
                   #            checkboxInput(inputId = 'sample_subHUCs',
                   #                          label = 'Sample sub-HUCs to increase plotting speed?',
                   #                          value = TRUE),
                   #            plotOutput(outputId = 'plot2',
                   #                       #add the hover options
                   #                       hover = hoverOpts(
                   #                         id = "plot2_hover",
                   #                         delay = 400,
                   #                         nullOutside = TRUE)
                   #                       )
                   #            # textOutput(outputId = 'hoverText')
                   #            # verbatimTextOutput(outputId = "info",
                   #            #                    placeholder = T)
                   #            ),
                   #   tabPanel(title = 'Taylor',
                   #            uiOutput(outputId = "datasets_for_Taylor"),
                   #            plotOutput(outputId = 'taylorPlotclick'),
                   #            h6('Taylor, K.E. Summarizing multiple aspects of model performance in a 
                   #               single diagram (2001). Journal of Geophysical Research.')
                   #   ),
                   #   tabPanel(title = 'Shirley',
                   #            plotOutput(outputId = 'ShirleysPlot')
                   #   )
                   #   )
                   )
               )
             )
    )
)



