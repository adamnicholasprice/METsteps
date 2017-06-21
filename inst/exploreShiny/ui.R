#http://rpubs.com/bhaskarvk/proj4leaflet

#->UI
#===============
ui <- dashboardPage(
  dashboardHeader(title = "Controls", disable = FALSE),
  dashboardSidebar(
    conditionalPanel(
      condition = "output.plotsCreated == false",
      title <- "Input Controls",
      selectInput(inputId  = 'category_select',
                  label    = 'Component',
                  choices  = unique(fileInfo$dataCategory),
                  selected = 'AET'),
      selectInput(inputId  = 'tstep_select',
                  label    = 'Time Step',
                  choices  = unique(fileInfo$timeStep),
                  selected = 'month'),
      uiOutput(outputId = 'products_available'),
      selectInput(inputId  = 'map_HUC_select',
                  label    = 'Map HUC level',
                  choices  = list('HUC 2' = 2, 'HUC 4' = 4, 'HUC 6' = 6, 'HUC 8' = 8),
                  selected = 2),
      uiOutput(outputId = "stats_available"),
      title <- "Color Scheme",
      checkboxInput(inputId = 'colorCheckBox',
                    label    = 'Manual Color Scheme?',
                    value    = FALSE),
      conditionalPanel(
        condition = 'input.colorCheckBox == false',
        selectInput(inputId  = "colors",
                    label    = "Colors",
                    choices  = c('Viridis', 'Magma', 'Inferno', 'Plasma'),
                    selected = 'Viridis')
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
      actionButton(inputId = 'go',
                   label   = "Create Map",
                   width   = '87%'),
      textOutput(outputId = 'colorError')
    ),
    conditionalPanel(
      condition = "output.plotsCreated",
      title <- "Map Controls",
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
    disable = FALSE
  ),
  dashboardBody(
    title = "",
    id    = "tabset1",
    width = 12,
    tabPanel(title = "Component Exploration",
             fluidRow(
               box(
                 textOutput(outputId    = "text2"),
                 leafletOutput(outputId = "mymap"),
                 p(),
                 plotOutput(outputId    = 'plot3')),
               box(plotOutput(outputId = 'plot1'),
                   tabsetPanel(
                     tabPanel(title = 'Sub-HUCs',
                              plotOutput(outputId = 'plot2')
                              ),
                     tabPanel(title = 'Taylor',
                              uiOutput(outputId = "datasets_for_Taylor"),
                              plotOutput(outputId = 'taylorPlotclick'),
                              h6('Taylor, K.E. Summarizing multiple aspects of model performance in a 
                                 single diagram (2001). Journal of Geophysical Research.')
                     )
                     )
                   )
               )
             )
    )
)



