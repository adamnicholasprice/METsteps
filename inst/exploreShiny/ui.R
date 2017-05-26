#http://rpubs.com/bhaskarvk/proj4leaflet

#->UI
#===============
ui <- dashboardPage(
  dashboardHeader(title = "Controls", disable = FALSE),
  dashboardSidebar(
    title <- "Input Data",
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
    selectInput(inputId  = 'season',
                label    = 'Select season',
                choices  = list('All' = 'All', 'Fall' = 'Fall', 'Winter' = 'Winter', 'Spring' = 'Spring', 'Summer' = 'Summer'),
                selected = 'All'),
    actionButton(inputId = 'go',
                 label   = "Update",
                 width   = '100%'),
    title <- "                ",
    title <- "Controls",
    selectInput(inputId  = "colors",
                label    = "Color Scheme",
                choices  = c('Viridis', 'Magma', 'Inferno', 'Plasma'),
                selected = 'Viridis'),
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
    disable = FALSE
    ),
  dashboardBody(
    tabBox(
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
                     plotOutput(outputId = 'plot2')
                     )
                 )
               ),
      tabPanel(title = 'Water Budget Equation',
               fluidRow(
                 box(radioButtons(inputId = 'eqn_timeStep',
                                  label   = 'Select Time Step',
                                  choices = fileInfo %>%
                                              distinct(timeStep) %>%
                                              .[['timeStep']],
                                  inline  = TRUE),
                     width  = 2,
                     status = 'danger'),
                 box(selectInput(inputId = 'eqn_HUC',
                                 label   = 'Select HUC',
                                 choices = list("2"=2, "4"=4, "6"=6, "8"=8)),
                     width  = 2,
                     status = 'danger')
                 ),
               p(),
               uiOutput(outputId = "equation_box"),
               actionButton(inputId = 'calculateEqn',
                            label   = 'Calculate Water Balance',
                            icon    = icon("refresh")),
               p(),
               textOutput(outputId = 'tab2text'),
               # fluidRow(box(tableOutput(outputId = 'tab2tableInputs'), status = 'primary'),
               #          box(tableOutput(outputId = 'tab2tableOutputs'), status = 'primary')),
               p(),
               leafletOutput(outputId = "wbMap"),
               p(),
               fluidRow(box(tableOutput(outputId = 'tab2tableInputs'), status = 'primary'),
                        box(tableOutput(outputId = 'tab2tableOutputs'), status = 'primary'))
               )
      )
    )
  
  
  # dashboardBody(
  #   fluidRow(
  #     box(
  #       textOutput(outputId    = "text2"),
  #       leafletOutput(outputId = "mymap"),
  #       p(),
  #       plotOutput(outputId    = 'plot3')),
  #     box(plotOutput(outputId = 'plot1'),
  #         plotOutput(outputId = 'plot2')
  #     )
  #   ),
  #   fluidRow(
  #     textOutput(outputId = "text1")
  #   )
  # )
)



