ui <- dashboardPage(
  dashboardHeader(title = 'Data'),
  dashboardSidebar(),
  dashboardBody(
    tabBox(
      id    = "tabset1",
      width = 12,
      tabPanel(title = 'Input Selection',
               fluidRow(
                 box(radioButtons(inputId = 'timeStep',
                                  label   = 'Select Time Step',
                                  choices = fileInfo %>%
                                    distinct(timeStep) %>%
                                    unlist(use.names = F),
                                  inline  = TRUE),
                     width  = 2,
                     status = 'danger'),
                 box(selectInput(inputId = 'HUC',
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
               textOutput(outputId = 'tab1infoText'),
               textOutput(outputId = 'tab1errorText'),
               tags$head(tags$style("#tab1errorText{color: red;
                                                           font-size: 18px;
                                                    }")),
               fluidRow(
                 box(
                   box(tableOutput(outputId = 'tab1tableInputs'), status = 'primary', width = 12),
                   box(tableOutput(outputId = 'tab1tableOutputs'), status = 'primary', width = 12)),
                 box(plotOutput(outputId = 'timeperiodPlot'), status = 'success'))
      ),
      tabPanel(title = 'Water Balance Results',
               textOutput(outputId = 'wbMapTitle'),
               leafletOutput(outputId = "wbMap"),
               p(),
               fluidRow(box(tableOutput(outputId = 'tab2tableInputs'), status = 'primary'),
                        box(tableOutput(outputId = 'tab2tableOutputs'), status = 'primary'))
      )
    )
  )
)