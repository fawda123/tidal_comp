library(shiny)

# Define UI for application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Evaluation of weighting scheme for Patuxent regressions"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    selectInput(inputId = 'stat',
      label = h3('Pick station'),
      choices = c('LE1.1', 'LE1.2', 'LE1.3', 'LE1.4', 'RET1.1', 'TF1.3', 'TF1.4', 'TF1.5', 'TF1.6', 'TF1.7'), 
      selected = 'TF1.6'),
    
    uiOutput("refdate"),
    
    uiOutput("daterng"),
    
    numericInput("day_num", 
      label = h3("Pick seasonal window"), 
      min = 0, 
      max = 1e6, 
      value = 0.5, 
      step = 0.1),

    numericInput("year", 
      label = h3('Pick year window'), 
      min = 0, 
      max = 1e6, 
      value = 10, 
      step = 1),
    
    numericInput("sal", 
      label = h3("Pick salinity window"), 
      min = 0, 
      max = 1e6, 
      value = 2, 
      step = 0.1),

    textInput("col_vec",
      label = h3('Color palette'), 
      value = 'Spectral'),
    
    submitButton("Submit"), 
    
    width = 4
    
  ),
  
  # plot output
  mainPanel(
    plotOutput("simplot", width = "100%")
  )
  
#   # output tabs
#   mainPanel(
#     tabsetPanel(
#       tabPanel("Segment identification", plotOutput("segplot", height = "110%")),
#       tabPanel("Depth estimates", plotOutput("simplot", height = "110%"))
# #       tabPanel("Summary tables", h3('Summary of metabolism estimates'), tableOutput("tablemet"), h3('Correlations with tidal change'), tableOutput('tablecorr'))
#       ), width = 9
#     )

))