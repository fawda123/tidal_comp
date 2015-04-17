library(shiny)

# Define UI for application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Evaluation of weighted regressions for Patuxent estuary"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    selectInput(inputId = 'stat',
      label = h4('Pick station'),
      choices = c('TF1.6', 'TF1.7', 'LE1.3'), 
      selected = 'LE1.3'),
    
    uiOutput("daterng"),
    
    selectInput("day_num", 
      label = h4("Seasonal window"), 
      choices = as.character(seq(0.1, 2, length = 5)),
      selected = '0.575'
    ),
    
    selectInput("year", 
      label = h4('Year window'), 
      choices = as.character(seq(1, 30, length = 5)),
      selected = '8.25'
    ),
    
    selectInput("sal", 
      label = h4("Salinity window"), 
      choices = as.character(seq(0.25, 15, length = 5)),
      selected = '3.9375'
    ),
    
    selectInput(inputId = 'logspace',
      label = h4('Chlorophyll'),
      choices = c('observed', 'log'), 
      selected = 'observed'
      ),
    
    textInput("col_vec",
      label = h4('Color palette'), 
      value = 'Spectral'),
    
    submitButton("Submit"), 
    
    width = 3
    
  ),
  
  
  # output tabs
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("Observed data", 
        
        column(3, 
          selectInput(inputId = 'obstype',
            label = h4('Plot type'),
            choices = c('lines', 'points'), 
            selected = 'lines'
          )
        ),
        
        plotOutput("obsplot", height = "100%")
        
      ),
      
      tabPanel("Weights plot", 
        
        uiOutput("refdate"),
        
        plotOutput("wtsplot", height = "100%")
        
      ),
      
      tabPanel("Predictions", 
        
        fluidRow(
          
          column(4, 
            selectInput(inputId = 'annuals',
              label = h4('Aggregation period'),
              choices = c('annual', 'monthly'), 
              selected = 'annual'
              )
            
          )
          
        ),
        
        plotOutput("resplot", height = "100%")
        
      ),
      
      tabPanel("Seasonal grid plot", 
      
        fluidRow(
          
          column(3, 
            selectInput(inputId = 'month',
              label = h4('Grid month'),
              choices = as.character(seq(1, 12)), 
              selected = '7'
              )
          ),
          
          column(3, 
            uiOutput('tau')
          ), 
          
          column(3, 
            numericInput(inputId = 'sal_fac',
              label = h4('Smoothing factor'),
              value = 10,
              min = 1, 
              max = 20,
              step = 1
              )
          )
          
        ),
        
        plotOutput("gridplot", height = "100%")
      
      )
      
    ), width = 9
  
  )

))