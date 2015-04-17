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
      selected = 'log'
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
        
      h5(HTML('View the raw chlorophyll (top) and salinity (bottom) data for the time series.')),   
        
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
        
        h5(HTML('The plots illustrate the weights that are used when fitting a weighted regression in reference to a single observation. The plots indicate one example of many because a single model is used for each unique observation during the fitting process.  The top plot shows salinity over time with the points colored and sized by the combined weight vector. The remaining four plots show the weights over time for each separate weighting component (months/days, year, and salinity) and the final combined vector.  The date at the top is the closest date in the observed data to the reference position.')), 
        
        uiOutput("refdate"),
        
        plotOutput("wtsplot", height = "100%")
        
      ),
      
      tabPanel("Predictions and performance", 
        
        h5(HTML('Plot combined predicted (points) and normalized (lines) results from a tidal object to evaluate the influence of salinity changes on chlorophyll. Values can be shown as annual mean aggregations or as monthly results.')),
        
        fluidRow(
          
          column(4, 
            selectInput(inputId = 'annuals',
              label = h4('Aggregation period'),
              choices = c('annual', 'monthly'), 
              selected = 'annual'
              )
            
          )
          
        ),
        
        plotOutput("resplot", height = "100%"),
        
        h4('Performance metrics for each model'), 
        
        h5(HTML('Performance metrics for each model include goodness of fit (gfit), root mean square error (rmse), and normalized mean square error (nmse).  Goodness of fit is calculated as 1 minus the ratio between the sum of absolute deviations in the fully parameterized models and the sum of absolute deviations in the null (non-conditional) quantile model. Root mean square error is based on square root of the mean of the squared residuals. Normalized mean square error is the sum of the squared errors divided by the sum of the non-conditional errors (i.e., sum of the squared values of the observed minus the mean of the observed). This measure allows comparability of error values for data with different ranges, although the interpretation for quantile models is not clear.')),
        
        tableOutput("tableperf")
        
      ),
      
      tabPanel("Seasonal grid plot", 
      
        fluidRow(
          
          h5(HTML('Plot the relationship between chlorophyll and salinity across the time series using a gridded surface for chlorophyll. The plot is limited to the same month throughout the time series to limit seasonal variation. The plot is also constrained to the fifth and ninety-fifth percentile of observed salinity values during the month of interest to limit the predictions within the data domain. Linear interpolation is used to create a smoother surface.  The raw data in the interpolation grid can be seen by setting the smoothing factor to one.')), 
          
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