library(shiny)

# Define UI for application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Evaluation of weighted regressions for Patuxent estuary, mean models"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    actionButton('getdat', label = "Get data!"),
    
    selectInput(inputId = 'stat',
      label = h4('Pick station'),
      choices = c('TF1.6', 'LE1.2'), 
      selected = 'LE1.2'),
    
    uiOutput("daterng"),
    
    selectInput(inputId = 'exp', 
      label = h4('Pick explanatory variable'), 
      choices = c('Flow', 'Salinity'), 
      selected = 'Salinity'
    ),
      
    selectInput("day_num", 
      label = h4("Seasonal window"), 
      choices = as.character(c(seq(0.5, 1, by = 0.25), 2, 10)),
      selected = '0.5'
    ),
    
    selectInput("year", 
      label = h4('Year window'), 
      choices = as.character(c(seq(5, 15, by = 3), 50)),
      selected = '5'
    ),
    
    selectInput("sal", 
      label = h4("Salinity/flow window"), 
      choices = as.character(c(seq(0.5, 1, by = 0.1), 5)),
      selected = '0.5'
    ),
    
    selectInput(inputId = 'logspace',
      label = h4('Chlorophyll'),
      choices = c('observed', 'log'), 
      selected = 'log'
      ),
    
    textInput("col_vec",
      label = h4('Color palette'), 
      value = NULL),
    
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
        
        fluidRow(
          
          column(4, 
            selectInput(inputId = 'annuals',
              label = h4('Aggregation period'),
              choices = c('annual', 'monthly'), 
              selected = 'annual'
              )
          ),
          
          column(4, 
            selectInput(inputId = 'modout',
              label = h4('Model output'),
              choices = c('predicted', 'normalized'), 
              selected = 'salinity-normalized'
              )
          )
          
        ),
        
        h4('Performance metrics'), 
        
        h5(HTML('Performance metrics include root mean square error (rmse) and normalized mean square error (nmse).  Root mean square error is the square root of the mean of the squared residuals, whereas normalized mean square error is the sum of the squared errors divided by the sum of the non-conditional errors.  Residuals are observed minus predicted values.')),
        
        tableOutput("tableperf"),
        
        h4(HTML('Observed and predicted')), 
        
        h5(HTML('Observed chlorophyll (points) and model predictions (lines) as a function of salinity, season, and year.')),
        
        plotOutput("fitplot", height = "100%"),
        
        h4(HTML('Observed and predicted by month')), 
        
        h5(HTML('Observed chlorophyll (points) and model predictions (lines) by month')),
        
        column(3, 
            textInput("fitmonth",
              label = h4('Months'), 
              value = '1:12')
          ),
        
        plotOutput("fitmoplot", height = "100%"),
        
        h4(HTML('Predicted and salinity-normalized estimates')), 
        
        h5(HTML('Predicted (points) and salinity-normalized (lines) results from to evaluate the influence of salinity changes on chlorophyll. Substantial differences between lines and points would indicate effects of salinity on chlorophyll.')),
        
        plotOutput("resplot", height = "100%")
        
      ),
      
      tabPanel("Interpolation grids", 
      
        fluidRow(
          
          h5(HTML('Plot the relationship between chlorophyll and salinity across the time series using a gridded surface for chlorophyll.  Plots are facetted by month to reduce seasonal variation in the observed trends. The plots are also constrained to the fifth and ninety-fifth percentile of observed salinity values during the month of interest to limit the predictions within the data domain. Linear interpolation is used to create a smoother surface.  The raw data in the interpolation grid can be seen by setting the smoothing factor to one.  Processing time may be excessive for large smoothing factors (e.g., > 10).')), 
          
          column(3, 
            selectInput("gridsorlines",
              label = h4('Plot type'),
              choices = c('grid', 'line'), 
              selected = 'grid')
          ),
          
          column(3, 
            textInput("month",
              label = h4('Months'), 
              value = '1:12')
          ),
          
          column(3, 
            numericInput(inputId = 'sal_fac',
              label = h4('Smoothing factor'),
              value = 3,
              min = 1, 
              max = 10,
              step = 1
              )
          )
          
        ),
        
        plotOutput("gridplot", height = "100%")
      
      )
      
    ), width = 9
  
  )

))