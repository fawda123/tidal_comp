# packages to use
# library(WRTDStidal)
devtools::load_all('M:/docs/wtreg_for_estuaries')

# functions to use

# load data
load('pax_data.RData')
pax_data$lim <- 0

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  # for initial window center
  output$refdate <- renderUI({

    stat <- input$stat
    dat <- pax_data[pax_data$STATION %in% stat, ]
    rngs <- range(dat$date)
    refs <- as.Date(mean(dat$date), format = '%Y-%m-%d', 
      origin = '1970-01-01')
    
    dateInput('refdt', 
      label = h3("Pick reference date"),
      value = refs,
      startview = 'year'
      )
  
  })
  
  # for initial date range
  output$daterng <- renderUI({

    stat <- input$stat
    dat <- pax_data[pax_data$STATION %in% stat, ]
    rngs <- range(dat$date)
    
    dateRangeInput("dt_rng",
      label = h3("Pick date range"), 
      start = rngs[1], 
      end = rngs[2],
      startview = 'year'
    )
    
  })
  
  # plot
  output$simplot <- renderPlot({
    
    # inputs
    stat <- input$stat
    dt_rng <- input$dt_rng
    wins <- list(input$day_num, input$year, input$sal)
    refdt <- input$refdt
    
    # get color vector as parsed text string
    col_vec <- input$col_vec
    col_vec <- try(eval(parse(text = col_vec)), silent = T)
    if('try-error' %in% class(col_vec)) col_vec <- input$col_vec
    
    # format data
    dat <- pax_data[pax_data$STATION %in% stat, ]
    dat$STATION <- NULL
    dat <- tidal(na.omit(dat))
    
    wtsplot(dat, ref = refdt, wins = wins, dt_rng = dt_rng, col_vec = col_vec, alpha = 1)
 
    
    },height = 700, width = 700)

})