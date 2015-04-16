# packages to use
library(WRTDStidal)
# devtools::load_all('M:/docs/wtreg_for_estuaries')

# functions to use

# load data
load('LE13ests.RData')
load('TF16ests.RData')
mods <- list(LE1.3 = LE13ests, TF1.6 = TF16ests)
load('pax_data.RData')
pax_data$lim <- 0

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  ## reactive data based on inputs
  
  # raw data
  dat <- reactive({

    stat <- input$stat
    out <- pax_data[pax_data$STATION %in% stat, ]
    
    # make tidal object
    out$STATION <- NULL
    out <- tidal(na.omit(out))
    
    return(out)
    
  })
  
  # modelled data
  ests <- reactive({

    # get station
    stat <- input$stat
    
    out <- mods[[stat]]
    
    # find element that matches window selection
    wins <- paste(input$day_num, input$year, input$sal)
    out <- out[[which(wins == names(out))]]
    
    return(out)
    
  })
  
  ## reactive ui controls
  
  # for initial window center
  output$refdate <- renderUI({

    refs <- as.Date(mean(dat()$date), format = '%Y-%m-%d', 
      origin = '1970-01-01')
    
    dateInput('refdt', 
      label = h4("Pick reference date"),
      value = refs,
      startview = 'year'
      )
  
  })
  
  # for initial date range
  output$daterng <- renderUI({

    rngs <- range(dat()$date)
    
    dateRangeInput("dt_rng",
      label = h4("Pick date range"), 
      start = rngs[1], 
      end = rngs[2],
      startview = 'year'
    )
    
  })
  
  # for quantile selections
  output$tau <- renderUI({

    tau <- gsub('^fit', '', names(attr(ests(), 'fits')))
    tau_sel <- tau[round(length(tau)/2)]

    selectInput('tau', 
      label = h4('Quantile'),
      choices = tau,
      selected = tau_sel
      )
  
  })
  
  ## plots
  
  # observed data
  output$obsplot <- renderPlot({
    
    # inputs
    obstype <- input$obstype
    dt_rng <- input$dt_rng
    
    # points or lines
    lines <- TRUE
    if(obstype == 'points') lines <- FALSE
    
    # chlorophyll trans
    logspace <- FALSE
    if(input$logspace == 'log') logspace <- TRUE
    
    # get color vector as parsed text string
    p <- obsplot(dat(), lines = lines, dt_rng = dt_rng, logspace = logspace, 
      alpha = 0.6, size = 4, lwd = 0.7)
    p
 
    },height = 400, width = 700)
  
  # weights plot
  output$wtsplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    wins <- list(input$day_num, input$year, input$sal)
    wins <- lapply(wins, as.numeric)
    refdt <- input$refdt
    
    # get color vector as parsed text string
    col_vec <- input$col_vec
    col_vec <- try(eval(parse(text = col_vec)), silent = T)
    if('try-error' %in% class(col_vec)) col_vec <- input$col_vec
    
    wtsplot(dat(), ref = refdt, wins = wins, dt_rng = dt_rng, col_vec = col_vec, alpha = 0.8)
 
    },height = 700, width = 700)
  
  # results plot
  output$resplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng

    # aggregation period
    annuals <- TRUE
    if(input$annuals == 'monthly') annuals <- FALSE
    
    # chlorophyll trans
    logspace <- FALSE
    if(input$logspace == 'log') logspace <- TRUE
    
    # create plot
    prdnrmplot(ests(), annuals = annuals, logspace = logspace, dt_rng = dt_rng)

    },height = 350, width = 700)
  
  # grid plot
  output$gridplot <- renderPlot({
    
    # inputs
    stat <- input$stat
    dt_rng <- input$dt_rng
    month <- input$month
    tau <- input$tau
    sal_fac <- input$sal_fac

    # format date range
    if(!is.null(dt_rng)){
      dt_rng <- as.numeric(strftime(dt_rng, '%Y'))
      dt_rng <- seq(dt_rng[1], dt_rng[2])
    }
    
    # chlorophyll trans
    logspace <- FALSE
    if(input$logspace == 'log') logspace <- TRUE
    
    # get color vector as parsed text string
    col_vec <- input$col_vec
    col_vec <- try(eval(parse(text = col_vec)), silent = TRUE)
    if('try-error' %in% class(col_vec)) col_vec <- input$col_vec

    # create plot
    gridplot(ests(), month = as.numeric(month), logspace = logspace, years = dt_rng, 
      col_vec = col_vec, tau = tau, sal_fac = sal_fac)

    },height = 350, width = 700)
  

})