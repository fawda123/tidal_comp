# packages to use
library(WRTDStidal)
library(survival)
library(XML)
# devtools::load_all('M:/docs/wtreg_for_estuaries')

# get names of files on server
files_s3 <- httr::GET('https://s3.amazonaws.com/patux/')$content
files_s3 <- rawToChar(files_s3)
files_s3 <- htmlTreeParse(files_s3, useInternalNodes = T)
files_s3 <- xpathSApply(files_s3, '//contents//key', xmlValue)

# raw data
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
    
    # get windows
    wins <- paste(input$day_num, input$year, input$sal, sep = '_')
    
    # object and file names
    nm <- paste(stat, wins, sep = '_')
    fl <- paste0(nm, '.RData')

    # upload
    raw_content <- paste0('https://s3.amazonaws.com/patux/', fl)
    raw_content <- httr::GET(raw_content)$content
    connect <- rawConnection(raw_content)
    load(connect)
    out <- get(nm)
    rm(list = nm)
    close(connect) 

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
  
  # for quantile selections, preds/norms
  output$taubox <- renderUI({

    tau <- gsub('^fit', '', names(attr(ests(), 'fits')))

    checkboxGroupInput('taubox', 
      label = h4('Quantile'),
      choices = tau,
      selected = tau,
      inline = TRUE
      )
  
  })
  
  # for quantile selections, gridplot
  output$tauinp <- renderUI({

    tau <- gsub('^fit', '', names(attr(ests(), 'fits')))
    tau_sel <- tau[round(length(tau)/2)]

    selectInput('tauinp', 
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
    
    wtsplot(dat(), ref = refdt, wins = wins, dt_rng = dt_rng, col_vec = col_vec, alpha = 0.8, min_obs = FALSE)
 
    },height = 700, width = 700)
  
  
  # predictions and flow norms plot
  output$fitplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    tau <- as.numeric(input$taubox)
    
    # get model output
    modout <- TRUE
    if(input$modout == 'flow-normalized') modout <- FALSE
    
    # aggregation period
    annuals <- TRUE
    if(input$annuals == 'monthly') annuals <- FALSE
    
    # chlorophyll trans
    logspace <- FALSE
    if(input$logspace == 'log') logspace <- TRUE
    
    # get color vector as parsed text string
    col_vec <- input$col_vec
    col_vec <- try(eval(parse(text = col_vec)), silent = TRUE)
    if('try-error' %in% class(col_vec)) col_vec <- input$col_vec
    
    # create plot
    fitplot(ests(), annuals = annuals, predicted = modout, col_vec = col_vec, tau = tau, logspace = logspace, dt_rng = dt_rng, size = 3, alpha = 0.8)

    }, height = 350, width = 700)
  
  # predictions and flow norms plot
  output$fitmoplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    tau <- as.numeric(input$taubox)
    month <- eval(parse(text = input$fitmonth))
    
    # months

    # chlorophyll trans
    logspace <- FALSE
    if(input$logspace == 'log') logspace <- TRUE
    
    # get model output
    modout <- TRUE
    if(input$modout == 'flow-normalized') modout <- FALSE
    
    # get color vector as parsed text string
    col_vec <- input$col_vec
    col_vec <- try(eval(parse(text = col_vec)), silent = TRUE)
    if('try-error' %in% class(col_vec)) col_vec <- input$col_vec
    
    # create plot
    fitmoplot(ests(), month = month, col_vec = col_vec, tau = tau, predicted = modout, logspace = logspace, dt_rng = dt_rng, size = 3, alpha = 0.8, ncol = 3)

    }, height = 500, width = 700)
  
  # predictions and flow norms plot
  output$resplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    tau <- as.numeric(input$taubox)

    # aggregation period
    annuals <- TRUE
    if(input$annuals == 'monthly') annuals <- FALSE
    
    # get color vector as parsed text string
    col_vec <- input$col_vec
    col_vec <- try(eval(parse(text = col_vec)), silent = T)
    if('try-error' %in% class(col_vec)) col_vec <- input$col_vec
    
    # chlorophyll trans
    logspace <- FALSE
    if(input$logspace == 'log') logspace <- TRUE
    
    # create plot
    prdnrmplot(ests(), annuals = annuals, logspace = logspace, tau = tau, col_vec = col_vec, dt_rng = dt_rng, size = 3, alpha = 0.8)

    },height = 350, width = 700)
  
  # grid plot
  output$gridplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    month <- input$month
    tau <- input$tauinp
    sal_fac <- input$sal_fac
    gridsorlines <- input$gridsorlines

    # format date range
    if(!is.null(dt_rng)){
      dt_rng <- as.numeric(strftime(dt_rng, '%Y'))
      dt_rng <- seq(dt_rng[1], dt_rng[2])
    }
    
    # months
    month <- eval(parse(text = month))
    
    # chlorophyll trans
    logspace <- FALSE
    if(input$logspace == 'log') logspace <- TRUE
    
    # get color vector as parsed text string
    col_vec <- input$col_vec
    col_vec <- try(eval(parse(text = col_vec)), silent = TRUE)
    if('try-error' %in% class(col_vec)) col_vec <- input$col_vec

    # create plot
    if(gridsorlines == 'grid'){
      gridplot(ests(), month = month, logspace = logspace, years = dt_rng, 
        col_vec = col_vec, tau = tau, sal_fac = sal_fac)
    } else {
      dynaplot(ests(), month = month, logspace = logspace, years = dt_rng, 
        col_vec = col_vec, tau = tau)
    }

    },height = 500, width = 700)
  
  # table of performance metrics
  output$tableperf <- renderTable({
    
    to_tab <- wrtdsperf(ests())
    to_tab <- to_tab[rev(1:nrow(to_tab)), ]
    
    to_tab
  
  }, include.rownames = F)
  

})