# packages to use
library(WRTDStidal)
library(survival)
library(XML)
# devtools::load_all('M:/docs/wtreg_for_estuaries')

# get names of files on server
files_s3 <- httr::GET('https://s3.amazonaws.com/patuxmean/')$content
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
  dat <- eventReactive(input$getdat, {

    stat <- input$stat
    exp <- 'Salinity'
    if(stat == 'TF1.6') exp <- 'Flow'
    exprm <- 'lnQ'
    if(exp != 'Salinity') exprm <- 'sal'
  
    out <- pax_data[pax_data$STATION %in% stat, ]
    
    ## make the tidal object
    out$STATION <- NULL
    out <- out[, !names(out) %in% exprm]

    out <- list(dat = tidal(na.omit(out)), exp = exp)
    
    return(out)
    
  })
  
  # modelled data
  ests <- eventReactive(input$getdat, {

    # get station
    stat <- input$stat
    exp <- 'Salinity'
    if(stat == 'TF1.6') exp <- 'Flow'
    expfl <- 'sal'
    if(exp == 'Flow') expfl <- 'flo'
    
    # get windows
    wins <- paste(input$day_num, input$year, input$sal, sep = '_')
    
    # object and file names
    nm <- paste0(stat, expfl, '_', wins)
    fl <- paste0(nm, '.RData')

    # upload
    raw_content <- paste0('https://s3.amazonaws.com/patuxmean/', fl)
    raw_content <- httr::GET(raw_content)$content
    connect <- rawConnection(raw_content)
    load(connect)
    out <- get(nm)
    rm(list = nm)
    close(connect) 

    return(list(dat = out, exp = exp))
    
  })
  
  ## reactive ui controls
  
  # for initial window center
  output$refdate <- renderUI({

    refs <- as.Date(mean(dat()$dat[, 'date']), format = '%Y-%m-%d', 
      origin = '1970-01-01')
    
    dateInput('refdt', 
      label = h4("Pick reference date"),
      value = refs,
      startview = 'year'
      )
  
  })
  
  # for initial date range
  output$daterng <- renderUI({

    rngs <- range(dat()$dat[, 'date'])
    
    dateRangeInput("dt_rng",
      label = h4("Pick date range"), 
      start = rngs[1], 
      end = rngs[2],
      startview = 'year'
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

    # sort out names for facets to pass to ggplot
    facet_names <- list(
      'chl'= chllab(logspace),
      'sal' = dat()$exp
    )
    names(facet_names)[1] <- grep('chla$', names(dat()$dat), value = TRUE)
        
    facet_labeller <- function(variable,value){
      return(facet_names[value])
    }

    # get color vector as parsed text string
    p <- obsplot(dat()$dat, lines = lines, dt_rng = dt_rng, logspace = logspace, 
      alpha = 0.6, size = 4, lwd = 0.7)
    p <- p + facet_grid(variable ~ .,  labeller = facet_labeller)
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
    
    wtsplot(dat()$dat, ref = refdt, wins = wins, dt_rng = dt_rng, col_vec = col_vec, alpha = 0.8,
      min_obs = FALSE, ylabs = c(dat()$exp, 'Weights'))
 
    },height = 700, width = 700)
  
  
  # predictions and flow norms plot
  output$fitplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    
    # get model output
    modout <- TRUE
    if(input$modout == 'normalized') modout <- FALSE
    
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
    fitplot(ests()$dat, annuals = annuals, predicted = modout, col_vec = col_vec, logspace = logspace, dt_rng = dt_rng, size = 3, alpha = 0.8)

    }, height = 350, width = 700)
  
  # predictions and norms plot
  output$fitmoplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    month <- eval(parse(text = input$fitmonth))
    
    # months

    # chlorophyll trans
    logspace <- FALSE
    if(input$logspace == 'log') logspace <- TRUE
    
    # get model output
    modout <- TRUE
    if(input$modout == 'normalized') modout <- FALSE
    
    # get color vector as parsed text string
    col_vec <- input$col_vec
    col_vec <- try(eval(parse(text = col_vec)), silent = TRUE)
    if('try-error' %in% class(col_vec)) col_vec <- input$col_vec
    
    # create plot
    fitmoplot(ests()$dat, month = month, col_vec = col_vec, predicted = modout, logspace = logspace, dt_rng = dt_rng, size = 3, alpha = 0.8, ncol = 3)

    }, height = 500, width = 700)
  
  # predictions and flow norms plot
  output$resplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng

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
    prdnrmplot(ests()$dat, annuals = annuals, logspace = logspace, col_vec = col_vec, dt_rng = dt_rng, size = 3, alpha = 0.8)

    },height = 350, width = 700)
  
  # table of performance metrics
  output$tableperf <- renderTable({
    
    # chlorophyll trans
    logspace <- FALSE
    if(input$logspace == 'log') logspace <- TRUE
    
    to_tab <- wrtdsperf(ests()$dat, logspace = logspace)
    to_tab
  
  }, include.rownames = F)
  
  
  # grid plot
  output$gridplot <- renderPlot({
    
    # inputs
    dt_rng <- input$dt_rng
    month <- input$month
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
      p <- gridplot(ests()$dat, month = month, logspace = logspace, years = dt_rng, 
        col_vec = col_vec, sal_fac = sal_fac)
      p + scale_y_continuous(ests()$exp)
    } else {
      p <- dynaplot(ests()$dat, month = month, logspace = logspace, years = dt_rng, 
        col_vec = col_vec)
      p + scale_x_continuous(ests()$exp)
    }

    # get appropriate y axis title
 
    },height = 500, width = 700)
  

})