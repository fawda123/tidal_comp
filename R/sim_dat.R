# create simulated datasets
library(dplyr)
library(ggplot2)
library(WRTDStidal)
# devtools::load_all('M:/docs/wtreg_for_estuaries')

# # flow data, cfs converted to log m3/s
# pax_flow <- read.csv('inst/PatuxentDailyFlow_84to15.csv', 
#   header = TRUE, stringsAsFactors = FALSE)
# pax_flow <- mutate(pax_flow, 
#   date = as.Date(date, format = '%m/%d/%Y'),
#   Q = flow_cfs * 0.0283168,
#   lnQ = log(Q),
#   date = as.Date(date, '%Y-%m-%d'), 
#   jday = as.numeric(strftime(date, '%j')),
#   year = as.numeric(strftime(date, '%Y')),
#   day = as.numeric(strftime(date, '%d')),
#   dec_time = year + (jday - 1)/365
#   ) %>% 
#   select(-flow_cfs) %>% 
#   filter(year > 1984 & year < 2015)
# 
# # jug bay chl data, all yrs of data combined, averaged by day
# fls <- list.files(path = 'ignore/jug_bay/', pattern = '^mddnr', full.names = TRUE)
# pax_chl <- vector('list', length(fls))
# names(pax_chl) <- fls
# for(fl in fls){
#   dat <- read.csv(fl, header = T)
#   pax_chl[[fl]] <- dat
# }
# pax_chl <- do.call('rbind', pax_chl)
# pax_chl <- pax_chl[, grepl('^Chl|^Sal|^Date\\.Time', names(pax_chl))]
# row.names(pax_chl) <- 1:nrow(pax_chl)
# names(pax_chl) <- c('DateTimeStamp', 'sal', 'chla')
# pax_chl$DateTimeStamp <- as.POSIXct(as.character(pax_chl$DateTimeStamp), 
#   format = '%d-%b-%Y %H:%M:%S', tz = 'America/Atikokan')
# pax_chl$date <- as.Date(pax_chl$DateTimeStamp)
# pax_chl <- aggregate(cbind(chla, sal) ~ date, data = pax_chl, function(x) mean(x, na.rm = T))
# pax_chl$lnchla <- log(pax_chl$chla)
# pax_chl$chla <- NULL
# 
# # merge chla with flow, 2004-2014
# obs_dat <- merge(pax_chl, pax_flow, by = 'date')
# obs_dat <- filter(obs_dat, year > 2004)
# save(obs_dat, file = 'data/obs_dat.RData')

#######
# assume that chlorophyll follows functional forum in wrtds 
# find error structures of chlorophyll residuals using observed data
# create sims using sim Q and sim error structure

data(obs_dat)

# flow mod
# requires lnQ and decimal time
lnQ_sim <- function(dat_in){

  lnQ <- dat_in$lnQ
  dec_time <- dat_in$dec_time
  
  # stationary seasonal model
  seas_mod <- lm(lnQ ~ sin(2 * pi * dec_time) + cos(2 * pi * dec_time))
  seas_fit <- fitted(seas_mod)
  seas_res <- resid(seas_mod)

  # get arma coefficients of resids
  mod <- forecast::auto.arima(seas_res, d = 0, seasonal = FALSE)
  ars <- coef(mod)[grep('^ar', names(coef(mod)))]
  mas <- coef(mod)[grep('^ma', names(coef(mod)))]

  # simulate rnorm errors using arma(p,q) process 
  errs <- arima.sim(list(ar = ars, ma = mas, order = c(length(ars), 0, length(mas))), n = nrow(dat_in), 
    rand.gen = function(x) rnorm(x, 0, 1))

  # simulated data, linear trans to range of discharge
  sim_out <- as.numeric({seas_fit +  sd(seas_res) * errs})
  rng <- range(lnQ, na.rm = T)
  sim_out <- scales::rescale(sim_out, to = rng)
  
  dat_in$lnQ_sim <- sim_out
  return(dat_in)
  
}

lnchla_err <- function(dat_in, yr = NULL) {
  
  # pick yr for stationary mod, defaults to median
  if(is.null(yr))
    yr <- median(unique(dat_in$year)) %>% 
      round(., 0)
    
  # data for the year to get stationary mod
  stat_dat <- filter(dat_in, year == yr)
  
  # use wrtds for the year
  tomod <- select(stat_dat, date, lnchla, lnQ) %>% 
    rename(
      chla = lnchla,
      sal = lnQ
    ) %>% 
    mutate(lim = -1e6)
  chlmod <- modfit(tomod, resp_type = 'mean')
  chlmod <- chlscls(chlmod)
  
  # get model residuals, scale parameter, and decimal time minus year
  stat_toerr <- with(chlmod,
    data.frame(
      dec_mo = stat_dat$dec_time - stat_dat$year,
      res = chla - fits,
      scls = scls
      )
    )

  # get arma model from resids
  errmod <- forecast::auto.arima(stat_toerr$res, d = 0, seasonal = FALSE)
  ars <- coef(errmod)[grep('^ar', names(coef(errmod)))]
  mas <- coef(errmod)[grep('^ma', names(coef(errmod)))]
  
  # simulate rnorm errors using arma(p,q) process 
  errs <- arima.sim(list(ar = ars, ma = mas, order = c(length(ars), 0, length(mas))), n = nrow(dat_in), 
    rand.gen = function(x) rnorm(x, 0, 1))
  
  # add errors and scl values to dat_in
  dat_in$errs <- as.numeric(errs)
  dat_in$dec_mo <- with(dat_in, dec_time - year)
  dat_in <- left_join(dat_in, stat_toerr, by = 'dec_mo')
  
  # linear transform errrs to match those in the from the observed data
  rng <- range(dat_in$res, na.rm = T)
  errs <- scales::rescale(errs, to = rng)
  
  # remove extra cols, sort on date
  dat_in <- select(dat_in, date, sal, lnchla, Q, lnQ, jday, year, day, dec_time, scls, errs, lnQ_sim) %>% 
    arrange(date)
  
  return(dat_in)
  
}

lnchla_sim <- function(dat_in, lnQ_coef = NULL){

  if(!'errs' %in% names(dat_in)) 
    stop('Need error simulation from chlorophyll residuals')
  
  if(!'lnQ_sim' %in% names(dat_in)) 
    stop('Need simulated flow data')
  
  if(is.null(lnQ_coef)) lnQ_coef <- rep(1, length = nrow(dat_in))
  
  # seasonal chla component, no discharge
  lnchla_noQ <- lm(lnchla ~ dec_time + sin(2 * pi * dec_time) + cos(2 * pi * dec_time), 
    data = dat_in)
  lnchla_noQ <- predict(lnchla_noQ) + with(dat_in, scls * errs)
  
  # add discharge, rescale 
  lnchla_Q <- lnchla_noQ + with(dat_in, lnQ_coef * scale(lnQ_sim, scale = FALSE))
  
  dat_in$lnchla_noQ <- lnchla_noQ
  dat_in$lnchla_Q <- lnchla_Q
  
  return(dat_in)
  
}

all_sims <- function(dat_in, ...){

  out <- lnQ_sim(dat_in) %>% 
    lnchla_err %>% 
    lnchla_sim(., ...)
 
  return(out)
 
}

# sample the simulated dataset with a random selection by month
samp_sim <- function(dat_in, month_samps = 1){
 
  dat_in$mos <- strftime(dat_in$date, '%m')

  splits <- split(dat_in, dat_in[, c('year', 'mos')])
  
  sels <- lapply(splits, function(x){
    tosel <- sample(1:nrow(x), size = month_samps)
    x[tosel, ]
  })

  sels <- do.call('rbind', sels)
  sels <- sels[order(sels$date), ]
  row.names(sels) <- 1:nrow(sels)
  sels$mos <- NULL

  return(sels)
   
}
  
set.seed(123)
coefs <- dnorm(seq(-7, 7, length = nrow(obs_dat)))
coefs <- scales::rescale(coefs, c(0, 6))
toeval <- all_sims(obs_dat, lnQ_coef = coefs)
toeval2 <- samp_sim(toeval, month_samps = 3)

# toplo <- reshape2::melt(toeval, id.vars = 'date', 
#   measure.vars = c('lnQ_sim', 'lnchla', 'lnchla_noQ', 'lnchla_Q'))
# 
# p1 <- ggplot(toplo, aes(x = date, y = value, group = variable, colour = variable)) + 
#   geom_line() + 
#   theme_bw() + 
#   facet_wrap(~variable)
# 
# toplo2 <- reshape2::melt(toeval2, id.vars = 'date', 
#   measure.vars = c('lnQ_sim', 'lnchla', 'lnchla_noQ', 'lnchla_Q'))
# 
# p2 <- ggplot(toplo2, aes(x = date, y = value, group = variable, colour = variable)) + 
#   geom_line() + 
#   theme_bw() + 
#   facet_wrap(~variable)
# 
# grid.arrange(p1, p2, ncol = 1)

##
# try some mods

tomod <- select(toeval2, date, lnchla_Q, lnQ_sim)
names(tomod) <- c('date', 'chla', 'sal')
tomod$lim <- 1e-6

# library(doParallel)
# ncores <- detectCores() - 4
# registerDoParallel(cores = ncores)

# run search function - takes a while
tosrch <- tidalmean(tomod)
res <- winsrch_optim(tosrch)

#### create one model
eval <- modfit(tomod, resp_type = 'mean', wins = list(0.2, 3, 0.2))

plot(chla ~ fits, eval)
plot(eval$norm, na.omit(toeval2$lnchla_noQ))

