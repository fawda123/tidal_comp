# create simulated datasets
library(dplyr)
library(ggplot2)
library(WRTDStidal)

# flow data, cfs converted to log m3/s
pax_flow <- read.csv('inst/PatuxentDailyFlow_84to15.csv', 
  header = TRUE, stringsAsFactors = FALSE)
pax_flow <- mutate(pax_flow, 
  date = as.Date(date, format = '%m/%d/%Y'),
  Q = flow_cfs * 0.0283168,
  lnQ = log(Q),
  date = as.Date(date, '%Y-%m-%d'), 
  jday = as.numeric(strftime(date, '%j')),
  year = as.numeric(strftime(date, '%Y')),
  day = as.numeric(strftime(date, '%d')),
  dec_time = year + (jday - 1)/365
  ) %>% 
  select(-flow_cfs) %>% 
  filter(year > 1984 & year < 2015)

# jug bay chl data, all yrs of data combined, averaged by day
fls <- list.files(path = 'ignore/jug_bay/', pattern = '^mddnr', full.names = TRUE)
pax_chl <- vector('list', length(fls))
names(pax_chl) <- fls
for(fl in fls){
  dat <- read.csv(fl, header = T)
  pax_chl[[fl]] <- dat
}
pax_chl <- do.call('rbind', pax_chl)
pax_chl <- pax_chl[, grepl('^Chl|^Sal|^Date\\.Time', names(pax_chl))]
row.names(pax_chl) <- 1:nrow(pax_chl)
names(pax_chl) <- c('DateTimeStamp', 'sal', 'chla')
pax_chl$DateTimeStamp <- as.POSIXct(as.character(pax_chl$DateTimeStamp), 
  format = '%d-%b-%Y %H:%M:%S', tz = 'America/Atikokan')
pax_chl$date <- as.Date(pax_chl$DateTimeStamp)
pax_chl <- aggregate(cbind(chla, sal) ~ date, data = pax_chl, function(x) mean(x, na.rm = T))
pax_chl$lnchla <- log(pax_chl$chla)
pax_chl$chla <- NULL

# merge chla with flow, 2004-2014
obs_dat <- merge(pax_chl, pax_flow, by = 'date')
obs_dat <- filter(obs_dat, year > 2004)

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

sim_dat <- lnQ_sim(obs_dat)

##
# create chl
lnchla_sim <- function(dat_in, yr = NULL){
  
  if(!'lnQ_sim' %in% names(dat_in))
    stop('No simulated discharge data, run lnQ_sim first')
  
  # pick yr for stationary mod, defaults to median
  if(is.null(yr))
    yr <- median(unique(dat_in$year)) %>% 
      round(., 0)
    
  # data for the year to get stationary mod
  stat_dat <- filter(dat_in, year == yr)
  
  # stationary mod using wrtds form
  chlmod <- lm(lnchla ~ dec_time + lnQ_sim + sin(2 * pi * dec_time) + cos(2 * pi * dec_time), 
    data = stat_dat)
  
  # get model predictions, merge by decimal month in original data
  dec_mo <- stat_dat$dec_time - stat_dat$year
  stat_pred <- data.frame(mu_lnchla = predict(chlmod), dec_mo = dec_mo)
  dat_in$dec_mo <- with(dat_in, dec_time - year)
  dat_in <- merge(dat_in, stat_pred, by = 'dec_mo', all.x = TRUE)
  
  # get lambda values by year
  dat_in$res <- with(dat_in, lnchla - mu_lnchla)
  lamb <- aggregate(res ~ year, dat_in, FUN = sd, na.rm = T)
  names(lamb)[names(lamb) %in% 'res'] <- 'lamb'
  dat_in <- merge(dat_in, lamb, by = 'year', all.x = TRUE)
  
  # get arma model from resids
  mod <- forecast::auto.arima(dat_in$res, d = 0, seasonal = FALSE)
  ars <- coef(mod)[grep('^ar', names(coef(mod)))]
  mas <- coef(mod)[grep('^ma', names(coef(mod)))]
  
  # simulate rnorm errors using arma(p,q) process 
  errs <- arima.sim(list(ar = ars, ma = mas, order = c(length(ars), 0, length(mas))), n = nrow(dat_in), 
    rand.gen = function(x) rnorm(x, 0, 1))
  
  # linear transform errrs to match those in the from the observed data
  rng <- range(dat_in$res, na.rm = T)
  errs <- scales::rescale(errs, to = rng)
  
  # simulated data as mean model plus errors x sd for each year
  sim_out <- with(dat_in, as.numeric({mu_lnchla +  lamb * errs}))

  # add to output  
  dat_in$lnchla_sim <- sim_out
  
  # remove extra cols, sort on date
  dat_in <- select(dat_in, date, sal, lnchla, Q, lnQ, jday, year, day, dec_time, lnQ_sim, lnchla_sim) %>% 
    arrange(date)
  
  return(dat_in)
  
}

sim_vals <- lnchla_sim(sim_dat)

toplo <- reshape2::melt(sim_vals, id.vars = 'date', 
  measure.vars = c('lnQ', 'lnQ_sim', 'lnchla', 'lnchla_sim'))

ggplot(toplo, aes(x = date, y = value, group = variable, colour = variable)) + 
  geom_line() + 
  theme_bw() + 
  facet_wrap(~variable)

##
# create sample dataset for monthly 
sim_mo <- sim_vals[sim_vals$day == c(23), ]

toplo <- reshape2::melt(sim_mo, id.vars = 'date', 
  measure.vars = c('lnQ', 'lnQ_sim', 'lnchla', 'lnchla_sim'))

ggplot(toplo, aes(x = date, y = value, group = variable, colour = variable)) + 
  geom_line() + 
  theme_bw() + 
  facet_wrap(~variable)

# run wrtds
sim_mo <- select(sim_mo, date, lnchla, lnQ)
sim_mo$lim <- -2
names(sim_mo) <- names(chldat)

mod <- modfit(sim_mo, wins = list(1, 5, 0.3), resp_type = 'mean')

prdnrmplot(mod)
