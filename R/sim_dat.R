# create simulated datasets
library(dplyr)

dat <- read.csv("C:/Users/mbeck/Desktop/jug_chl.csv")

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
  dec_time = year + (jday - 1)/365
  ) %>% 
  select(-flow_cfs) %>% 
  filter(year > 1984 & year < 2015)

# flow mod
# requires lnQ and decimal time
create_Q <- function(dat_in){

  lnQ <- dat_in$lnQ
  dec_time <- dat_in$dec_time
  
  # stationary seasonal model
  seas_mod <- lm(lnQ ~ sin(2 * pi * dec_time) + cos(2 * pi * dec_time))
  seas_fit <- fitted(seas_mod)
  seas_res <- rstudent(seas_mod)

  # get arma coefficients of resids
  mod <- forecast::auto.arima(seas_res, d = 0, seasonal = FALSE)
  ars <- coef(mod)[grep('^ar', names(coef(mod)))]
  mas <- coef(mod)[grep('^ma', names(coef(mod)))]

  # simulate rnorm errors using arma(p,q) process 
  errs <- arima.sim(list(ar = ars, ma = mas, order = c(2, 0, 2)), n = nrow(dat_in), 
    rand.gen = function(x) rnorm(x, 0, 1))

  # simulated data, linear trans to range of discharge
  sim_out <- seas_fit +  sd(seas_res) * errs
  rng <- range(lnQ, na.rm = T)
  sim_out <- scales::rescale(sim_out, to = rng)
  
  dat_in$lnQ_sim <- sim_out
  return(dat_in)
  
}

# create chl


pax_flow <- create_Q(pax_flow)
toplo <- pax_flow
par(mar = c(4, 4, 0.5, 0.5), mfrow= c(2, 1))
plot(lnQ ~ date, data = toplo, type = 'l')
plot(lnQ_sim ~ date, data = toplo, type = 'l')

