# create simulated datasets
library(dplyr)
library(ggplot2)
library(gridExtra)
# library(WRTDStidal)
devtools::load_all('M:/docs/wtreg_for_estuaries')

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
# 
#######
# create simulated chlorophyll datasets with known bio and flow components
# daydat is the same as obs_dat, just ported over to WRTDStidal

data(daydat)
  
set.seed(123)

# scenarios
# constant influence
# no influence
# steady increase

## get simulated discharge
sims_day <- lnQ_sim(daydat)
sims_day <- lnchla_err(sims_day)

sims_day <- lnchla_sim(sims_day)
names(sims_day)[names(sims_day) %in% 'lnchla_Q'] <- 'sim1' # constant effect
sims_day$sim2 <- lnchla_sim(sims_day, lnQ_coef = rep(0, nrow(daydat)))$lnchla_Q # no effect
sims_day$sim3 <- lnchla_sim(sims_day, lnQ_coef = seq(0, 1, length = nrow(daydat)))$lnchla_Q # increasing effect
sims_mos <- samp_sim(sims_day)

# remove extra columns
sims_mos <- select(sims_mos, -dec_time, -sal, -lnchla, -Q, -lnQ, -jday, -year, -day, -errs, -scls)
sims_day <- select(sims_day, -dec_time, -sal, -lnchla, -Q, -lnQ, -jday, -year, -day, -errs, -scls)
save(sims_day, file = 'data/sims_day.RData')
save(sims_mos, file = 'data/sims_mos.RData')
save(sims_day, file = 'M:/docs/manuscripts/patux_manu/data/sims_day.RData')
save(sims_mos, file = 'M:/docs/manuscripts/patux_manu/data/sims_mos.RData')
