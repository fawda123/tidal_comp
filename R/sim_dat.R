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

#######
# create simulated chlorophyll datasets with known bio and flow components
# daydat is the same as obs_dat, just ported over to WRTDStidal

data(daydat)
  
set.seed(123)

# scenarios
# constant influence
# no influence
# steady increase
# steady decrease

## get simulated discharge
sims <- lnQ_sim(daydat)
sims <- lnchla_err(sims)

tomod <- lnchla_sim(sims)
names(tomod)[names(tomod) %in% 'lnchla_Q'] <- 'sim1' # constant effect
tomod$sim2 <- lnchla_sim(sims, lnQ_coef = rep(0, nrow(daydat)))$lnchla_Q # no effect
tomod$sim3 <- lnchla_sim(sims, lnQ_coef = seq(0, 1, length = nrow(daydat)))$lnchla_Q # increasing effect
tomod$sim4 <- lnchla_sim(sims, lnQ_coef = seq(1, 0, length = nrow(daydat)))$lnchla_Q # decreasing effect
samped <- samp_sim(tomod)

# the daily ts
toplo <- reshape2::melt(tomod, id.vars = 'date', 
  measure.vars = c('lnchla', 'lnchla_noQ', 'sim1', 'sim2', 'sim3', 'sim4'))

p1 <- ggplot(toplo, aes(x = date, y = value, group = variable)) + 
  geom_line() + 
  theme_bw() + 
  facet_wrap(~variable)

# the sampled ts
toplo2 <- reshape2::melt(samped, id.vars = 'date', 
  measure.vars = c('lnchla', 'lnchla_noQ', 'sim1', 'sim2', 'sim3', 'sim4'))

p2 <- ggplot(toplo2, aes(x = date, y = value, group = variable)) + 
  geom_line() + 
  theme_bw() + 
  facet_wrap(~variable)

grid.arrange(p1, p2, ncol = 1)

##
# try some mods

testmod <- select(samped, date, sim3, lnQ_sim)
names(testmod) <- c('date', 'chla', 'sal')
testmod$lim <- 1e-6

#### create one model
eval <- modfit(testmod, resp_type = 'mean', wins = list(3, 1, 3))

plot(chla ~ fits, eval)
plot(eval$norm, na.omit(samped$lnchla_noQ))

dynaplot(eval)
