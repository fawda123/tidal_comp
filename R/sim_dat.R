# create simulated datasets
library(dplyr)
library(ggplot2)
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
# assume that chlorophyll follows functional forum in wrtds 
# find error structures of chlorophyll residuals using observed data
# create sims using sim Q and sim error structure

data(obs_dat)
  
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
res <- winsrch_optim(tosrch, upper = c(20, 100, 20), lower = c(0.25, 1, 0.25), 
  min_obs = FALSE, control = list(factr = 1e11))
res2 <- winsrch_constrOptim(tosrch, upper = c(20, 100, 20), lower = c(0.25, 1, 0.25), 
  min_obs = FALSE, control = list(factr = 1e11))
save(res, file = 'C:/Users/mbeck/Desktop/sim_res.RData')
save(res2, file = 'C:/Users/mbeck/Desktop/sim_res2.RData')

#### create one model
eval <- modfit(tomod, resp_type = 'mean', wins = list(0.2, 3, 0.2))

plot(chla ~ fits, eval)
plot(eval$norm, na.omit(toeval2$lnchla_noQ))

