###
# wt reg window evals for patux

# salinity first, LE1.2

# library(WRTDStidal)
devtools::load_all("M:/docs/wtreg_for_estuaries")

data(pax_data)
data(optimLE12_opt)

# which variable to use for flow? salinity...
pax_data <- pax_data[, !names(pax_data) %in% 'lnQ']

stat <- c('LE1.2')

tomod <- pax_data[pax_data$STATION %in% stat, ]
row.names(tomod) <- 1:nrow(tomod)
tomod$limval <- 0
tomod$STATION <- NULL
tomod <- tomod[order(tomod$date), ]

bestLE12_wrtds <- modfit(tomod, resp_type = 'mean', wins = as.list(optimLE12_opt$par), 
  min_obs = FALSE, sal_div = 50)
save(bestLE12_wrtds, file = 'data/bestLE12_wrtds.RData')
save(bestLE12_wrtds, file = 'M:/docs/manuscripts/patux_manu/data/bestLE12_wrtds.RData')

###
# flow second, TF16

data(pax_data)
data(optimTF16_opt)

# which variable to use for flow?
pax_data <- pax_data[, !names(pax_data) %in% 'sal']
names(pax_data)[names(pax_data) %in% 'lnQ'] <- 'sal'

stat <- c('TF1.6')

tomod <- pax_data[pax_data$STATION %in% stat, ]
row.names(tomod) <- 1:nrow(tomod)
tomod$limval <- 0
tomod$STATION <- NULL
tomod <- tomod[order(tomod$date), ]

bestTF16_wrtds <- modfit(tomod, resp_type = 'mean', wins = as.list(optimTF16_opt$par), min_obs = FALSE, 
  sal_div = 50)
save(bestTF16_wrtds, file = 'data/bestTF16_wrtds.RData')
save(bestTF16_wrtds, file = 'M:/docs/manuscripts/patux_manu/data/bestTF16_wrtds.RData')

######
# get GAM results from Rebecca

rm(list = ls())

library(readxl)
library(dplyr)
library(data.table)

###
# tf16 

tf16_prd <- read_excel('ignore/GAM_Patuxent_2015-08-11.xlsx', sheet = 'TF16predict', skip= 1) %>% 
  mutate(dates = as.Date(dates))
tf16_nrm <- read_excel('ignore/GAM_Patuxent_2015-08-11.xlsx', sheet = 'TF16FlowNorm', skip= 1) %>%
  mutate(dates = as.Date(dates))

# gam norm are one per month, merge by nereast date in preds, norms will be duplicated for some months
bestTF16_gams <- data.table::data.table(tf16_prd, key = 'dates')
mrgs <- data.table::data.table(tf16_nrm, key = 'dates')
bestTF16_gams <- as.data.frame(mrgs[bestTF16_gams, roll = 'nearest'])
names(bestTF16_gams) <- c('date', 'norm', 'dec_time', 'fits', 'se')
bestTF16_gams <- bestTF16_gams[, c(1, 4, 2, 5)]

save(bestTF16_gams, file = 'data/bestTF16_gams.RData')
save(bestTF16_gams, file = 'M:/docs/manuscripts/patux_manu/data/bestTF16_gams.RData')

###
# le12

le12_prd <- read_excel('ignore/GAM_Patuxent_2015-08-11.xlsx', sheet = 'LE12predict', skip= 1) %>% 
  mutate(dates = as.Date(dates))
le12_nrm <- read_excel('ignore/GAM_Patuxent_2015-08-11.xlsx', sheet = 'LE12FlowNorm', skip= 1) %>%
  mutate(dates = as.Date(dates))

# gam norm are one per month, merge by nereast date in preds, norms will be duplicated for some months
bestLE12_gams <- data.table::data.table(le12_prd, key = 'dates')
mrgs <- data.table::data.table(le12_nrm, key = 'dates')
bestLE12_gams <- as.data.frame(mrgs[bestLE12_gams, roll = 'nearest'])
names(bestLE12_gams) <- c('date', 'norm', 'dec_time', 'fits', 'se')
bestLE12_gams <- bestLE12_gams[, c(1, 4, 2, 5)]

save(bestLE12_gams, file = 'data/bestLE12_gams.RData')
save(bestLE12_gams, file = 'M:/docs/manuscripts/patux_manu/data/bestLE12_gams.RData')

######
# create combined gams/wrtds data for each station
rm(list = ls())

# import mods
data(bestLE12_gams)
data(bestLE12_wrtds)
data(bestTF16_gams)
data(bestTF16_wrtds)

# import pax to add flow, sal to data objects that did not use either for model fits
data(pax_data)
flowsalTF16 <- filter(pax_data, STATION %in% 'TF1.6') %>% 
  select(date, sal)
flowsalLE12 <- filter(pax_data, STATION %in% 'LE1.2') %>% 
  select(date, lnQ)

bestLE12 <- data.frame(bestLE12_wrtds) %>% 
  select(date, dec_time, chla, sal, fits, norm) %>% 
  rename(
    fits_wrtds = fits, 
    norm_wrtds = norm
  ) %>% 
  left_join(., bestLE12_gams, by = 'date') %>% 
  rename(
    fits_gams = fits, 
    norm_gams = norm, 
    se_gams = se
  ) %>% 
  mutate(
    res_wrtds = chla - fits_wrtds,
    res_gams = chla - fits_gams
  ) %>% 
  left_join(., flowsalLE12, by = 'date') %>% 
  mutate(
    mo = as.numeric(strftime(date, '%m')), 
    yr = as.numeric(strftime(date, '%Y')), 
    flcat = cut(lnQ, breaks = c(-Inf, quantile(lnQ, c(0.25, 0.5, 0.75)), Inf), labels = c('Flow 1 (Low)', 'Flow 2', 'Flow 3', 'Flow 4 (High)')), 
    mocat = cut(mo, breaks = c(-Inf, 3, 6, 9, Inf), labels = c('JFM', 'AMJ', 'JAS', 'OND')), 
    yrcat = cut(yr, breaks = c(-Inf, 1993, 2000, 2007, Inf), 
      labels = c('1986-1993', '1994-2000', '2001-2007', '2008-2014'))
  ) %>% 
  select(-mo, -yr) 

save(bestLE12, file = 'data/bestLE12.RData')
save(bestLE12, file = 'M:/docs/manuscripts/patux_manu/data/bestLE12.RData')

bestTF16 <- data.frame(bestTF16_wrtds) %>% 
  select(date, dec_time, chla, sal, fits, norm) %>% 
  rename(
    fits_wrtds = fits, 
    norm_wrtds = norm,
    lnQ = sal
  ) %>% 
  left_join(., bestTF16_gams, by = 'date') %>% 
  rename(
    fits_gams = fits, 
    norm_gams = norm, 
    se_gams = se
  ) %>% 
  mutate(
    res_wrtds = chla - fits_wrtds,
    res_gams = chla - fits_gams
  ) %>% 
  left_join(., flowsalTF16, by = 'date') %>% 
  mutate(
    mo = as.numeric(strftime(date, '%m')), 
    yr = as.numeric(strftime(date, '%Y')), 
    flcat = cut(lnQ, breaks = c(-Inf, quantile(lnQ, c(0.25, 0.5, 0.75)), Inf), labels = c('Flow 1 (Low)', 'Flow 2', 'Flow 3', 'Flow 4 (High)')), 
    mocat = cut(mo, breaks = c(-Inf, 3, 6, 9, Inf), labels = c('JFM', 'AMJ', 'JAS', 'OND')), 
    yrcat = cut(yr, breaks = c(-Inf, 1993, 2000, 2007, Inf), 
      labels = c('1986-1993', '1994-2000', '2001-2007', '2008-2014'))
  ) %>% 
  select(-mo, -yr) 

save(bestTF16, file = 'data/bestTF16.RData')
save(bestTF16, file = 'M:/docs/manuscripts/patux_manu/data/bestTF16.RData')