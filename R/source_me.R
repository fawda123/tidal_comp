# evaluation of predictive performance of GAMs/WRTDS with simulated data

library(tidyverse)
library(WRTDStidal)
library(mgcv)
library(foreach)
library(doParallel)

# parallel setup
cl <- makeCluster(4)
registerDoParallel(cl)

source('text/R/funcs.R')

# simulated daily data
data(sims_day)

# format the daily data for weekly sampling and use with WRTDS optim
tomod <- select(sims_day, date, lnQ_sim, sim3) %>% 
  rename(
    res = sim3,
    flo = lnQ_sim
    ) %>% 
  mutate(
    lim = -1e6
  ) %>% 
  samp_sim(unit = 'week', irregular = F) %>% 
  tidalmean(
    reslab = expression(paste('ln-Chl-',italic(a),' (',italic('\u03bc'),'g ',L^-1,')')),
    flolab = expression(paste('ln-Flow (', m^3, ' ', s^-1, ')'))
  )

# run optimization
wk_opt <- winsrch_optim(tomod, upper = c(20, 100, 20), lower = c(0.25, 1, 0.25), 
  min_obs = FALSE, control = list(factr = 1e11))

save(wk_opt, file = 'data/wk_opt.RData', compress = 'xz')