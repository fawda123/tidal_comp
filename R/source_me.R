######
# evaluation of predictive performance of GAMs/WRTDS with simulated data

library(tidyverse)
library(WRTDStidal)
library(mgcv)
library(foreach)
library(doParallel)
source('text/R/funcs.R')

# ######
# # find optimum window widths for WRTDS for whole weekly time series 
# 
# # parallel setup
# cl <- makeCluster(4)
# registerDoParallel(cl)
# 
# # simulated daily data
# data(sims_day)
# 
# # format the daily data for weekly sampling and use with WRTDS optim
# tomod <- select(sims_day, date, lnQ_sim, sim1) %>% 
#   rename(
#     res = sim1,
#     flo = lnQ_sim
#     ) %>% 
#   mutate(
#     lim = -1e6
#   ) %>% 
#   samp_sim(unit = 'week', irregular = F) %>% 
#   tidalmean(
#     reslab = expression(paste('ln-Chl-',italic(a),' (',italic('\u03bc'),'g ',L^-1,')')),
#     flolab = expression(paste('ln-Flow (', m^3, ' ', s^-1, ')'))
#   )
# 
# # run optimization
# wk_opt <- winsrch_optim(tomod, upper = c(20, 100, 20), lower = c(0.25, 1, 0.25), 
#   min_obs = FALSE, control = list(factr = 1e11))
# 
# # format the daily data for monthly sampling and use with WRTDS optim
# tomod <- select(sims_day, date, sim1, lnQ_sim) %>% 
#   rename(
#     res = sim1,
#     flo = lnQ_sim
#     ) %>% 
#   mutate(
#     lim = -1e6
#   ) %>% 
#   samp_sim(unit = 'month', irregular = F) %>% 
#   tidalmean(
#     reslab = expression(paste('ln-Chl-',italic(a),' (',italic('\u03bc'),'g ',L^-1,')')),
#     flolab = expression(paste('ln-Flow (', m^3, ' ', s^-1, ')'))
#   )
# 
# # run optimization
# mo_opt <- winsrch_optim(tomod, upper = c(20, 100, 20), lower = c(0.25, 1, 0.25), 
#   min_obs = FALSE, control = list(factr = 1e11))
# 
# # combine week optimization and month optimization
# # load(file = 'data/wk_opt.RData')
# val_opt <- list(wk_opt = wk_opt, mo_opt = mo_opt)
# 
# save(val_opt, file = 'data/val_opt.RData', compress = 'xz')

######
# evaluation grid
# used both for GAMs and WRTDS

unts <- c('week')
mper <- seq(0.05, 0.5, by = 0.05)
reps <- 1:1000
blck <- c(1, c(0.1, 0.5, 1))
blckper <- c(F, T, T, T) # must be same length as blck
grd <- expand.grid(blck, unts, mper, reps)
grd <- data.frame(grd, blckper)
names(grd) <- c('blck', 'unts', 'mper', 'reps', 'blckper')

######
# get WRTDS validation error

# window widths data
data(val_opt)
wk_opt <- val_opt$wk_opt

# simulated daily data
data(sims_day)

# format the daily data for use with WRTDS
tosamp <- select(sims_day, date, sim1, lnQ_sim) %>% 
  rename(
    res = sim1,
    flo = lnQ_sim
    ) %>% 
  mutate(
    lim = -1e6
  )

# setup parallel
cl <- makeCluster(8)
registerDoParallel(cl)
strt <- Sys.time()

# process
wrtds_val <- foreach(val = 1:nrow(grd)) %dopar% {   
  
  library(dplyr)
  library(WRTDStidal)
  
  sink('C:/Users/mbeck/Desktop/log.txt')
  cat(val, 'of', nrow(grd), '\n')
  print(Sys.time()-strt)
  sink()
  
  # get the conditions in grd
  unit <- grd[val, 'unts']
  missper <- grd[val, 'mper']
  blck <- grd[val, 'blck']
  blckper <- grd[val, 'blckper']

  # prep data for training, validation  
  samps <- samp_sim(tosamp, unit = unit, irregular = F, missper = missper, blck = blck, blckper = blckper)

  # training and validation datasets
  alldat <- samps$alldat
  trndat <- alldat[!1:nrow(alldat) %in% samps$smps, ]
  valdat <- alldat[samps$smps, ]

  # fit model, note the min_obs argument makes sure that window width expansions don't get stuck for small datasets
  mod <- tidalmean(trndat,
    reslab = expression(paste('ln-Chl-',italic(a),' (',italic('\u03bc'),'g ',L^-1,')')),
    flolab = expression(paste('ln-Flow (', m^3, ' ', s^-1, ')'))
    ) %>% 
    wrtds(wins = as.list(wk_opt$par), min_obs = F)
  
  # get training, validation predictions
  trn <- respred(mod)
  val <- respred(mod, dat_pred = valdat) %>% 
    select(., date, res.y, fits) %>% 
    filter(date %in% valdat$date)

  # get error (wrtdsperf does not work 
  errtrn <- wrtdsperf(trn)[, 'rmse']
  errval <- rmse.fun(val$res.y, val$fits)
  
  # return errors
  c(errtrn, errval)

}

save(wrtds_val, file = 'data/wrtds_val.RData', compress = 'xz')
  
######
# get GAMS validation error

# simulated daily data
data(sims_day)

# format the daily data for sub sampling and use with GAM
tosamp <- select(sims_day, date, sim1, lnQ_sim) %>% 
  rename(
    res = sim1,
    flo = lnQ_sim
    ) %>% 
  mutate(
    # lim = -1e6, 
    doy = as.numeric(strftime(date, '%j'))
  )
tosamp$dec_time <- dec_time(tosamp$date)$dec_time

# setup parallel
cl <- makeCluster(8)
registerDoParallel(cl)
strt <- Sys.time()

# process
gams_val <- foreach(val = 1:nrow(grd)) %dopar% {   
  
  library(dplyr)
  library(WRTDStidal)
  library(mgcv)
  
  sink('C:/Users/mbeck/Desktop/log.txt')
  cat(val, 'of', nrow(grd), '\n')
  print(Sys.time()-strt)
  sink()
  
  unit <- grd[val, 'unts']
  missper <- grd[val, 'mper']
  blck <- grd[val, 'blck']
  blckper <- grd[val, 'blckper']

  # prep data for training, validation  
  samps <- samp_sim(tosamp, unit = unit, irregular = F, missper = missper, blck = blck, blckper = blckper)

  # traving and validation datasets
  alldat <- samps$alldat
  trndat <- alldat[!1:nrow(alldat) %in% samps$smps, ]
  valdat <- alldat[samps$smps, ]

  # gam mod and predictions, rmse on validation  
  trnmod <- try({
    gam(res~te(dec_time, doy, flo, bs=c("tp","cc","tp")), k = c(5, 11, 5), data = trndat, knots=list(doy=c(1,366)))
    })
  if(inherits(trnmod, 'try-error')){
    
    errtrn <- errval <- NA
    
  } else {
    
    valprd <- predict(trnmod, newdata = valdat)
    errtrn <- rmse.fun(resid(trnmod))
    errval <- rmse.fun(valdat$res, valprd)
    
  }
  
  # output
  c(errtrn, errval)
  
}

save(gams_val, file = 'data/gams_val.RData', compress = 'xz')

