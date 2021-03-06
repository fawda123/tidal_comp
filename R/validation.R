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
reps <- 1:100
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
  trndat <- alldat[samps$smps, ]
  alldat$res[samps$smps] <- NA
  
  # fit model
  mod <- tidalmean(alldat,
    reslab = expression(paste('ln-Chl-',italic(a),' (',italic('\u03bc'),'g ',L^-1,')')),
    flolab = expression(paste('ln-Flow (', m^3, ' ', s^-1, ')'))
    ) %>% 
    wrtds(min_obs = F, flo_div = 10) %>% 
    respred
     
  # get training, validation predictions
  trn <- mod[!1:nrow(mod) %in% samps$smps, ] %>% 
    select(date, res, fits)
  val <- data.frame(trndat, fits = mod[samps$smps, 'fits']) %>% 
    select(-lim, - flo) %>% 
    mutate(dat = 'val')

  # get error (wrtdsperf does not work 
  errtrn <- rmse.fun(trn$res, trn$fits)
  errval <- rmse.fun(val$res, val$fits)
  
  # return errors
  c(errtrn, errval)

}

# combine with grd
wrtds_val <- do.call('rbind', wrtds_val) %>% 
  data.frame %>% 
  rename(
    rmse_trn = X1, 
    rmse_val = X2
  ) %>% 
  cbind(grd, .) %>% 
  data.frame(mod = 'wrtds', .)

save(wrtds_val, file = 'data/wrtds_val.RData', compress = 'xz')
save(wrtds_val, file = 'M:/docs/manuscripts/patux_manu/data/wrtds_val.RData', compress = 'xz')
  
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

# combine with grd
gams_val <- do.call('rbind', gams_val) %>% 
  data.frame %>% 
  rename(
    rmse_trn = X1, 
    rmse_val = X2
  ) %>% 
  cbind(grd, .) %>% 
  data.frame(mod = 'gams', .)

save(gams_val, file = 'data/gams_val.RData', compress = 'xz')
save(gams_val, file = 'M:/docs/manuscripts/patux_manu/data/gams_val.RData', compress = 'xz')
  
# ######
# # examples of simulated data - training and validation datasets
#   
# library(tidyverse)
# library(WRTDStidal)
# 
# # simulated daily data
# data(sims_day)
# 
# blcks <- c(1, seq(0.5, 1, by = 0.5))
# blckpers <- c(F, T, T)
# units <- c('yday', 'week', 'month')
# misspers <- c(0.1, 0.2)
# grd <- expand.grid(blcks, misspers, units)
# grd <- data.frame(grd, blckpers)
# names(grd) <- c('blcks', 'misspers', 'units', 'blckpers')
# 
# par(mfcol = c(6, 3), mar = c(2, 2, 2, 1))
# for(i in 1:nrow(grd)){
#  
#   unit <- grd[i, 'units']
#   blck <- grd[i, 'blcks']
#   blckper <- grd[i, 'blckpers']
#   missper <- grd[i, 'misspers']
#   
#   tmp <- samp_sim(sims_day, unit = unit, irregular = F, missper = missper, blck = blck, blckper = blckper)
# 
#   blcklab <- 'random'
#   msslab <- paste0(round(100 * missper), '% missing')
#   unitlab <- paste('sample by', unit)
#   if(blckper)
#     blcklab <- paste0('blocks ', round(100 * blck) , '% of missing')
#   
#   titleval <- paste(unitlab, msslab, blcklab, sep = ', ')
#   
#   plot(sim1 ~ date, tmp$alldat, xlab = '', ylab = '')
#   mtext(titleval, side = 3, cex = 0.8)
#   points(sim1 ~ date, tmp$alldat[tmp$smps, ], col = 'red', pch = 19)
#   
# }

# ######
# # diagnostic plots for individual training/validation splits
# 
# library(tidyverse)
# devtools::load_all('M:/docs/wtreg_for_estuaries')
# library(mgcv)
# library(foreach)
# library(doParallel)
# library(gridExtra)
# source('text/R/funcs.R')
# 
# ######
# # get WRTDS validation error
# 
# # simulated daily data
# data(sims_day)
# 
# # format the daily data for use with WRTDS
# tosamp <- select(sims_day, date, sim1, lnQ_sim) %>% 
#   rename(
#     res = sim1,
#     flo = lnQ_sim
#     ) %>% 
#   mutate(
#     lim = -1e6
#   )
# # tosamp <- select(sims_day, date, sim1, lnQ_sim) %>% 
# #   rename(
# #     res = sim1,
# #     flo = lnQ_sim
# #     ) %>% 
# #   mutate(
# #     # lim = -1e6, 
# #     doy = as.numeric(strftime(date, '%j'))
# #   )
# # tosamp$dec_time <- dec_time(tosamp$date)$dec_time
# 
# # get the conditions in grd
# unit <- 'week'
# missper <- 0.5
# blck <- 0.3
# blckper <- T
# 
# # prep data for training, validation  
# set.seed(4321)
# samps <- samp_sim(tosamp, unit = unit, irregular = F, missper = missper, blck = blck, blckper = blckper)
# 
# # training and validation datasets
# alldat <- samps$alldat
# trndat <- alldat[!1:nrow(alldat) %in% samps$smps, ]
# valdat <- alldat[samps$smps, ]
# 
# # fit model
# mod <- tidalmean(trndat,
#   reslab = expression(paste('ln-Chl-',italic(a),' (',italic('\u03bc'),'g ',L^-1,')')),
#   flolab = expression(paste('ln-Flow (', m^3, ' ', s^-1, ')'))
#   ) %>% 
#   wrtds(min_obs = F, flo_div = 10)
# # trnmod <- gam(res~te(dec_time, doy, flo, bs=c("tp","cc","tp")), data = trndat, knots=list(doy=c(1,366)), 
# #   na.action = na.exclude)
#    
# # get training, validation predictions
# trn <- respred(mod)
# val <- respred(mod, dat_pred = valdat) %>% 
#   select(., date, res.y, fits) %>% 
#   filter(date %in% valdat$date) %>% 
#   rename(res = res.y) %>% 
#   mutate(dat = 'val')
# # trn <- data.frame(date = trndat$date, res = trndat$res, fits = predict(trnmod))
# # val <- data.frame(date = valdat$date, res = valdat$res, fits = predict(trnmod, newdata = valdat)) %>% 
# #   mutate(dat = 'val')
# 
# toplo1 <- select(trn, date, res, fits) %>% 
#   mutate(dat = 'trn') %>% 
#   rbind(val) %>% 
#   gather('valtyp', 'val', res:fits) %>% 
#   mutate(
#     valtyp = factor(valtyp, levels = c('res', 'fits'), labels = c('Observed', 'Predicted')), 
#     dat = factor(dat, levels = c('trn', 'val'), labels = c('Training', 'Validation'))
#   )
# 
# toplo2 <- select(trn, date, res, fits) %>% 
#   mutate(dat = 'trn') %>% 
#   rbind(val) %>% 
#   mutate( 
#     dat = factor(dat, levels = c('trn', 'val'), labels = c('Training', 'Validation'))
#   )
# 
# mytheme <- theme_minimal() + 
#   theme(
#     axis.ticks.x = element_line(),
#     axis.ticks.y = element_line(),
#     panel.border = element_rect(fill = NA),
#     axis.ticks.length = unit(.1, "cm"), 
#     legend.position = 'top'
#   )
# 
# p1 <- ggplot(toplo1, aes(x = date, y = val)) +
#   geom_point() + 
#   facet_grid(valtyp ~ dat) + 
#   mytheme + 
#   theme(axis.title.x = element_blank()) + 
#   scale_y_continuous("ln-Chl-a")
# 
# p2 <- ggplot(toplo2, aes(x = res, y = fits)) + 
#   geom_abline(intercept = 0, slope = 1, colour = 'blue') + 
#   geom_point() + 
#   facet_grid(~ dat) + 
#   scale_y_continuous('Predicted') + 
#   scale_x_continuous('Observed') + 
#   mytheme
#   
# grid.arrange(p1, p2, ncol = 1, heights = c(1, 1))





