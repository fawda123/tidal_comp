# devtools::load_all('M:/docs/wtreg_for_estuaries/')
library(WRTDStidal)
library(foreach)
library(doParallel)

data(pax_data)

stats <- c('LE1.3', 'TF1.6', 'TF1.7')

sal <- c(seq(1, 10, by = 2), 30)
yrs <- c(seq(2, 13, by = 2), 50)
mos <- c(seq(0.5, 1, by = 0.25), 1.5, 2, 10)
grd <- expand.grid(sal, yrs, mos)
names(grd) <- c('sal', 'yrs', 'mos')

cl <- makeCluster(4)
registerDoParallel(cl)

strt <- Sys.time()

for(stat in stats){

  tomod <- pax_data[pax_data$STATION %in% stat, ]
  tomod$limval <- 0
  tomod$STATION <- NULL
  
  ests <- foreach (i = 1:nrow(grd)) %dopar% {
    
    # log
    sink('C:/Users/mbeck/Desktop/log.txt')
    cat(i, '\n')
    print(Sys.time() - strt)
    sink()
    
    # wts from grid
    wins <- grd[i, ]
    wins <- with(wins, list(mos, yrs, sal))
    
    # fit model
    mod <- WRTDStidal::modfit(tomod, wins = wins, tau = c(0.1, 0.5, 0.9), min_obs = FALSE, trace = T)
  
    mod
  
  }

  # create names for indexing
  # note that column order was changed but doesn't affect order of results
  # new order is day_num, year, sal
  names(ests) <- paste(grd[, 3], grd[, 2], grd[, 1])

  # save output
  flnm <- paste0(gsub('\\.', '', stat), 'ests')
  assign(flnm, ests)
  save(list = flnm, file = paste0('M:/docs/tidal_comp/data/', flnm, '.RData'))
  save(list = flnm, file = paste0('M:/docs/tidal_comp/patux_wtreg/', flnm, '.RData'))
  
}
