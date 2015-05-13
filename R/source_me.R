# devtools::load_all('M:/docs/wtreg_for_estuaries/')
library(WRTDStidal)
library(foreach)
library(doParallel)

data(pax_data)

stats <- c('TF1.6', 'LE1.3')

sal <- c(seq(5, 15, by = 3), 30)
yrs <- c(seq(5, 15, by = 3), 50)
mos <- c(seq(0.5, 1, by = 0.25), 2, 10)
grd <- expand.grid(sal, yrs, mos)
names(grd) <- c('sal', 'yrs', 'mos')

cl <- makeCluster(6)
registerDoParallel(cl)

strt <- Sys.time()

for(stat in stats){

  tomod <- pax_data[pax_data$STATION %in% stat, ]
  row.names(tomod) <- 1:nrow(tomod)
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
    mod <- WRTDStidal::modfit(tomod, wins = wins, resp_type = 'quantile', min_obs = FALSE, trace = T, tau = c(0.1, 0.5, 0.9))
    # fit model
    # mod <- WRTDStidal::modfit(tomod, wins = wins, resp_type = 'mean', min_obs = FALSE, trace = T)
    
    nm <- paste(grd[i, 3], grd[i, 2], grd[i, 1], sep = '_')
    
    nm <- paste0(stat, '_' , nm)
    # nm <- paste0(stat, 'mean_' , nm)
    assign(nm, mod)
    save(list = nm, file = paste0('data/', nm, '.RData'))
  
  }

}
