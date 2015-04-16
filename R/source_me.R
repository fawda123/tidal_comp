# devtools::load_all('M:/docs/wtreg_for_estuaries/')
library(WRTDStidal)
library(foreach)
library(doParallel)

data(pax_data)

stats <- c('LE1.3', 'TF1.6')

sal <- seq(0.25, 15, length = 5)
yrs <- seq(1, 30, length = 5)
mos <- seq(0.1, 2, length = 5)
grd <- expand.grid(sal, yrs, mos)
names(grd) <- c('sal', 'yrs', 'mos')

cl <- makeCluster(8)
registerDoParallel(cl)

tomod <- pax_data[pax_data$STATION %in% stats[2], ]
tomod$limval <- 0
tomod$STATION <- NULL

strt <- Sys.time()
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
  mod <- WRTDStidal::modfit(tomod, wins = wins, tau = c(0.1, 0.5, 0.9))
  
  mod
  
}

# create names for indexing
# note that column order was changed but doesn't affect order of results
# new order is day_num, year, sal
names(ests) <- paste(grd[, 3], grd[, 2], grd[, 1])

TF16ests <- ests
save(TF16ests, file = 'data/TF16ests.RData')
save(TF16ests, file = 'shiny/TF16ests.RData')
