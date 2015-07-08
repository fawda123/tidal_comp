###
# wt reg window evals for patux

# salinity first, LE1.2

library(WRTDStidal)
# devtools::load_all("M:/docs/wtreg_for_estuaries")
library(foreach)
library(doParallel)

data(pax_data)

# which variable to use for flow? salinity...
pax_data <- pax_data[, !names(pax_data) %in% 'lnQ']

stat <- c('LE1.2')

cl <- makeCluster(6)
registerDoParallel(cl)

tomod <- pax_data[pax_data$STATION %in% stat, ]
row.names(tomod) <- 1:nrow(tomod)
tomod$limval <- 0
tomod$STATION <- NULL
tomod <- tomod[order(tomod$date), ]
tomod <- tidalmean(tomod)
 
# eval
# optimLE12 <- winsrch_constrOptim(tomod, upper = c(20, 100, 20), lower = c(0.25, 1, 0.25), 
#   min_obs = FALSE, control = list(factr = 1e10))
optimLE12_opt <- winsrch_optim(tomod, upper = c(20, 100, 20), lower = c(0.25, 1, 0.25), 
  min_obs = FALSE, control = list(factr = 1e11))
save(optimLE12_opt, file = 'C:/Users/mbeck/Desktop/optimLE12_opt.RData')

###
# flow second, TF16

data(pax_data)

# which variable to use for flow?
pax_data <- pax_data[, !names(pax_data) %in% 'sal']
names(pax_data)[names(pax_data) %in% 'lnQ'] <- 'sal'

stat <- c('TF1.6')

cl <- makeCluster(6)
registerDoParallel(cl)

tomod <- pax_data[pax_data$STATION %in% stat, ]
row.names(tomod) <- 1:nrow(tomod)
tomod$limval <- 0
tomod$STATION <- NULL
tomod <- tomod[order(tomod$date), ]
tomod <- tidalmean(tomod)

# eval
# optimTF16 <- winsrch_constrOptim(tomod, upper = c(20, 100, 20), lower = c(0.25, 1, 0.25), 
#   min_obs = FALSE, control = list(factr = 1e10))
optimTF16_opt <- winsrch_optim(tomod, upper = c(20, 100, 20), lower = c(0.25, 1, 0.25), 
  min_obs = FALSE, control = list(factr = 1e11))
save(optimTF16_opt, file = 'C:/Users/mbeck/Desktop/optimTF16_opt.RData')
