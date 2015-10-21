######

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

cl <- makeCluster(4)
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

cl <- makeCluster(4)
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

######
# wtreg window evals for simulated data

library(dplyr)
library(WRTDStidal)
# devtools::load_all("M:/docs/wtreg_for_estuaries")
library(foreach)
library(doParallel)

cl <- makeCluster(6)
registerDoParallel(cl)

# load data
data(sims_mos)

##
# select the dataset, run evaluation

# first simulation
sim1 <- select(sims_mos, date,  sim1, lnQ_sim)
names(sim1) <- c('date', 'chla', 'sal')
sim1$lim <- 0
sim1 <- tidalmean(sim1)

sim1_opt <- winsrch_optim(sim1, upper = c(1, 20, 1), lower = c(0.25, 1, 0.25), 
  min_obs = FALSE, k = 5)
save(sim1_opt, file = 'data/sim1_opt.RData')

# second simulation
sim2 <- select(sims_mos, date,  sim2, lnQ_sim)
names(sim2) <- c('date', 'chla', 'sal')
sim2$lim <- 0
sim2 <- tidalmean(sim2)

sim2_opt <- winsrch_optim(sim2, upper = c(1, 20, 1), lower = c(0.25, 1, 0.25), 
  min_obs = FALSE, k = 5)
save(sim2_opt, file = 'data/sim2_opt.RData')

# third simulation
sim3 <- select(sims_mos, date,  sim3, lnQ_sim)
names(sim3) <- c('date', 'chla', 'sal')
sim3$lim <- 0
sim3 <- tidalmean(sim3)

sim3_opt <- winsrch_optim(sim3, upper = c(1, 20, 1), lower = c(0.25, 1, 0.25), 
  min_obs = FALSE, k = 5)
save(sim3_opt, file = 'data/sim3_opt.RData')
