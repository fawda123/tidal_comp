###
# wt reg window evals for patux

# salinity first, LE1.2

library(WRTDStidal)
library(foreach)
library(doParallel)

data(pax_data)

# which variable to use for flow? salinity...
pax_data <- pax_data[, !names(pax_data) %in% 'lnQ']

stat <- c('LE1.2')

sal <- c(seq(0.5, 1, by = 0.1))
yrs <- c(seq(5, 15, by = 2))
mos <- c(seq(0.5, 1, by = 0.25), 1.5, 2)
grd <- expand.grid(sal, yrs, mos)
names(grd) <- c('sal', 'yrs', 'mos')

cl <- makeCluster(8)
registerDoParallel(cl)

tomod <- pax_data[pax_data$STATION %in% stat, ]
row.names(tomod) <- 1:nrow(tomod)
tomod$limval <- 0
tomod$STATION <- NULL
tomod <- tomod[order(tomod$date), ]
tomod <- tidal(tomod)

# eval
resLE12 <- wtssrch(tomod, grd)
save(resLE12, file = 'resLE12.RData')

###
# flow second, TF16

data(pax_data)

# which variable to use for flow?
pax_data <- pax_data[, !names(pax_data) %in% 'sal']
names(pax_data)[names(pax_data) %in% 'lnQ'] <- 'sal'

stat <- c('TF1.6')

cl <- makeCluster(8)
registerDoParallel(cl)

tomod <- pax_data[pax_data$STATION %in% stat, ]
row.names(tomod) <- 1:nrow(tomod)
tomod$limval <- 0
tomod$STATION <- NULL
tomod <- tomod[order(tomod$date), ]
tomod <- tidal(tomod)

# eval
resTF16 <- wtssrch(tomod, grd)
save(resTF16, file = 'resTF16.RData')
