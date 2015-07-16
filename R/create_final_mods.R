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

bestLE12 <- modfit(tomod, resp_type = 'mean', wins = as.list(optimLE12_opt$par), 
  min_obs = FALSE)

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

bestTF16 <- modfit(tomod, resp_type = 'mean', wins = as.list(optimTF16_opt$par), min_obs = FALSE)

