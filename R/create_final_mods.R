###
# wt reg results for patux, set half-window widths
# LE1.2, TF1.6, salinty and flow as separate exp vars

##
# salinity first

# devtools::load_all('M:/docs/wtreg_for_estuaries/')
library(WRTDStidal)
library(foreach)
library(doParallel)

data(pax_data)

# stations and master list
stats <- c('TF1.6', 'LE1.2')
out <- list()

# which variable to use for flow?
pax_data <- pax_data[, !names(pax_data) %in% 'lnQ']

# windows
wins <- list(0.5, 10, 0.5)

for(stat in stats){

  tomod <- pax_data[pax_data$STATION %in% stat, ]
  row.names(tomod) <- 1:nrow(tomod)
  tomod$limval <- 0
  tomod$STATION <- NULL

  # fit model
  mod <- WRTDStidal::modfit(tomod, wins = wins, resp_type = 'mean', min_obs = TRUE, trace = T)
    
  # name and save the output
  nm <- paste0(stat, 'sal_final')
  assign(nm, mod)
  out[[nm]] <- get(nm)

}

#
# flow second

data(pax_data)

# which variable to use for flow?
pax_data <- pax_data[, !names(pax_data) %in% 'sal']
names(pax_data)[names(pax_data) %in% 'lnQ'] <- 'sal'

for(stat in stats){

  tomod <- pax_data[pax_data$STATION %in% stat, ]
  row.names(tomod) <- 1:nrow(tomod)
  tomod$limval <- 0
  tomod$STATION <- NULL

  # fit model
  mod <- WRTDStidal::modfit(tomod, wins = wins, resp_type = 'mean', min_obs = TRUE, trace = T)
    
  # name and save the output
  nm <- paste0(stat, 'flo_final')
  assign(nm, mod)
  out[[nm]] <- get(nm)

}

pax_wrtds <- out
save(pax_wrtds, file = 'data/pax_wrtds.RData')
