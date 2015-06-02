######
# summarize and plot performance of mean models with different window widths

library(ggplot2)
library(dplyr)
library(RColorBrewer)

# import files, summarize
fls <- list.files('data/', '^LE1\\.2|^TF1\\.6', full.names = T)

res <- vector("list", length(fls))
names(res) <- fls
for(fl in fls){
  
  cat(fl, '\n')
  
  load(file = fl)
  nm <- gsub('^data/|\\.RData', '', fl)
  perf <- wrtdsperf(get(nm))
  res[[fl]] <- perf
  rm(nm)
  
}

# format for plotting
perf <- do.call('rbind', res)
perf <- data.frame(perf)
perf$ind <- gsub('^data/|\\.RData', '', names(res))

ind <- gsub('^data/|\\.RData', '', names(res)) %>% 
  strsplit(., '_') %>% 
  do.call('rbind', .)
stat <- substr(ind[, 1], 1, 5)
exp_var <- substr(ind[, 1], 6, 9)
ind <- ind[, 2:ncol(ind)]

perf <- data.frame(perf, stat, exp_var, ind)
perf$ind <- NULL
perf$nmse <- NULL
names(perf) <- c('rmse', 'stat', 'exp_var', 'mos', 'yrs', 'sal')

perf$mos <- factor(perf$mos, levels = sort(as.numeric(levels(perf$mos)))) 
perf$yrs <- factor(perf$yrs, levels = sort(as.numeric(levels(perf$yrs)))) 
perf$sal <- factor(perf$sal, labels = sort(as.numeric(levels(perf$sal))))
perf$sal <- paste('sal/flo', perf$sal)

# plot
ggplot(perf, aes(x = mos, y = yrs, z = rmse, fill = rmse)) + 
  geom_tile() +
  facet_grid(stat + exp_var ~ sal) +
  scale_fill_gradientn(name = 'RMSE', 
    colours = brewer.pal(11, 'Spectral')
    ) +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) +
  geom_text(aes(label = round(rmse, 2)), size = 2) + 
  theme_bw() 

######
rm(list = ls())

data(pax_wrtds)

toplo <- lapply(pax_wrtds, wrtdsres)
toplo <- do.call('rbind', toplo)
ind <- gsub('_final\\.[0-9]*$', '', row.names(toplo))
toplo$stat <- substr(ind, 1, 5)
toplo$exps <- substr(ind, 6, 8)

p1 <- ggplot(toplo, aes(x = sal, y = chla)) + 
  geom_point(size = 2, alpha = 0.6, colour = 'purple') +
  geom_point(aes(y = fits, colour = 'Model fits'),
    size = 2, alpha = 0.6) +
  facet_wrap(stat ~ exps) +
  theme_bw() +
  theme(legend.position = 'top')

p2 <- ggplot(toplo, aes(x = sal, y = res)) + 
  geom_point(size = 2, alpha = 0.6, colour = 'purple') +
  geom_hline(yintercept = 0) +
  facet_wrap(stat ~ exps) +
  theme_bw()

grid.arrange(p1, p2)

