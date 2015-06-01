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
pdf('C:/Users/mbeck/Desktop/mod_perf.pdf', height = 9, width = 13, family = 'serif')
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
dev.off()