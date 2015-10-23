data(bestLE12)
data(bestTF16)

bestLE12 <- mutate(bestLE12, stat = 'LE1.2')
bestTF16 <- mutate(bestTF16, stat = 'TF1.6')

toplo <- rbind(bestLE12, bestTF16) %>% 
  select(date, fits_wrtds, fits_gams, chla, stat) %>% 
  mutate(
    Year = '2014', 
    year_act = as.numeric(strftime(date, '%Y')), 
    mo = as.numeric(strftime(date, '%m')),
    day = as.numeric(strftime(date, '%d')),
    fake_date = as.Date(paste(Year, mo, day, sep = '-'))
  ) %>% 
  gather('mod', 'val', fits_wrtds:chla) %>% 
  mutate(
    mod = factor(mod, levels = c('fits_gams', 'fits_wrtds', 'chla'), 
      labels = c('GAM', 'WRTDS', 'observed')),
    yeargrp = cut(year_act, c(-Inf, 2000, Inf), c('1985-2000', '2001-2014'))
  )

ylab <- expression(paste('ln-Chl ',' (',italic('\u03bc'),'g ',L^-1,')'))
xlab <- 'Day of year'

ggplot(toplo, aes(x = fake_date, y = val, group = mod)) + 
  geom_point(pch = 1, col = cols[40]) + 
  stat_smooth(method = 'loess', span = 0.4, se = F, col = cols[1], size = 1) +
  scale_y_continuous(ylab) + 
  scale_x_date(xlab, labels = date_format("%m/%d")) + 
  facet_grid(stat + yeargrp ~ mod, scales = 'free_y') + 
  theme_mine() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))