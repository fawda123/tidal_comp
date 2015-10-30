library(ggplot2)
library(dplyr)
library(tidyr)
library(wesanderson)
library(scales)

cols <- wes_palette('Zissou', 100, 'continuous') %>% 
  as.character %>% 
  .[1:60]

theme_mine <- function (base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9), 
    plot.background = element_rect(fill='transparent', 
      colour = NA),
    panel.background = element_rect(fill='transparent', 
      colour = NA),
    legend.background = element_rect(fill='transparent', 
      colour = NA),
    strip.background = element_rect(fill = 
        alpha(cols[length(cols)], 0.5)),
    legend.key = element_rect(fill = 'transparent', 
      colour = NA)
    )   
}

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
    yeargrp = cut(year_act, c(-Inf, 1995, 2003, Inf), c('1985-1995', '1996-2003', '2004-2014'))
  ) %>% 
  filter(yeargrp != '1996-2003')



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