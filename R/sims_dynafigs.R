##
# this code creates a 6 by 12 figure matrix for the model results for each month for each simulated time series

load(file = 'M:/docs/manuscripts/patux_manu/data/bestsim_wrtds.RData')

## gam data and mods for three simulated datasets

noflo_tomod <- select(bestsim_wrtds[[2]], date, dec_time, chla, sal) %>% 
  mutate(
    doy = as.numeric(strftime(date, '%j'))
  )
const_tomod <- select(bestsim_wrtds[[1]], date, dec_time, chla, sal) %>% 
  mutate(
    doy = as.numeric(strftime(date, '%j'))
  )
incrs_tomod <- select(bestsim_wrtds[[3]], date, dec_time, chla, sal) %>% 
  mutate(
    doy = as.numeric(strftime(date, '%j'))
  )

noflo_gam <- gam(chla~te(dec_time, doy, sal, bs=c("tp","cc","tp")), data = noflo_tomod, 
  knots=list(doy=c(1,366)), na.action = na.pass)
const_gam <- gam(chla~te(dec_time, doy, sal, bs=c("tp","cc","tp")), data = const_tomod, 
  knots=list(doy=c(1,366)), na.action = na.pass)
incrs_gam <- gam(chla~te(dec_time, doy, sal, bs=c("tp","cc","tp")), data = incrs_tomod, 
  knots=list(doy=c(1,366)), na.action = na.pass)


# color palette
cols <- wes_palette('Zissou', 100, 'continuous') %>% 
  as.character %>% 
  .[1:60]
modcols <- cols[c(1, 50)]

lims <- c(-0.5, 3)
mars <- grid::unit(c(0, 0, 0, 0), 'cm')

mos <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

# no effect gams
p1a <- dynagam(noflo_gam, noflo_tomod, month = 1:12, col_vec = cols, fac_nms = paste0('GAM: ', mos), ncol = 1) + 
  scale_y_continuous(limits = lims) +
  theme_mine() +
  theme(legend.position = 'none', axis.title = element_blank(), plot.margin = mars, 
    strip.text.x = element_text(size = NULL))

# no effect wrtds
p1b <- dynaplot(bestsim_wrtds[[2]], month = 1:12, col_vec = cols, fac_nms = paste0('WRTDS: ', mos), ncol = 1) + 
  scale_y_continuous(limits = lims) +
  theme_mine() +
  theme(legend.position = 'none', axis.title = element_blank(), plot.margin = mars)

# constant gams
p2a <- dynagam(const_gam, const_tomod, month = 1:12, col_vec = cols, fac_nms = paste0('GAM: ', mos), ncol = 1) + 
  scale_y_continuous(limits = lims) +
  theme_mine() +
  theme(legend.position = 'none', axis.title = element_blank(), plot.margin = mars, 
    strip.text.x = element_text(size = NULL))

# constant wrtds
p2b <- dynaplot(bestsim_wrtds[[1]], month = 1:12, col_vec = cols, fac_nms = paste0('WRTDS: ', mos), ncol = 1) + 
  scale_y_continuous(limits = lims) +
  theme_mine() +
  theme(legend.position = 'none', axis.title = element_blank(), plot.margin = mars)

# constant gams
p3a <- dynagam(incrs_gam, incrs_tomod, month = 1:12, col_vec = cols, fac_nms = paste0('GAM: ', mos), ncol = 1) + 
  scale_y_continuous(limits = lims) +
  theme_mine() +
  theme(legend.position = 'none', axis.title = element_blank(), plot.margin = mars, 
    strip.text.x = element_text(size = NULL))

# constant wrtds
p3b <- dynaplot(bestsim_wrtds[[3]], month = 1:12, col_vec = cols, fac_nms = paste0('WRTDS: ', mos), ncol = 1) + 
  scale_y_continuous(limits = lims) +
  theme_mine() +
  theme(legend.position = 'none', axis.title = element_blank(), plot.margin = mars)


ylabs <- expression(paste('ln-Chl-',italic(a),' (',italic('\u03bc'),'g ',L^-1,')'))

pdf('C:/Users/mbeck/Desktop/sim_res.pdf', height = 14, width = 14, family = 'serif')
grid.arrange(
  arrangeGrob(
    grid::textGrob(ylabs, rot = 90),
    arrangeGrob(
      grid::textGrob('No flow'),
      arrangeGrob(p1a, p1b, ncol = 2),
      heights = c(1, 30)
      ), 
    arrangeGrob(
      grid::textGrob('Constant'),
      arrangeGrob(p2a, p2b, ncol = 2),
      heights = c(1, 30)
      ),
    arrangeGrob(
      grid::textGrob('Increasing'),
      arrangeGrob(p3a, p3b, ncol = 2), 
      heights = c(1, 30)
      ),
    ncol = 4, widths = c(1, 20, 20, 20)
  ), 
  grid::textGrob('Flow'), ncol = 1,
  heights = c(50, 1)
)
dev.off()