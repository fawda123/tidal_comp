######
# helper functions

######
#r.squared function
#created for data from weighted regression, but works for all models
#residuals are observed - predicted, but taken from model objects (see 'epc_mods.R')
rsq.fun<-function(resid,obs){
  
  require(Metrics)
  
  # get complete cases
  toeval <- data.frame(resid, obs)
  toeval <- na.omit(toeval)
  
  ssr<-sum(toeval$resid^2)
  sst<-sum(se(toeval$obs,mean(toeval$obs)))
  
  return(1 - (ssr/sst))

}

######
#variant of rmse fun in Metrics package but handles na values
#resid is obs - predicted
rmse.fun<-function(val1, val2 = NULL){
  
  if(!is.null(val2)) resid <- val1 - val2
  else resid <- val1
  
  out<-sqrt(mean(resid^2,na.rm=T))
    
  return(out) 
  
}

######
# average difference
ave.fun <- function(ts1, ts2){
  
  ts1 <- sum(ts1, na.rm = TRUE)
  ts2 <- sum(ts2, na.rm = TRUE)
  
  out <- 100 * (ts1 - ts2)/ts2 
  
  return(out)
  
}

######
# linear model function used for data prep in summary table
lm.fun <- function(ts1, ts2, origin = FALSE, alph = 0.05){
  
  tomod <- na.omit(data.frame(ts1, ts2))
  
  # through origin
  if(origin){ 
    
    # model coefs
    mod <- lm(ts1 ~ 0 + ts2, data = tomod)
    mod <- coef(summary(mod))
    
    # ests and se
    slo <- mod[, 'Estimate']
    slose <- mod[, 'Std. Error']
    
    # margin of error for slope
    qtval <- qt(1 - alph/2, df = nrow(tomod) - 1) 
    slom <- qtval * slose
    slorng <- c(slo - slom, slo + slom)
    slorng <- slorng[1] <= 1 & 1 <= slorng[2] # checks if confint for slope covers one
    
    # make bold, italic to indicate estimate is different from one
    if(!slorng) slo <- paste0('{\\bf \\textit{', form_fun(slo), '}}')
    else slo <- form_fun(slo)
    
    out <- slo
    
  # not through origin
  } else {      
   
    # model coefs
    mod <- lm(ts1 ~ ts2, data = tomod)
    mod <- coef(summary(mod))
        
    # ests and se
    int <- mod[1, 'Estimate']
    intse <- mod[1, 'Std. Error'] 
    slo <- mod[2, 'Estimate']
    slose <- mod[2, 'Std. Error'] 
    
    # margin of errors for each
    qtval <- qt(1 - alph/2, df = nrow(tomod) - 2) 
    intm <- qtval * intse
    slom <- qtval * slose
    intrng <- c(int - intm, int + intm)
    intrng <- intrng[1] <= 0 & 0 <= intrng[2] # checks if confint for intercept covers zero
    slorng <- c(slo - slom, slo + slom)
    slorng <- slorng[1] <= 1 & 1 <= slorng[2] # checks if confint for slope covers one
    
    # make bold, italic to indicate estimate is different from zero
    if(!intrng) int <- paste0('{\\bf \\textit{', form_fun(int), '}}')
    else int <- form_fun(int)
    
    # make bold, italic to indicate estimate is different from one
    if(!slorng) slo <- paste0('{\\bf \\textit{', form_fun(slo), '}}')
    else slo <- form_fun(slo)

    out <- c(int, slo)
    
  }
  
  return(out)
  
}

######
# formatting of values in S expressions
form_fun <- function(x, rnd_val = 2, dig_val = 2, nsm_val = 2) {
  format(round(x, rnd_val), digits = dig_val, nsmall = nsm_val)
}

######
# get legend from an existing ggplot object
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

######
# gam equivalent of dynaplot function
# looks at changes in in the chlorophyll-flow relationships by month over different years
# grd is the number of salinity values to use in predictions
dynagam <- function(mod_in, dat_in, grd = 30, years = NULL, alpha = 1,
  size = 1, col_vec = NULL, allflo = FALSE, month = c(1:12), scales = NULL, ncol = NULL, 
  pretty = TRUE, grids = TRUE, use_bw = TRUE){

  # add year, month columns to dat_in
  dat_in <- mutate(dat_in, 
    month = as.numeric(strftime(date, '%m')), 
    year = as.numeric(strftime(date, '%Y'))
  )
  to_plo <- dat_in
  
  # flo values to predict
  flo_vals <- range(to_plo[, 'sal'], na.rm = TRUE)
  flo_vals <- seq(flo_vals[1], flo_vals[2], length = grd)
  
  # get model predictions across range of flow values
  dynadat <- rep(flo_vals, each = nrow(to_plo)) %>% 
    matrix(., nrow = nrow(to_plo), ncol = grd) %>% 
    cbind(to_plo[, c('dec_time', 'doy')], .) %>%
    gather('split', 'sal', -dec_time, -doy) %>% 
    select(-split) %>% 
    data.frame(., chla = predict(mod_in, .)) %>% 
    spread(sal, chla) %>% 
    select(-dec_time, -doy)
  
  # merge predictions with year, month data, make long format
  to_plo <- select(to_plo, year, month) %>% 
    cbind(., dynadat) %>% 
    gather('sal', 'chla', -year, -month) %>% 
    mutate(sal = as.numeric(as.character(sal)))
  
  # subset years to plot
  if(!is.null(years)){
    
    to_plo <- to_plo[to_plo$year %in% years, ]
    to_plo <- to_plo[to_plo$month %in% month, ]
    
    if(nrow(to_plo) == 0) stop('No data to plot for the date range')
  
  }
    
  # constrain plots to salinity limits for the selected month
  if(!allflo){
    
    #min, max salinity values to plot
    lim_vals <- group_by(data.frame(dat_in), month) %>% 
      summarise(
        Low = quantile(sal, 0.05, na.rm = TRUE),
        High = quantile(sal, 0.95, na.rm = TRUE)
      )
  
    # month sal ranges for plot
    lim_vals <- lim_vals[lim_vals$month %in% month, ]

    # merge limts with months
    to_plo <- left_join(to_plo, lim_vals, by = 'month')
    to_plo <- to_plo[to_plo$month %in% month, ]
    
    # reduce data
    sel_vec <- with(to_plo, 
      sal >= Low &
      sal <= High
      )
    to_plo <- to_plo[sel_vec, !names(to_plo) %in% c('Low', 'High')]
    to_plo <- arrange(to_plo, year, month)
    
  }
  
  # reshape data frame, average by year, month for symmetry
  to_plo <- group_by(to_plo, year, month, sal) %>% 
    summarise(
      chla = mean(chla, na.rm = TRUE)
    )
  
  # months labels as text
  mo_lab <- data.frame(
    num = seq(1:12), 
    txt = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
  )
  mo_lab <- mo_lab[mo_lab$num %in% month, ]
  to_plo$month <- factor(to_plo$month, levels =  mo_lab$num, labels = mo_lab$txt)
  
  # make plot
  p <- ggplot(to_plo, aes(x = sal, y = chla, group = year)) + 
    facet_wrap(~month, ncol = ncol, scales = scales)
  
  # return bare bones if FALSE
  if(!pretty) return(p + geom_line())
  
  # colors, uses gradcols from WRTDStidal
  cols <- gradcols(col_vec = col_vec)
  
  # chllab function from WRTDStidal
  ylabel <- chllab(TRUE)
  
  # use bw theme
  if(use_bw) p <- p + theme_bw()
  
  p <- p + 
    geom_line(size = size, aes(colour = year), alpha = alpha) +
    scale_y_continuous(ylabel, expand = c(0, 0)) +
    scale_x_continuous('Salinity', expand = c(0, 0)) +
    theme(
      legend.position = 'top'
    ) +
    scale_colour_gradientn('Year', colours = cols) +
    guides(colour = guide_colourbar(barwidth = 10)) 
  
  # remove grid lines
  if(!grids) 
    p <- p + 
      theme(      
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  
  return(p)
  
}