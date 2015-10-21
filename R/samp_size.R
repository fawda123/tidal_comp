library(WRTDStidal)
library(gridExtra)

load(file = 'data/bestLE12_wrtds_min.RData')
load(file = 'data/bestTF16_wrtds_min.RData')
le12_min <- bestLE12_wrtds
tf16_min <- bestTF16_wrtds
load(file = 'data/bestLE12_wrtds_nomin.RData')
load(file = 'data/bestTF16_wrtds_nomin.RData')
le12_nomin <- bestLE12_wrtds
tf16_nomin <- bestTF16_wrtds

mos <- c(1:12)

p1 <- dynaplot(le12_min, ncol = 1, month = mos) + 
  ggtitle('LE12, minimum') + 
  scale_x_reverse('Salinity')
p2 <- dynaplot(le12_nomin, ncol = 1, month = mos) + 
  ggtitle('LE12, no minimum') + 
  scale_x_reverse('Salinity')
p3 <- nobsplot(le12_min, ncol = 1, month = mos, allsal  = F) + 
  ggtitle('LE12, minimum') + 
  scale_x_continuous('Date')
p4 <- nobsplot(le12_nomin, ncol = 1, month = mos, allsal  = F) + 
  ggtitle('LE12, no minimum') + 
  scale_x_continuous('Date')
p5 <- dynaplot(tf16_min, ncol = 1, month = mos) + 
  ggtitle('TF16, minimum') + 
  scale_x_continuous('Flow')
p6 <- dynaplot(tf16_nomin, ncol = 1, month = mos) + 
  ggtitle('TF16, no minimum') + 
  scale_x_continuous('Flow')
p7 <- nobsplot(tf16_min, ncol = 1, month = mos, allsal  = F) + 
  ggtitle('TF16, minimum') + 
  scale_x_continuous('Date')
p8 <- nobsplot(tf16_nomin, ncol = 1, month = mos, allsal  = F) + 
  ggtitle('TF16, no minimum') + 
  scale_x_continuous('Date')

pdf('C:/Users/mbeck/Desktop/checkobs.pdf', family = 'serif', height = 15, width = 14)
grid.arrange(p1, p2, p3, p4, ncol = 4)
grid.arrange(p5, p6, p7, p8, ncol = 4)
dev.off()


