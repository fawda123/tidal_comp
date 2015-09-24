# GAM fit for Patuxent
# 9-16-15
#Rebecca Murphy

#########################################################################
###### Part 1. Set up
#########################################################################

library(mgcv)    #gam package
library(chron)   #date package

# ----------------------------------------------------------------
###### 1a. input data
# ----------------------------------------------------------------
data.in<- read.table("f:/Trends/Patuxent/GAMs/pax_data.csv", header=TRUE,sep=",", na.strings="NA", dec=".", stringsAsFactors = FALSE)
data1<-data.in[complete.cases(data.in),]

#
# ----------------------------------------------------------------
###### 1b. data file processing
# ----------------------------------------------------------------


dt.stat<-as.Date(data1$date)
posdt.stat<-strptime(data1$date,"%Y-%m-%d")
doy.stat<-posdt.stat$yday
yr.stat<-posdt.stat$year+1900
dec.stat<-as.numeric(yr.stat)+doy.stat/365
mo.stat<-posdt.stat$mon+1
dy.stat<-posdt.stat$mday


# ----------------------------------------------------------------
###### 1c. Pick Station  - cycle through s=1 (TF1.6) and s=2 (LE1.2)
# ----------------------------------------------------------------
stats_s<-c("TF1.6","LE1.2")  

s<-1
stat_s<-stats_s[s]
stat_s

stats<-data1$STATION

# ----------------------------------------------------------------
###### 1d. Pull out values, dates, and flow or salinity
# ----------------------------------------------------------------

values<-data1$lnchla[stats==stat_s]
sal<-data1$sal[stats==stat_s]
lnQ<-data1$lnQ[stats==stat_s]

dates<-dt.stat[stats==stat_s]
years<-yr.stat[stats==stat_s]
doy<-doy.stat[stats==stat_s]
dec.yr<-dec.stat[stats==stat_s]
mo<-mo.stat[stats==stat_s]
pst.date<-posdt.stat[stats==stat_s]

#Decided in preliminary analyses that we are using flow for TF1.6 and salinity for LE1.2
{if(stat_s=="TF1.6") {
  txt.Qsal<-"Flow"
  Qsal<-lnQ
}
else {
  txt.Qsal<-"Salinity"
  Qsal<-sal
}}


#########################################################################
###### Part 2. GAMs
#########################################################################

gam4te<-gam(values~te(dec.yr,doy,Qsal,bs=c("tp","cc","tp")),knots=list(doy=c(1,366)))

summary(gam4te)
AIC(gam4te)
R.gam<-residuals(gam4te)
acf.g<-acf(R.gam,type="correlation",plot=TRUE)

#GAMpredictions
predict.te<-predict(gam4te,se=TRUE)  


#########################################################################
###### Part 3.  Flow-normalized: draft - each day of record version
#########################################################################


flow.norm<-matrix(0,nrow=length(values),ncol=2)

Q.pred<-matrix(0,nrow=29,ncol=12)
ct1<-1

for(i in 1:12) {
  
  #identify all flows/salinities in this month
  all.i.Qs<-Qsal[mo==i]
  all.i.doy<-doy[mo==i]
  all.i.dec<-dec.yr[mo==i]
  
  Q.pred<-matrix(0,nrow=length(all.i.Qs),ncol=length(all.i.Qs))
  
  for(q in 1:length(all.i.Qs)) {
    newd.q<-data.frame(dec.yr=all.i.dec,Qsal=all.i.Qs[q],doy=all.i.doy)
    Q.pred[,q]<-predict(gam4te,newd.q,se=FALSE)
  }
  
  #average the predictions for each flow to have a values for each year, in this month
  ct2<-ct1+length(all.i.Qs)-1
  flow.norm[ct1:ct2,2]<-rowMeans(Q.pred)
  flow.norm[ct1:ct2,1]<-all.i.dec
  ct1<-ct2+1
  
}

ord.fn<-order(flow.norm[,1])
fn.out<-flow.norm[ord.fn,]


#######################################
#  output predictions and flow-normalization results
######################################

for.out<-data.frame(dates,dec.yr,Predict_lnchla=predict.te$fit,FlowNorm_lnchla=fn.out[,2])
write.csv(for.out,paste("F:/Trends/Patuxent/GAMs/GAM_output_",stat_s,".csv",sep=""))

