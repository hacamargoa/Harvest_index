source("C:/users/hac809/Desktop/Harvest_index/HI_data_extract.R")
library(raster)
library(ncdf4)
library(lubridate)
library(plyr)
sdates<-read.table("C:/Users/hac809/Desktop/Newcrops/data/env/sdates.txt",h=T)
sdates<-sdates[-c(10,11,12,13)]
hdates<-read.table("C:/Users/hac809/Desktop/Newcrops/data/env/hdates.txt",h=T)
hdates<-hdates[-c(10,11,12,13)]
cropNa<-colnames(sdates)[c(-1,-2)]
hdat<-list()
sdat<-list()
for (i in 1:length(cropNa)){
  hdat[[i]]<-hdates[c("Lon","Lat",cropNa[i])]
  coordinates(hdat[[i]])<- ~Lon+Lat
  gridded(hdat[[i]])<- TRUE
  hdat[[i]]<-raster(hdat[[i]])
  sdat[[i]]<-sdates[c("Lon","Lat",cropNa[i])]
  coordinates(sdat[[i]])<- ~Lon+Lat
  gridded(sdat[[i]])<- TRUE
  sdat[[i]]<-raster(sdat[[i]])
}

#extracting sdate & hdate
for(i in 1:nrow(HI2)){
  co<-cellFromXY(sdat[[1]],c(HI2$Longitude[i],HI2$Latitude[i]))
  HI2$sdate[i]<-ifelse(HI2$Crop[i]=="Rice",extract(sdat[[7]],co),
                       ifelse(HI2$Crop[i]=="Maize",ifelse(HI2$Irrigation[i]=="Irrigated",extract(sdat[[6]],co),extract(sdat[[5]],co)),
                              ifelse(HI2$Cult[i]=="Spring",ifelse(HI2$Irrigation[i]=="Irrigated",extract(sdat[[4]],co),extract(sdat[[3]],co)),
                                     ifelse(HI2$Irrigation[i]=="Irrigated",extract(sdat[[2]],co),extract(sdat[[1]],co)))))
  HI2$hdate[i]<-ifelse(HI2$Crop[i]=="Rice",extract(hdat[[7]],co),
                       ifelse(HI2$Crop[i]=="Maize",ifelse(HI2$Irrigation[i]=="Irrigated",extract(hdat[[6]],co),extract(hdat[[5]],co)),
                              ifelse(HI2$Cult[i]=="Spring",ifelse(HI2$Irrigation[i]=="Irrigated",extract(hdat[[4]],co),extract(hdat[[3]],co)),
                                     ifelse(HI2$Irrigation[i]=="Irrigated",extract(hdat[[2]],co),extract(hdat[[1]],co)))))
  
}

attach(HI2)
HI2$sdate<-ifelse(sdate>0,sdate,ifelse(Crop=="Maize",ifelse(Latitude>0,140,320),
                                       ifelse(Crop=="Rice",ifelse(Latitude>0,120,300),
                                              ifelse(Cult=="Spring",ifelse(Latitude>0,120,300),
                                                     ifelse(Latitude > 0, 330, 150)))))
detach(HI2)
HI2$hdate<-ifelse(HI2$hdate>0,HI2$hdate,ifelse((HI2$sdate+150)<365,HI2$sdate+150,HI2$sdate-215))

#reading the cru data
preci2<-brick("C:/Users/hac809/Documents/Pughtam-cropozone/Harvest Index/Harvest_index_inputs/cru_ts4.04.1901.2019.pre.dat.nc")
preci2<-subset(preci2,877:1428)
temp2<-brick("C:/Users/hac809/Documents/Pughtam-cropozone/Harvest Index/Harvest_index_inputs/cru_ts4.04.1901.2019.tmp.dat.nc")
temp2<-subset(temp2,877:1428)
temx<-brick("C:/Users/hac809/Documents/Pughtam-cropozone/Harvest Index/Harvest_index_inputs/cru_ts4.04.1901.2019.tmx.dat.nc")
temx<-subset(temx,877:1428)
attach(HI2)
for (i in 1:nrow(HI2)){
HI2$smonth[i]<-length(seq(from=as.Date("1974-01-01"),to=as.Date(as.Date(sdate[i],origin=paste0(as.character(year[i]),"-01-01"))),by='month'))
HI2$smonth[i]<-ifelse(day(as.Date(sdate[i],origin=paste0(as.character(year[i]),"-01-01")))>20,HI2$smonth[i]+1,HI2$smonth[i])
HI2$hmonth[i]<-length(seq(from=as.Date("1974-01-01"),to=as.Date(as.Date(hdate[i],origin=paste0(as.character(year[i]),"-01-01"))),by='month'))
HI2$hmonth[i]<-ifelse(day(as.Date(hdate[i],origin=paste0(as.character(year[i]),"-01-01")))<10,HI2$hmonth[i]-1,HI2$hmonth[i])
co<-cellFromXY(preci2,c(HI2$Longitude[i],HI2$Latitude[i]))
HI2$prec[i]<-sum(extract(subset(preci2,HI2$smonth[i]:HI2$hmonth[i]),co))
HI2$temp[i]<-mean(extract(subset(temp2,HI2$smonth[i]:HI2$hmonth[i]),co))
HI2$temx[i]<-mean(extract(subset(temx,HI2$smonth[i]:HI2$hmonth[i]),co))
}
GDP<-read.csv("C:/Users/hac809/Documents/Pughtam-cropozone/Harvest Index/GDPpC.csv",h=T)
GDPv<-list()
for (i in 1:nrow(GDP)){
  GDPv[[i]]<-data.frame(Country=rep(GDP[i,1],ea=59),year=c(1960:2018))
  GDPv[[i]]$GDPpC<-t(GDP[i,3:61])
}
GDPbC<-na.omit(do.call("rbind",GDPv))
HI2<-join(HI2,GDPbC,by=c("Country","year"))
GDPMaize<-lm(HI2$HI[(HI2$Crop=="Maize")]~log(HI2$GDPpC[(HI2$Crop=="Maize")]))
plot(log(HI2$GDPpC[(HI2$Crop=="Maize")]),HI2$HI[(HI2$Crop=="Maize")], ylab=" ", xlab=" ")
lines(fitted(GDPMaize))
title(ylab="Observed HI", xlab=ifelse(j==3,"Log(GDP)",""),cex.lab=1.5,font.lab=2)


#Correlation and regressions
HI3<-HI2
HI3$Irr<-ifelse(HI3$Irrigation=="Irrigated",1,0)
#write.csv(HI3,paste0("C:/Users/hac809/Documents/Pughtam-cropozone/Harvest Index/Harvest_index_outputs/HI2.csv"))
REG_HI<-list()
COR<-list()
cro<-list()
for (j in 1:length(crops)){
  cro[[j]]<-subset(HI3,Crop==crops[j])
  cro[[j]]<-subset(cro[[j]],HILPJ>0.1)
  T1<-cro[[j]]
  T1$GDPpC<-as.numeric(T1$GDPpC)
  T2<-T1[,c(8:10,15:17)]
  T2$lGDP<-log(T2$GDPpC)
  COR[[j]]<-rcorr(as.matrix(T2),type=c("pearson"))
}
#For reference HI
test<-lm(cro[[1]]$HI~poly(cro[[1]]$prec,degree=2)+poly(cro[[1]]$temp,degree=2)+poly(log(cro[[1]]$GDPpC),degree=2)+cro[[1]]$Irr)
summary(test)
test2<-lm(cro[[2]]$HI~poly(cro[[2]]$prec,degree=2)+poly(cro[[2]]$temp,degree=2)+poly(log(cro[[2]]$GDPpC),degree=2)+cro[[2]]$Irr)
summary(test2)
test3<-lm(cro[[3]]$HI~poly(cro[[3]]$prec,degree=2)+poly(cro[[3]]$temp,degree=2)+poly(log(cro[[3]]$GDPpC),degree=2)+cro[[3]]$Irr)
summary(test3)

testt<-lm(cro[[1]]$HI~poly(cro[[1]]$prec,degree=2)+poly(cro[[1]]$temp,degree=2)+poly(cro[[1]]$GDPpC,degree=2)+cro[[1]]$Irr)
summary(testt)
testt2<-lm(cro[[2]]$HI~poly(cro[[2]]$prec,degree=2)+poly(cro[[2]]$temp,degree=2)+poly(cro[[2]]$GDPpC,degree=2)+cro[[2]]$Irr)
summary(testt2)
testt3<-lm(cro[[3]]$HI~poly(cro[[3]]$prec,degree=2)+poly(cro[[3]]$temp,degree=2)+poly(cro[[3]]$GDPpC,degree=2)+cro[[3]]$Irr)
summary(testt3)

#Selecting the models
REG_HI<-list(test,testt2,test3)
COR

Wp<-cro[[1]]$prec^2
Wt<-cro[[1]]$temp^2
Wlg<-log(cro[[1]]$GDPpC)^2
wheat<-lm(cro[[1]]$HI~Wp+cro[[1]]$temp+Wt+log(cro[[1]]$GDPpC)+Wlg)
summary(wheat)

Mp<-cro[[2]]$prec^2
Mt<-cro[[2]]$temp^2
Mlg<-log(cro[[2]]$GDPpC)^2
Maize<-lm(cro[[2]]$HI~cro[[2]]$prec+Mp+cro[[2]]$GDPpC)
summary(Maize)

Rp<-cro[[3]]$prec^2
Rt<-cro[[3]]$temp^2
Rlg<-log(cro[[3]]$GDPpC)^2
Rice<-lm(cro[[3]]$HI~cro[[3]]$prec+Rp+cro[[3]]$temp+Rt+log(cro[[3]]$GDPpC)+Rlg+cro[[3]]$Irr)
summary(Rice)

#For Simulated HI
stest<-lm(cro[[1]]$HILPJ~poly(cro[[1]]$prec,degree=2)+poly(cro[[1]]$temp,degree=2)+poly(log(cro[[1]]$GDPpC),degree=2)+cro[[1]]$Irr)
summary(stest)
stest2<-lm(cro[[2]]$HILPJ~poly(cro[[2]]$prec,degree=2)+poly(cro[[2]]$temp,degree=2)+poly(log(cro[[2]]$GDPpC),degree=2)+cro[[2]]$Irr)
summary(stest2)
stest3<-lm(cro[[3]]$HILPJ~poly(cro[[3]]$prec,degree=2)+poly(cro[[3]]$temp,degree=2)+poly(log(cro[[3]]$GDPpC),degree=2)+cro[[3]]$Irr)
summary(stest3)

stestt<-lm(cro[[1]]$HILPJ~poly(cro[[1]]$prec,degree=2)+poly(cro[[1]]$temp,degree=2)+poly(cro[[1]]$GDPpC,degree=2)+cro[[1]]$Irr)
summary(stestt)
stestt2<-lm(cro[[2]]$HILPJ~poly(cro[[2]]$prec,degree=2)+poly(cro[[2]]$temp,degree=2)+poly(cro[[2]]$GDPpC,degree=2)+cro[[2]]$Irr)
summary(stestt2)
stestt3<-lm(cro[[3]]$HILPJ~poly(cro[[3]]$prec,degree=2)+poly(cro[[3]]$temp,degree=2)+poly(cro[[3]]$GDPpC,degree=2)+cro[[3]]$Irr)
summary(stestt3)

#Selecting the models
REG_HILPJ<-list(stest,stest2,stest3)
COR

Wp<-cro[[1]]$prec^2
Wt<-cro[[1]]$temp^2
Wlg<-(log(cro[[1]]$GDPpC))^2
swheat<-lm(cro[[1]]$HILPJ~Wp+cro[[1]]$prec+cro[[1]]$temp+Wt+log(cro[[1]]$GDPpC)+Wlg+cro[[1]]$Irr)
summary(swheat)

Mp<-cro[[2]]$prec^2
Mt<-cro[[2]]$temp^2
Mlg<-log(cro[[2]]$GDPpC)^2
smaize<-lm(cro[[2]]$HILPJ~Mp+cro[[2]]$prec+cro[[2]]$temp+Mt+log(cro[[2]]$GDPpC)+Mlg+cro[[2]]$Irr)
summary(smaize)

Rp<-cro[[3]]$prec^2
Rt<-cro[[3]]$temp^2
Rlg<-log(cro[[3]]$GDPpC)^2
srice<-lm(cro[[3]]$HILPJ~Rp+cro[[3]]$prec+Rt+log(cro[[3]]$GDPpC)+Rlg+cro[[3]]$Irr)
summary(srice)

