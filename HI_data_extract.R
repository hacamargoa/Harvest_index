library(raster)
library(ncdf4)
library(rworldmap)
library(plyr)
library(dplyr)
library(zoo)
library(Hmisc)
#Selecting the right Wheat and Maize HI
setwd("C:/users/hac809/Documents/output/")
HI_LPJ<-read.table("HI.out", h=T)
HI_LPJ=subset(HI_LPJ,Year>=1974)
#yield=read.table('yield.out', h=TRUE)
countries<-as.data.frame(gridCountriesDegreesHalf)
names(countries)<-c("UN","Lon","Lat")
HI_LPJ<-join(HI_LPJ,countries)
MaizCult<-read.csv("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAO_MaizBC.csv",h=T)
MaizCult$Cultivar<-ifelse(MaizCult$FYield>0.055*(MaizCult$Year-1960)+4.15,1,2)
MaizCult<-MaizCult[c(1,3,7)]
MaizCult<-subset(MaizCult,!is.na(UN)&UN!=-99)
MaizCult<-distinct(MaizCult)
HI_LPJ<-join(HI_LPJ,MaizCult)
HI_LPJ=na.locf(HI_LPJ,fromLast = TRUE)
HI_LPJ2=HI_LPJ
HI_LPJ2$TeCo<-ifelse(HI_LPJ2$Cultivar==2,HI_LPJ2$TeCG,HI_LPJ2$TeCS)
HI_LPJ2$TeCoi<-ifelse(HI_LPJ2$Cultivar==2,HI_LPJ2$TeCGi,HI_LPJ2$TeCSi)
HI_LPJ2<-HI_LPJ2[,c(1,2,3,15,4,5,17,8,9,10,18,13,14)]

yield=read.table('yield.out', h=TRUE)
yield1=subset(yield[c(1:5,9,10)],Year>=1974)
vect<-c(1974:2010)
li<-list()
for (i in 1:length(vect)){
  li[[i]]<-subset(yield1,Year==vect[i])
  li[[i]]<-li[[i]][c(-3)]
}

wheat<-names(li[[i]])[c(3,4,5,6)]
bb <- extent(-180, 180, -90, 90)
rasterize<-function(coord,value){
  areas<-SpatialPointsDataFrame(coords=coord,data.frame(value),proj4string = CRS("+proj=longlat +datum=WGS84"))
  areas2<-SpatialPixelsDataFrame(areas,tolerance=0.0192077,areas@data) 
  areas3<-as(areas2,"SpatialGridDataFrame")
  Rstr<-raster(areas3)
}
rastcrop<-list()
for(j in 1:length(wheat)){
  crop_r<-list()
  for (i in 1:length(vect)){
    yield_coords<-cbind(li[[i]]$Lon,li[[i]]$Lat)
    crop_r[[i]]<-rasterize(yield_coords,li[[i]][wheat[j]])
  }
  names(crop_r)<-paste0("Yi_",vect)
  rastcrop[[j]]<-stack(crop_r)
  rastcrop[[j]]<-extend(rastcrop[[j]],bb)
}
names(rastcrop)<-paste0(wheat)

#Calling Ray files, if not available see the end of this script to call the original files
setwd("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs")
ArNewW<-brick("./Area/SPAMest_Wheat_1960_2010.nc")
ArNewWi<-brick("./Area/SPAMest_Wheatirr_1960_2010.nc")
ArNewWi[is.na(ArNewWi)]<-0
ArNewWr<-ArNewW-ArNewWi
crop_rayW<-brick("Ray_Yield_Wheat_newcrops_1970-2010.nc")

#Wheatprod<-((rastcrop[[2]]*ArNewr[[1]]+rastcrop[[1]]*ArNewi[[1]])*10/0.88)
WWprod<-((rastcrop[[1]]*ArNewWr[[c(14:50)]]+rastcrop[[2]]*ArNewWi[[c(14:50)]])*10/0.88)
SWprod<-((rastcrop[[3]]*ArNewWr[[c(14:50)]]+rastcrop[[4]]*ArNewWi[[c(14:50)]])*10/0.88)
WWcorr<- stack(WWprod,crop_rayW[[c(5:41)]])
WWcorr1<- calc(WWcorr, fun=function(x) cor(x[1:37], x[38:74], method='pearson'))
SWcorr<- stack(SWprod,crop_rayW[[c(5:41)]])
SWcorr1<- calc(SWcorr, fun=function(x) cor(x[1:37], x[38:74], method='pearson'))
Scomp<-overlay(SWcorr1,WWcorr1,fun=function(x,y) {ifelse(x>y,1,0)})
Wcomp<-overlay(WWcorr1,SWcorr1,fun=function(x,y) {ifelse(x>y,2,0)})
Wheatcult<-Wcomp+Scomp
Wheatcultdf<-as.data.frame(Wheatcult,xy=TRUE)
colnames(Wheatcultdf)<-c("Lon","Lat","Wcult")
HI_LPJ2<-join(HI_LPJ2,Wheatcultdf)
HI_LPJ3=na.locf(HI_LPJ2,fromLast = TRUE)
HI_LPJ3$TeW<-ifelse(HI_LPJ3$Wcult==2,HI_LPJ3$TeWW,HI_LPJ3$TeSW)
HI_LPJ3$TeWi<-ifelse(HI_LPJ3$Wcult==2,HI_LPJ3$TeWWi,HI_LPJ3$TeSWi)
HI_LPJ3<-HI_LPJ3[c(1:3,15,16,7,11,8,12,13)]

#Rasterizing the HI file from LPJ-GUESS
HI_LPJRa<-list()
HI_LPJ_<-list()
bb <- extent(-180, 180, -90, 90)
for(j in 1:7){
  Temp1<-HI_LPJ3[c(1,2,3,j+3)]
  for(i in 1:42){
    Temp2<-subset(Temp1,Year==1973+i)
    Temp2<-Temp2[-3]
    coordinates(Temp2) <- ~ Lon + Lat
    gridded(Temp2) <- TRUE  
    HI_LPJ_[[i]]<-raster(Temp2)
    HI_LPJ_[[i]] <-extend(HI_LPJ_[[i]],bb)
  }
  HI_LPJRa[[j]]<-brick(HI_LPJ_)
}
names(HI_LPJRa)<-colnames(HI_LPJ3[-c(1:3)])

#extracting the info
HI<-read.csv("C:/Users/hac809/Documents/Pughtam-cropozone/Harvest Index/HI_database.csv",sep=",",h=T)
HI2<-HI
HI2$year<-ifelse(HI2$year>2015,2015,HI2$year)
for(i in 1:nrow(HI2)){
  a<-HI2$year[i]-1973
  co<-cellFromXY(HI_LPJRa[[1]],c(HI2$Longitude[i],HI2$Latitude[i]))
  HI2$HILPJ[i]<-ifelse(HI2$Crop[i]=="Rice",extract(HI_LPJRa[[7]][[a]],co),
                       ifelse(HI2$Crop[i]=="Maize",ifelse(HI2$Irrigation[i]=="Irrigated",extract(HI_LPJRa[[4]][[a]],co),
                                                       ifelse(HI2$Irrigation[i]=="Rainfed",extract(HI_LPJRa[[3]][[a]],co),
                                                              (extract(HI_LPJRa[[4]][[a]],co)+extract(HI_LPJRa[[3]][[a]],co))/2)),
                                     ifelse(HI2$Irrigation[i]=="Irrigated",extract(HI_LPJRa[[2]][[a]],co),
                                            ifelse(HI2$Irrigation[i]=="Rainfed",extract(HI_LPJRa[[1]][[a]],co),
                                                   (extract(HI_LPJRa[[2]][[a]],co)+extract(HI_LPJRa[[1]][[a]],co))/2))))
}

HI_<-list()
crops<-c("Wheat","Maize","Rice")
X11(width=8,height=8)
par(mfrow=c(3,2),omi=c(0.3,0.2,0.1,0.1),mai=c(0.5,0.6,0.3,0.1))
for (j in 1:length(crops)){
  HI_[[j]]<-subset(HI2,Crop==crops[j])
  HI_[[j]]<-subset(HI_[[j]],HILPJ>0.1)
  lim<-c(min(min(HI_[[j]]$HILPJ),min(HI_[[j]]$HI)),max(max(HI_[[j]]$HILPJ),max(HI_[[j]]$HI)))
  plot(HI_[[j]]$HILPJ,HI_[[j]]$HI,xlim=lim,
       ylim=lim,ylab="", xlab="",cex.axis=1.5)
  abline(0,1)
  title( main =paste0("HI  ",crops[j]), line=0.9, cex.main=2.3,font.main=2)
  title(ylab="Observed HI", xlab=ifelse(j==3,"LPJ-GUESS HI",""),cex.lab=1.5,font.lab=2,line=2.5)
  temp1<-data.frame(index="LPJ-GUESS", HI=HI_[[j]]$HILPJ)
  temp2<-data.frame(index="Reference", HI=HI_[[j]]$HI)
  temp<-rbind(temp1,temp2)
  boxplot(HI~index,data=temp, xlab=" ", ylab= " ", cex.axis=1.5)
  title(main=paste0("Boxplot ", crops[j]), line=0.9, cex.main=2.3,font.main=2,)
  title(ylab="Harvest Index", xlab=" ",cex.lab=1.5,font.lab=2,  line=2.5)
}
perc<-list()
for (i in 1:3){
perc[[i]]<-quantile(HI_[[i]]$HI,c(0.05,0.95))
}
