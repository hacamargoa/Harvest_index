library(raster)

#Selecting the right Wheat HI
HI_LPJ<-read.table("../Harvest_index_inputs/HI.out", h=T)
HI_LPJ=subset(HI_LPJ,Year>=1974)
HI_LPJ2<-HI_LPJ[c(1:3,4,8,5,9,6,10,12)]

#Rasterizing the HI file from LPJ-GUESS
HI_LPJRa<-list()
HI_LPJ3<-list()
bb <- extent(-180, 180, -90, 90)
for(j in 1:7){
  Temp1<-HI_LPJ2[c(1,2,3,j+3)]
  for(i in 1:42){
    Temp2<-subset(Temp1,Year==1973+i)
    Temp2<-Temp2[-3]
    coordinates(Temp2) <- ~ Lon + Lat
    gridded(Temp2) <- TRUE  
    HI_LPJ3[[i]]<-raster(Temp2)
    HI_LPJ3[[i]] <-extend(HI_LPJ3[[i]],bb)
  }
  HI_LPJRa[[j]]<-brick(HI_LPJ3)
}

#extracting the info
HI<-read.csv("../../Harvest_Index/HI_database.csv",sep=",",h=T)
HI2<-HI
HI2$year<-ifelse(HI2$year>2015,2015,HI2$year)
for(i in 1:nrow(HI2)){
  a<-HI2$year[i]-1973
  co<-cellFromXY(HI_LPJRa[[1]],c(HI2$Longitude[i],HI2$Latitude[i]))
  HI2$HILPJ[i]<-ifelse(HI2$Crop[i]=="Rice",extract(HI_LPJRa[[7]][[a]],co),
                       ifelse(HI2$Crop[i]=="Maize",ifelse(HI2$Irrigation[i]=="Irrigated",extract(HI_LPJRa[[6]][[a]],co),
                                                       ifelse(HI2$Irrigation[i]=="Rainfed",extract(HI_LPJRa[[5]][[a]],co),
                                                              (extract(HI_LPJRa[[6]][[a]],co)+extract(HI_LPJRa[[5]][[a]],co))/2)),
                              ifelse(HI2$Cult[i]=="Spring",ifelse(HI2$Irrigation[i]=="Irrigated",extract(HI_LPJRa[[4]][[a]],co),
                                                               ifelse(HI2$Irrigation[i]=="Rainfed",extract(HI_LPJRa[[3]][[a]],co),
                                                                      (extract(HI_LPJRa[[4]][[a]],co)+extract(HI_LPJRa[[3]][[a]],co))/2)),
                                     ifelse(HI2$Irrigation[i]=="Irrigated",extract(HI_LPJRa[[2]][[a]],co),
                                            ifelse(HI2$Irrigation[i]=="Rainfed",extract(HI_LPJRa[[1]][[a]],co),
                                                   (extract(HI_LPJRa[[2]][[a]],co)+extract(HI_LPJRa[[1]][[a]],co))/2)))))
}
write.csv(HI2,paste0("../Harvest_index_outputs/HI2.csv"))

HI_<-list()
crops<-c("Wheat","Maize","Rice")
par(mfrow=c(3,1))
for (j in 1:length(crops)){
  HI_[[j]]<-subset(HI2,Crop==crops[j])
  HI_[[j]]<-subset(HI_[[j]],HILPJ>0.2)
  plot(HI_[[j]]$HILPJ,HI_[[j]]$HI, ylab="Observed HI", xlab="LPJ-GUESS HI",title(main=crops[j]),xlim=c(0.25,0.85))
  abline(0,1)
    }                                     
        

