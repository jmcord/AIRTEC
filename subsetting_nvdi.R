rm(list=ls())

#GDAL installed according to https://sandbox.idre.ucla.edu/sandbox/tutorials/installing-gdal-for-windows


library(sp)
library(rgdal)
library(gdalUtils)
library(raster)
library(dplyr)
library(stringr)
library(sqldf)

#parent_dir<-"C:/Users/Jose Mar�a/Documents/AIRTEC/PM/Paper_2/GIS/joining"
parent_dir <- "C:/GIS/input/NDVI"
setwd(parent_dir)

#file format:maiac_daily_h17v04_2010001.hdf (i.e. "maiac_daily_h17v04_", year, day, ".hdf")

year=2015

input_dir <-paste(parent_dir,sep="/",year)
output_dir <-paste("C:/GIS/output/NDVI",sep="/",year)


n <- length(list.files(input_dir))
#### read the hdf file and produce 4 R datafiles; 1 for each band ####
for (i in 1:12)
{
  month <- i
  file<-paste0(input_dir,"/maiac_ndvi_h17v04_", month, ".hdf")
  
  if (file.exists(file) == TRUE) {
    
    # Orbit_time_stamp=gdalinfo(file)[64]# names for band 1 to 4, Aqua lunched on 2002 May 4th, before 2002 May 4th there are 2 bands, after that day there are 4 bands
    # print(Orbit_time_stamp)# There are two bands for Terra and two bands for Aqua. 
    # 
    # spl <- str_split(Orbit_time_stamp, "  ")
    # n_bands <- length(spl[[1]]) - 2
    # 
    
    load("C:/GIS/tile_h17v04_reflocation.RData")
    load("C:/GIS/tile_h17v04_geolocation.RData")
    reflocation.x<-matrix(reflocation$X,nrow = 1200,ncol = 1200,byrow = T)
    reflocation.y<-matrix(reflocation$Y,nrow = 1200,ncol = 1200,byrow = T)
    geolocation.long<-matrix(geolocation$long,nrow = 1200,ncol = 1200,byrow = T)
    geolocation.lat<-matrix(geolocation$lat,nrow = 1200,ncol = 1200,byrow = T)
    
    for (j in 1:n_bands)# j is the band number, before 2002 May 4th, set to (i in 1:2)
    {
      maiac.daily<-data.frame(NDVI=numeric(),X=numeric(),Y=numeric(),Lon=numeric(),Lat=numeric())
      gdal_chooseInstallation()
      gdal_translate(file,paste(parent_dir,outsuffix ="AOT.tif",sep="/"),of="GTiff",sds=T,output_Raster=TRUE)
      subsets<-list.files(parent_dir,"tif")
      maiac.image1k<-stack(subsets[1:8])
      maiac.image5k<-stack(subsets[9:13])
      maiac.resample<-resample(maiac.image5k,maiac.image1k,method='bilinear')
      maiac.stack<-stack(maiac.image1k,maiac.resample)
      maiac.tile<-cbind.data.frame(as.numeric(as.matrix(maiac.stack$AOT_01)),
                                   as.numeric(as.matrix(maiac.stack$AOT_02)),
                                   as.numeric(as.matrix(maiac.stack$AOT_03)),
                                   as.numeric(as.matrix(maiac.stack$AOT_04)),
                                   as.numeric(as.matrix(maiac.stack$AOT_05)),
                                   as.numeric(as.matrix(maiac.stack$AOT_06)),
                                   as.numeric(as.matrix(maiac.stack$AOT_07)),
                                   as.numeric(as.matrix(maiac.stack$AOT_08)),
                                   as.numeric(as.matrix(maiac.stack$AOT_09)),
                                   as.numeric(as.matrix(maiac.stack$AOT_10)),
                                   as.numeric(as.matrix(maiac.stack$AOT_11)),
                                   as.numeric(as.matrix(maiac.stack$AOT_12)),
                                   as.numeric(as.matrix(maiac.stack$AOT_13)),
                                   as.numeric(reflocation.x),as.numeric(reflocation.y),as.numeric(geolocation.long),as.numeric(geolocation.lat))
      names(maiac.tile)<-names(maiac.daily)
      maiac.daily<-rbind(maiac.daily,maiac.tile)
      #file.remove(list.files(paste(parent_dir,sep ="/"),pattern = "AOT_",recursive = T,full.names = T))
      #  save(maiac.daily,file =paste0("maiac_daily",tiles,"2003030","_",i,".RData"))#final rdata, band refer to Orbit_time_stamp
      write.csv(maiac.daily,file =paste0(output_dir,"/maiac_daily_h17v04_",year,day,"_",j,".txt")) #final rdata, band refer to Orbit_time_stamp
      rm(maiac.daily)
      # you can use AOT_047 OR AOT_055 as the AOD value. There are two bands for each satellites, we usually choose the max values
    }
  }
  else {print(paste("no hay datos para el d�a",i)) 
    next}
}



# Mapeado con CMAQ


cmaq_id <- read.csv("assign_CMAQ-MAIAC.csv")

for (i in 360:365)
{
  day<-formatC(i, width=3, flag="0")
  
  file<-paste0(input_dir,"/maiac_daily_h17v04_", year, day, ".hdf")
  
  Orbit_time_stamp=gdalinfo(file)[64]# names for band 1 to 4, Aqua lunched on 2002 May 4th, before 2002 May 4th there are 2 bands, after that day there are 4 bands
  print(Orbit_time_stamp)# There are two bands for Terra and two bands for Aqua. 
  
  spl <- str_split(Orbit_time_stamp, "  ")
  n_bands <- length(spl[[1]]) - 2
  
  
  if (n_bands > 0) {
    
    for (j in 1:n_bands)
    {
      file<-paste0(output_dir,"/maiac_daily_h17v04_", year, day, "_", j,".txt")
      completo <- read.csv(file=file, sep=",")
      
      names(completo)[names(completo) == 'X.1'] <- 'ID_MAIAC'
      
      completo_m <- sqldf('select a.*, b.*
                        from cmaq_id As a
                        left join completo As b
                        on a.ID_MAIAC = b.ID_MAIAC
                        ')
      
      name<-paste0(output_dir,"/maiac_daily_Madrid_", year, day, "_", j,".txt")
      
      write.csv(completo_m, file = name, row.names = F, quote= F)
    }
  }
  else print(paste0("No hay datos en el d�a ",i))
}

