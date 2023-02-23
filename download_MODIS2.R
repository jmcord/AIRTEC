#Script made from failure of MODIS library, based on direct download of links as .txt file from https://search.earthdata.nasa.gov/downloads/3533164295/collections/138782/links

# 
# library(MODIS)
# library(stringr)
# library(lubridate)
# library(stringr)
# library(dplyr)
# 
# year = 2015
# 
# setwd(paste0("C:/GIS/input/",year))
# 
# file = list.files(pattern = "*.txt",full.names=T)
# line = lapply(file,readLines) 
# file = unlist(mapply(rep,file,sapply(line,length),SIMPLIFY=FALSE,USE.NAMES=FALSE))
# df=data.frame(file=file,line=unlist(line))
# 
# links <- list(df[,2])
# 
# for (link in df[,2]) {
#   file2 <- download.file(url = link)
# }
# 
# 




#NKVI


library(MODIS)
library(MODIStsp)
library(stringr)
library(lubridate)


for (i in c(2000:2018)){

setwd("C:/Users/Jose Mar?a/Documents/AIRTEC/PM/NKVI")
dir<-getwd()
year=i
newdir<-paste(dir,year,sep = "/")# dir to save the data
dir.create(newdir,showWarnings = TRUE,recursive = TRUE)


# set MODIS options (these can also be passed to '...' in getHdf())
# MODISoptions(localArcPath = newdir, outDirPath= newdir, quiet = FALSE)
MODISoptions(localArcPath = newdir, quiet = FALSE)


# download data
hdf = getHdf("MOD13A3", collection = "006"
             , tileH = 17, tileV = 4
             , begin = paste0(i,".01.01"), end = paste0(i,".12.31"), checkIntegrity = FALSE)

}

#Change names by year

setwd("C:/Users/Jose Mar?a/Documents/AIRTEC/PM/NKVI")

files <- list.files(pattern = "20")
files

year = 2015
#Aqu? meter?amos un for para recorrer los a?os, pero por ahora lo haremos manualmente

PATH <- paste0("C:/Users/Jose Mar?a/Documents/AIRTEC/PM/NKVI/",files[[16]],"/MODIS/MOD13A3.006/")
setwd(PATH)

files2 <- list.files()

for (i in c(1:length(files2))) {
  setwd(PATH)
  setwd(paste0(files2[i]))
  files3 <- list.files()
  fecha <- ymd(files2[i])
  year <- year(fecha)
  month <- month(fecha)
  nombre <- paste0("maiac_ndvi_h17v04_", month, ".hdf")
  if (length(files3 != 0)) { file.rename(from = files3, to = nombre)}
  else {i = i+1
  print(paste("El d?a",i-1,"no tiene registros"))}
  file.copy(from = paste0(getwd(),"/",files3), to = paste0("C:/GIS/input/NDVI/",year))
}
