#script taken from "https://gis.stackexchange.com/questions/295080/download-modis-mcd19a2-aod-product-with-r"
#based on the MODIS package
#It made me install MRT; that in turn, required JAVA

library(MODIS)
setwd("D:/R-analyses/MAIAC/download")
dir<-getwd()
year=2012
newdir<-paste(dir,year,sep = "/")# dir to save the data
dir.create(newdir,showWarnings = TRUE,recursive = TRUE)

# set MODIS options (these can also be passed to '...' in getHdf())
# MODISoptions(localArcPath = newdir, outDirPath= newdir, quiet = FALSE)
MODISoptions(localArcPath = newdir, quiet = FALSE)

# download data
hdf = getHdf("MCD19A2", collection = "006"
             , tileH = 17, tileV = 4
             , begin = "2012.01.01", end = "2012.12.31")
hdf


