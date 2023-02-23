library(iterators)
library(parallel)
library(foreach)
library(doParallel)
rm(list=ls())
gc()
cl<-makeCluster(15)
registerDoParallel(cl)
foreach (year = 2000:2014)%dopar% #study period
{
  newdir<-paste("E:/Middle_East_Study/Africa_MAIAC/h21v06",year,sep = "/")# dir to save the data
  dir.create(newdir,showWarnings = TRUE,recursive = TRUE)
  AOT <- paste('ftp://maiac@dataportal.nccs.nasa.gov/DataRelease/Africa/h21v06',year,sep = '/')#tile h21v06
  AOT<-paste(AOT,"/",sep="")
  library(bitops)
  library(RCurl)
  items<- strsplit(getURL(AOT), "\r\n")[[1]]
  filenames<-substr(items,58,102)
  for(i in 1:length(filenames))
  {
    download.file(paste(AOT,filenames[i],sep =""),paste(newdir,filenames[i],sep ="/"),mode="wb")
  }
  stopCluster(cl)
  rm(cl)
}

