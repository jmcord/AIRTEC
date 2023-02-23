rm(list=ls())

PATH <- 'C:/Users/jcord/Documents/AIRTEC/PM/NetCDF'
setwd(PATH)

library(RNetCDF)
library(ncdf4)

# Tiene 26880 celdas


#PARA UN ARCHIVO


#RNetCDF

dataset <- open.nc(con = 'wrfout_d01_2015-02-01_00', write=FALSE, share=FALSE, prefill=TRUE)

# data <- read.nc(dataset, recursive=FALSE)

file.inq.nc(dataset)

dim.inq.nc(dataset, dimension = 1)

var.inq.nc(dataset, variable = 'PSFC')

SLP <- var.get.nc(dataset, variable = 'PSFC')
T <- var.get.nc(dataset, variable = 'T2')
ele <- var.get.nc(dataset, variable = 'HGT')
lat <- var.get.nc(dataset, variable = 'XLAT')
lon <- var.get.nc(dataset, variable = 'XLONG')
times <- var.get.nc(dataset, variable = 'Times')
LU_index <- var.get.nc(dataset, variable = 'LU_INDEX')


#ncdf4

dataset <- nc_open('wrfout_d01_2015-02-01_00')
variables = names(dataset[['var']])
variables

capture.output(print(dataset), file = "info.txt")

#Prepare data

datos_final <- cbind(lon,lat,T,SLP,ele,times,LU_index) %>% data.frame()
datos_final$date <- times

n_grid <- length(times)
dates_grids <- vector()


for (i in c(1:n_grid)) {
  date_grid <- times[i]
  temp <- rep(date_grid, 26880)
  dates_grids <- append(dates_grids, temp)
}

datos_final$date <- dates_grids

dir.create("C:/Users/jcord/Documents/AIRTEC/PM/LU_index", recursive = TRUE)
write.csv(datos_final,"C:/Users/jcord/Documents/AIRTEC/PM/LU_index/LU_index.csv")



#PARA UN AÑO


PATH <- 'E:/NetCDF/2015/'
setwd(PATH)



files <- list.files()

netcdf_2015 <- data.frame()

netcdf <- lapply(files, function(x) {
  dataset <- open.nc(con = x, write=FALSE, share=FALSE, prefill=TRUE)
  
  SLP <- var.get.nc(dataset, variable = 'PSFC')
  T <- var.get.nc(dataset, variable = 'T2')
  ele <- var.get.nc(dataset, variable = 'HGT')
  lat <- var.get.nc(dataset, variable = 'XLAT')
  lon <- var.get.nc(dataset, variable = 'XLONG')
  times <- var.get.nc(dataset, variable = 'Times')
  
  datos_final <- cbind(lon,lat,T,SLP,ele) %>% data.frame()
  datos_final$date <- times
  
  n_grid <- length(times)
  dates_grids <- vector()
  
  
  for (i in c(1:n_grid)) {
    date_grid <- times[i]
    temp <- rep(date_grid, 26880)
    dates_grids <- append(dates_grids, temp)
  }
  
  datos_final$date <- dates_grids
  
  netcdf_2015 <- rbind(netcdf_2015, datos_final)
  
  }
  )
























#Pruebas

p <- datos_final[, colnames(datos_final) %in% c('lon','lat','T')]
write.csv(p[c(26881:53760),], 'prueba_GIS.csv')

datos_final_netcdf$cord <- paste0(round(datos_final_netcdf$lon,5),"_",round(datos_final_netcdf$lat,5))

cord <- "-3.51199_40.34633"
fecha <- as.Date("2015-02-03")

datos_final_netcdf$fecha <- str_sub(datos_final_netcdf$date, 1L, 10L) %>% ymd()

datos_final_netcdf <- sqldf('select a.lon,a.lat,avg(a.T) As T,avg(a.SLP) As SLP, avg(a.ele) As ele, a.date, a.cord, a.fecha
                   from datos_final_netcdf As a
                   group by a.fecha, a.cord')


which(datos_final_netcdf$cord == cord & datos_final_netcdf$fecha == fecha)

datos_final_netcdf_prueba <- datos_final_netcdf[which(datos_final_netcdf$cord == cord & datos_final_netcdf$fecha == fecha),]

which(datos_final$cord == cord & datos_final$fecha == fecha)

datos_agregados_prueba <- datos_final[which(datos_final$cord == cord & datos_final$fecha == fecha),]
