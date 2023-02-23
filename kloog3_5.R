
rm(list=ls())

year = 2015

#Celdas MAIAC = 1760 (pero ahora sólo tenemos 1759, faltando la 396) #Celdas WRF = 26880



#Eq(1)



library(sqldf)

library(tidyr)

library(stringr)

library(lubridate)

library(neuralnet)

library(dplyr)

library(caret)

library(DMwR)

library(lme4)

library(readxl)

library(readr)

#### funciones ####



MSE <- function(x,y)
  
{
  
  (sum((y-x)^2)/length(x))
  
}







RMSE <- function(x,y)
  
{
  
  (sum((y-x)^2)/length(x))^0.5
  
}



MAE <- function(x,y)
  
{
  
  (1/(length(x)))*sum(abs(x-y))
  
}



MAPE <- function(x,y)
  
{
  
  (100/(length(x)))*sum(abs((x-y)/y))
  
}



MBE <- function(x,y)
  
{
  
  (1/(length(x)))*sum(x-y)
  
}

#### end ####









year <- 2015



PATH = "C:/Users/jcord/Documents/AIRTEC/PM/"

setwd(PATH)


# 
# data <- read.csv(paste0("C:/GIS/output_final/MAIAC_2015.csv"))
# 
# join_data <- read.csv("C:/Users/jcord/Documents/AIRTEC/PM/NetCDF/assign_CMAQ-MAIAC.csv")
# 
# join_data_wrf <- read.csv("C:/Users/jcord/Documents/AIRTEC/PM/NetCDF/Mapeo_Lat_Lon_WRF_CMAQ_ID.csv", sep = ";")
# 
# join_data_wrf <- join_data_wrf[, colnames(join_data_wrf) %in% c("ID","lon_WRF","lat_WRF","CMAQ_ID")]
# 
# 
# 
# data <- sqldf('select a.*, b.D1KMAY_ID
# 
#                from data As a
# 
#                left join join_data As b
# 
#                on a.X = b.X and a.Y = b.Y' )
# 
# 
# 
# data$AOD <- data[colnames(data) %in%
#                    
#                    c("AOD_47_A","AOD_55_A","AOD_47_T","AOD_55_T")] %>% apply(1, max)
# 
# 
# 
# data$AOD <- ifelse(data$AOD == -Inf, NA, data$AOD)
# 
# data$haveAOD <- ifelse(is.na(data$AOD) == TRUE, 0, 1)
# 
# 
# 
# data$date <- as.character(data$date)
# 
# data$year <- str_sub(data$date, 1L, 4L) #%>% year() data$day <- str_sub(data$date, 5L, 8L) %>% as.numeric() -1 data$fecha <- as.Date(data$day, origin = paste0(year,"-01-01"))
# 
# 
# 
# 
# 
# #datos_wrf <- read.csv("G:/netcdf_2015_ampli.csv")
# 
# #datos_wrf <- WRF_2015
# 
# 
# 
# 
# 
# datos_wrf$cord <-
#   
#   paste0(round(datos_wrf$lon,5),"_",round(datos_wrf$lat,5))
# 
# datos_wrf$cord <- factor(datos_wrf$cord)
# 
# 
# 
# datos_wrf$fecha <- str_sub(datos_wrf$date, 1L, 10L) %>% ymd()
# 
# datos_wrf <- na.omit(datos_wrf)
# 
# 
# 
# datos_wrf <- sqldf('select avg(a.T) As T,avg(a.SLP) As SLP,
# 
#                     avg(a.ele) As ele,
# 
#                     avg(a.s_hum) As s_hum, avg(a.u_wind) As u_wind,
# 
# avg(a.v_wind) As v_wind,
# 
#                     avg(a.PBL) As PBL, avg(a.veg_cat) As veg_cat,
# 
# avg(a.veg_fraction) As veg_fraction, a.cord, a.fecha
# 
#                     from datos_wrf As a
# 
#                     group by a.fecha, a.cord')
# 
# 
# 
# 
# 
# 
# 
# join_data_wrf$cord <-
#   
#   paste0(round(join_data_wrf$lon_WRF,5),"_",round(join_data_wrf$lat_WRF,5))
# 
# 
# 
# 
# 
# datos_wrf_2 <- sqldf('select a.*,b.*,a.cord
# 
#                     from datos_wrf As a
# 
#                     left join join_data_wrf As b
# 
#                     on a.cord = b.cord')
# 
# 
# 
# 
# datos_final <- sqldf('select a.*, b.*, a.fecha
# 
#                        from data As a
# 
#                        left join datos_wrf_2 As b
# 
#                        on a.fecha = b.fecha and a.D1KMAY_ID = b.CMAQ_ID')
# 
# 
# 
# 
# 
# datos_final <- datos_final[, colnames(datos_final) %in%
#                              
#                              c("fecha","cord","lon","lat","lon_WRF","lat_WRF","D1KMAY_ID","CMAQ_ID","AOD","haveAOD","T","SLP","ele",
#                                
#                                
#                                
#                                "s_hum","u_wind","v_wind","PBL","veg_cat","veg_fraction")]
# 
# 
# 
# which_na <- which(is.na(datos_final$lon_WRF))
# 
# 
# 
# datos_final <- datos_final[-which_na, ]
# 
# 
# 
# datos_final$Month <- month(datos_final$fecha)
# 
# 
# 
# data <- datos_final
# 
# 
# 
# write.csv(data, "../datos_model_2015_ampli.csv")
# 
# 
# 
# library(ipw)
# 
# ipwmodel <- ipwpoint(exposure = haveAOD, family = "binomial", link =
#                        
#                        "logit",# haveAOD is 0 or 1, have AOD=1, missingAOD=0
#                      
#                      numerator = ~Month, denominator = ~ ele+SLP+T, data
#                      
#                      = data)# I am not sure if this formular is correct. Please refer to the help document of library(ipw)
# 
# data$ipw <- ipwmodel$ipw.weights
# 
# data$adjustAOD=data$AOD*data$ipw
# 
# 
# 
# #write.csv(data, "C:/Users/Jose María/Documents/AIRTEC/PM/data_model_2015_1_step_apli.csv")
# 
# data <- read.csv("C:/Users/jcord/Documents/AIRTEC/PM/data_model_2015_1_step_apli.csv")
# 
# #data <- read.csv("F:/GIS/data_model_2015_1_step_ampli.csv")
# 
# 
# 
# 
# 
# #### ADD NVDI ####
# 
# 
# 
# setwd("C:/Users/jcord/Documents/AIRTEC/PM/NDVI/2015")
# 
# files <- list.files(pattern=".csv")
# 
# 
# 
# meses <- lapply(files, function(x) {y = str_split(x, pattern="_",n=3)
# 
# y[[1]][2]})
# 
# 
# 
# files <- lapply(files, read.csv)
# 
# 
# 
# j <- 1
# 
# for (i in c(1:length(files))){
#   
#   df <- files[[i]]
#   
#   df$month <- meses[[j]]
#   
#   j <- j + 1
#   
#   files[[i]] <- df
#   
# }
# 
# 
# 
# 
# 
# 
# 
# nvdi <- bind_rows(files)
# 
# 
# 
# data <- sqldf('select a.*, b.maiac_ndvi As NDVI
# 
#                from data As a
# 
#                left join nvdi As b
# 
#                on a.D1KMAY_ID = b.D1KMAY_ID and a.Month = b.month')
# 
# 
# 
# 
# 
# #### END NVDI ####
# 
# 
# 
# 
# 
# #### ADD LU_INDEX ####
# 
# 
# 
# setwd("C:/Users/jcord/Documents/AIRTEC/PM/LU_index/")
# 
# files <- list.files(pattern=".csv")
# 
# 
# 
# files <- lapply(files, read.csv)
# 
# 
# 
# LU <- files[[2]]
# 
# LU=LU[LU$date == "2015-02-01_01:00:00",]
# 
# LU$lon <- round(LU$lon, 5)
# 
# LU$lat <- round(LU$lat, 5)
# 
# #LU$cord <- paste0(round(LU$lon,5),"_",round(LU$lat,5))
# 
# 
# join_data_wrf$cord <-
#   
#   paste0(round(join_data_wrf$lon,5),"_",round(join_data_wrf$lat,5))
# 
# join_data_wrf$cord <- factor(join_data_wrf$cord)
# 
# 
# LU$fecha <- ymd_hms(LU$times)
# LU$day <- day(LU$fecha)
# #LU <- LU[LU$day == 1, ]
# 
# datos_final <- sqldf('select a.*, b.*, a.fecha
# 
#                        from data As a
# 
#                        left join LU As b
# 
#                        on a.cord = b.cord')
# 
# 
# LU$cord <- paste0(round(LU$lon,5),"_",round(LU$lat,5))
# 
# LU$cord <- factor(LU$cord)
# 
# 
# 
# LU <- sqldf('select a.*, b.*
# 
#             from LU As a
# 
#             left join join_data_wrf As b
# 
#             on a.cord = b.cord')
# 
# 
# 
# LU <- na.omit(LU)
# 
# LU$dayofyear <- yday(LU$fecha)
# 
# data$dayofyear <- yday(data$fecha)
# 
# data <- data[,-26]
# 
# #LU <- LU[LU$dayofyear == 32,]
# 
# #LU$hour <- hour(LU$fecha)
# 
# #LU <- LU[LU$hour == 12, ]
# 
# LU <- LU[,-17]
# 
# data_lu <- sqldf('select a.*, b.LU_index As LU_index
# 
#                from data As a
# 
#                left join LU As b
# 
#                on a.cord = b.cord and a.D1KMAY_ID = b.CMAQ_ID')
# 
# 
# write.csv(data_lu,"data_lu.csv")
# 
# 
# #### END LU_INDEX ####
# 
# 
# 
# 
# 
# #### ADD population ####
# 
# 
# 
# setwd("C:/Users/jcord/Documents/AIRTEC/PM/Population")
# 
# files <- list.files(pattern=".csv")
# 
# 
# 
# files <- lapply(files, read.csv)
# 
# 
# 
# pop <- files[[3]]
# 
# 
# 
# pop$lon <- round(pop$Lon_1, 5)
# 
# pop$lat <- round(pop$Lat_1, 5)
# 
# 
# 
# join_data_wrf$cord <-
#   
#   paste0(round(join_data_wrf$lon,5),"_",round(join_data_wrf$lat,5))
# 
# join_data_wrf$cord <- factor(join_data_wrf$cord)
# 
# 
# 
# pop$cord <- paste0(round(pop$lon,5),"_",round(pop$lat,5))
# 
# pop$cord <- factor(pop$cord)
# 
# 
# 
# 
# 
# data <- sqldf('select a.*, b.Surr_Pobl As pop_index
# 
#                from data As a
# 
#                left join pop As b
# 
#                on a.D1KMAY_ID = b.D1KMAY_ID')
# 
# 
# 
# 
# 
# #### END population ####
# 
# 
# 
# 
# 
# #### ADD trafic ####
# 
# 
# 
# data_add <- read.csv("C:/GIS/datos_MAIAC_2015_menos_traffic.csv") ###CUIDADO, usar data en vez de esto, corregir!
# 
# traffic <- read.csv("C:/Users/jcord/Documents/AIRTEC/PM/traffic/traffic_final_GIS.csv")
# 
# 
# 
# data_add <- sqldf('select a.*, b.VH_KM_sum As VH_KM
# 
#                from data As a
# 
#                left join traffic As b
# 
#                on a.D1KMAY_ID = b.CMAQ_ID')
# 
# 
# 
# #write.csv(data_add,"C:/GIS/datos_MAIAC_2015_con_traffic.csv")
# 
# 
# 
# #### END trafic ####
# 
# 
# 

data <- read.csv("C:/Users/jcord/Documents/AIRTEC/PM/data_model_2015_1_step_apli.csv")


####FOR LOOP FOR IPW MODELS and ADD PM 2.5 ####

calculate_distance <- function(df, lon_s, lat_s) {
  df$d = 2*6371*asin(sqrt((sin((df$lon_WRF-lon_s)/2)^2)+cos(df$lon_WRF)*cos(lon_s)*sin((df$lat_WRF-lat_s)/2)^2))
  df
}

data <- read.csv("C:/Users/jcord/Documents/AIRTEC/PM/data_model_2015_1_step_apli.csv")
dff <- data
# df_year_2015_average_without_loop <- sqldf('select a.lon_WRF, a.lat_WRF, a.D1KMAY_ID, avg(a.ipw) As ipw
#                                 from data As a
#                                 group by a.D1KMAY_ID')
# 
# #IPW histograms
# 
# data_sierra <- data[data$D1KMAY_ID == 1682, ]
# hist(data_sierra$ipw, breaks = 20)
# 
# data_madrid <- data[data$D1KMAY_ID == 896, ]
# hist(data_madrid$ipw, breaks = 20)
# 
# # write.csv(df_year_2015_average_without_loop, "C:/Users/jcord/Documents/AIRTEC/PM/NetCDF/df_year_2015_average_without_loop.csv",
# #           row.names = FALSE)
# 

# 
# str(data)
# 
# library(ipw)
# 
# data$cord <- as.character(data$cord)
# 
# colnames_new_df <- colnames(data)
# 
# dff <- data.frame(matrix(ncol = 25, nrow = 0))
# colnames(dff) <- colnames_new_df
# 
# 
# models_list <- vector()
# for (cord in unique(data$cord)) {
#   print(cord)
#   df = data[data$cord == cord, ]
#   
#   ipwmodel <- ipwpoint(exposure = haveAOD, family = "binomial", link =
#                          "logit",# haveAOD is 0 or 1, have AOD=1, missingAOD=0
#                        numerator = ~Month, denominator = ~ ele+SLP+T, data
#                        = df)# I am not sure if this formular is correct. Please refer to the help document of library(ipw)
#   
#   df$ipw <- ipwmodel$ipw.weights
#   
#   df$adjustAOD=df$AOD*df$ipw
#   
#   dff = rbind(dff, df)
#   
# }
# 
# #IPW histograms
# 
# dff_sierra <- dff[dff$D1KMAY_ID == 1682, ]
# hist(dff_sierra$ipw, breaks = 20)
# 
# dff_madrid <- dff[dff$D1KMAY_ID == 896, ]
# hist(dff_madrid$ipw, breaks = 20)
# 
# #calculate 2015 average
# 
# 
# df_year_2015_average <- sqldf('select a.lon_WRF, a.lat_WRF, a.D1KMAY_ID, avg(a.ipw) As ipw
#                                 from dff As a
#                                 group by a.D1KMAY_ID')
# 
# 
# df_year_2015_average$ipw <- as.double(df_year_2015_average$ipw)
# str(df_year_2015_average)

#write.csv(df_year_2015_average, "C:/Users/jcord/Documents/AIRTEC/PM/NetCDF/df_year_2015_average.csv")

setwd("C:/Users/jcord/Documents/AIRTEC/PM/")

data_2015_PM25 <- read.csv("data_2015_PM25.csv")



data_2015_PM25_esc_aguirre <- data_2015_PM25[data_2015_PM25$ESTACION..13 == "Escuelas Aguirre", ]

data_2015_PM25_esc_aguirre$D1KMAY_ID <- 702



data_2015_PM25_esc_aguirre$fecha <- ymd(data_2015_PM25_esc_aguirre$fecha)

data <- dff

data$fecha <- ymd(data$fecha)

data$D1KMAY_ID <- as.numeric(data$D1KMAY_ID)





data_2015_PM25_esc_aguirre <- data_2015_PM25_esc_aguirre[,-12]

data_2015_PM25_esc_aguirre$PM25 <- data_2015_PM25_esc_aguirre$values

data_2015_PM25_esc_aguirre$values <- as.numeric(as.character(data_2015_PM25_esc_aguirre$values))

data_2015_PM25_esc_aguirre <- data_2015_PM25_esc_aguirre[order(data_2015_PM25_esc_aguirre$fecha),]





data_2015_PM25_esc_aguirre <- sqldf('select a.fecha, avg(a.PM25) As PM25

                                    from data_2015_PM25_esc_aguirre As a

                                    group by a.fecha')



data <- data[data$D1KMAY_ID == 702,]









dataa <-             sqldf('select a.*, b.*

                    from data As a

                    left join data_2015_PM25_esc_aguirre As b

                    on  a.fecha = b.fecha')







data <- dff





data_2015_PM25_casa_campo <- data_2015_PM25[data_2015_PM25$ESTACION..13 == "Casa de Campo", ]

data_2015_PM25_casa_campo$D1KMAY_ID <- 697                                                         



data_2015_PM25_casa_campo$fecha <- ymd(data_2015_PM25_casa_campo$fecha)

data$fecha <- ymd(data$fecha)

data$D1KMAY_ID <- as.numeric(data$D1KMAY_ID)





data_2015_PM25_casa_campo <- data_2015_PM25_casa_campo[,-12]

data_2015_PM25_casa_campo$PM25 <- data_2015_PM25_casa_campo$values

data_2015_PM25_casa_campo$values <- as.numeric(as.character(data_2015_PM25_casa_campo$values))

data_2015_PM25_casa_campo <- data_2015_PM25_casa_campo[order(data_2015_PM25_casa_campo$fecha),]





data_2015_PM25_casa_campo <- sqldf('select a.fecha, avg(a.PM25) As PM25

                                    from data_2015_PM25_casa_campo As a

                                    group by a.fecha')



data <- data[data$D1KMAY_ID == 697,]









datab <-             sqldf('select a.*, b.*

                    from data As a

                    left join data_2015_PM25_casa_campo As b

                    on  a.fecha = b.fecha')





data <- dff



data_2015_PM25_caminos <- data_2015_PM25[data_2015_PM25$ESTACION..13 == "Cuatro Caminos", ]

data_2015_PM25_caminos$D1KMAY_ID <- 780



data_2015_PM25_caminos$fecha <- ymd(data_2015_PM25_caminos$fecha)

data$fecha <- ymd(data$fecha)

data$D1KMAY_ID <- as.numeric(data$D1KMAY_ID)





data_2015_PM25_caminos <- data_2015_PM25_caminos[,-12]

data_2015_PM25_caminos$PM25 <- data_2015_PM25_caminos$values

data_2015_PM25_caminos$values <- as.numeric(as.character(data_2015_PM25_caminos$values))

data_2015_PM25_caminos <- data_2015_PM25_caminos[order(data_2015_PM25_caminos$fecha),]





data_2015_PM25_caminos <- sqldf('select a.fecha, avg(a.PM25) As PM25

                                    from data_2015_PM25_caminos As a

                                    group by a.fecha')



data <- data[data$D1KMAY_ID == 780,]                                                         







datac <-             sqldf('select a.*, b.*

                    from data As a

                    left join data_2015_PM25_caminos As b

                    on  a.fecha = b.fecha')









data <- dff



data_2015_PM25_castilla <- data_2015_PM25[data_2015_PM25$ESTACION..13 == "Plaza Castilla", ]

data_2015_PM25_castilla$D1KMAY_ID <- 862



data_2015_PM25_castilla$fecha <- ymd(data_2015_PM25_castilla$fecha)

data$fecha <- ymd(data$fecha)

data$D1KMAY_ID <- as.numeric(data$D1KMAY_ID)





data_2015_PM25_castilla <- data_2015_PM25_castilla[,-12]

data_2015_PM25_castilla$PM25 <- data_2015_PM25_castilla$values

data_2015_PM25_castilla$values <- as.numeric(as.character(data_2015_PM25_castilla$values))

data_2015_PM25_castilla <- data_2015_PM25_castilla[order(data_2015_PM25_castilla$fecha),]





data_2015_PM25_castilla <- sqldf('select a.fecha, avg(a.PM25) As PM25

                                    from data_2015_PM25_castilla As a

                                    group by a.fecha')



data <- data[data$D1KMAY_ID == 862,]                                                         







datad <-             sqldf('select a.*, b.*

                    from data As a

                    left join data_2015_PM25_castilla As b

                    on  a.fecha = b.fecha')







data <- dff



data_2015_PM25_malvaro <- data_2015_PM25[data_2015_PM25$ESTACION..13 == "Mendez Alvaro", ]

data_2015_PM25_malvaro$D1KMAY_ID <- 580



data_2015_PM25_malvaro$fecha <- ymd(data_2015_PM25_malvaro$fecha)

data$fecha <- ymd(data$fecha)

data$D1KMAY_ID <- as.numeric(data$D1KMAY_ID)





data_2015_PM25_malvaro <- data_2015_PM25_malvaro[,-12]

data_2015_PM25_malvaro$PM25 <- data_2015_PM25_malvaro$values

data_2015_PM25_malvaro$values <- as.numeric(as.character(data_2015_PM25_malvaro$values))

data_2015_PM25_malvaro <- data_2015_PM25_malvaro[order(data_2015_PM25_malvaro$fecha),]





data_2015_PM25_malvaro <- sqldf('select a.fecha, avg(a.PM25) As PM25

                                    from data_2015_PM25_malvaro As a

                                    group by a.fecha')



data <- data[data$D1KMAY_ID == 580,]                                                         







datae <-             sqldf('select a.*, b.*

                    from data As a

                    left join data_2015_PM25_malvaro As b

                    on  a.fecha = b.fecha')







data <- dff



data_2015_PM25_castellana <- data_2015_PM25[data_2015_PM25$ESTACION..13 == "Castellana", ]

data_2015_PM25_castellana$D1KMAY_ID <- 700



data_2015_PM25_castellana$fecha <- ymd(data_2015_PM25_castellana$fecha)

data$fecha <- ymd(data$fecha)

data$D1KMAY_ID <- as.numeric(data$D1KMAY_ID)





data_2015_PM25_castellana <- data_2015_PM25_castellana[,-12]

data_2015_PM25_castellana$PM25 <- data_2015_PM25_castellana$values

data_2015_PM25_castellana$values <- as.numeric(as.character(data_2015_PM25_castellana$values))

data_2015_PM25_castellana <- data_2015_PM25_castellana[order(data_2015_PM25_castellana$fecha),]





data_2015_PM25_castellana <- sqldf('select a.fecha, avg(a.PM25) As PM25

                                    from data_2015_PM25_castellana As a

                                    group by a.fecha')



data <- data[data$D1KMAY_ID == 700,]                                                          







dataf <-             sqldf('select a.*, b.*

                    from data As a

                    left join data_2015_PM25_castellana As b

                    on  a.fecha = b.fecha')



#Add more Stations

#Cotos 28120001 No hay datos
# 
# data_2015_PM25_cm <- read.csv("data_PM2_5_cm.csv")
# 
# 
# data <- read.csv("C:/Users/jcord/Documents/AIRTEC/PM/data_model_2015_1_step_apli.csv")
# 
# 
# 
# data_2015_PM25_cotos <- data_2015_PM25_cm[data_2015_PM25_cm$Estacion == 28120001, ]
# 
# data_2015_PM25_cotos$D1KMAY_ID <- 700
# 
# 
# 
# data_2015_PM25_cotos$fecha <- ymd(paste0(data_2015_PM25_cotos$ANO,"/",
#                                             data_2015_PM25_cotos$MES,"/",
#                                             data_2015_PM25_cotos$DIA))
# 
# data_2015_PM25_cotos <- na.omit(data_2015_PM25_cotos)
# 
# data$fecha <- ymd(data$fecha)
# 
# data$D1KMAY_ID <- as.numeric(data$D1KMAY_ID)
# 
# 
# data_2015_PM25_cotos <- data_2015_PM25_cotos[order(data_2015_PM25_cotos$fecha),]
# 
# 
# data_2015_PM25_cotos$PM25 <- data_2015_PM25_cotos$values
# 
# data_2015_PM25_cotos$values <- as.numeric(as.character(data_2015_PM25_cotos$values))
# 
# data_2015_PM25_cotos <- data_2015_PM25_cotos[order(data_2015_PM25_cotos$fecha),]
# 
# 
# 
# 
# 
# data_2015_PM25_cotos <- sqldf('select a.fecha, avg(a.PM25) As PM25
# 
#                                     from data_2015_PM25_cotos As a
# 
#                                     group by a.fecha')
# 
# 
# data <- data[data$D1KMAY_ID == 700,]                                                          
# 
# 
# datag <-             sqldf('select a.*, b.*
# 
#                     from data As a
# 
#                     left join data_2015_PM25_cotos As b
# 
#                     on  a.fecha = b.fecha')



#El Atazar 28016001

lon_s <- -4.01425
lat_s <- 40.633089

data_2015_PM25_cm <- read.csv("data_PM2_5_cm.csv")


data <- dff



data_2015_PM25_atazar <- data_2015_PM25_cm[data_2015_PM25_cm$Estacion == 28016001, ]

data <- calculate_distance(data,lon_s,lat_s)

id <- data[which(data$d == min(data$d))[1], ]$D1KMAY_ID

data_2015_PM25_atazar$D1KMAY_ID <- id



data_2015_PM25_atazar$fecha <- ymd(paste0(data_2015_PM25_atazar$ANO,"/",
                                          data_2015_PM25_atazar$MES,"/",
                                          data_2015_PM25_atazar$DIA))

data_2015_PM25_atazar <- na.omit(data_2015_PM25_atazar)

data$fecha <- ymd(data$fecha)

data$D1KMAY_ID <- as.numeric(data$D1KMAY_ID)


data_2015_PM25_atazar <- data_2015_PM25_atazar[order(data_2015_PM25_atazar$fecha),]


data_2015_PM25_atazar$PM25 <- data_2015_PM25_atazar$values

data_2015_PM25_atazar$values <- as.numeric(as.character(data_2015_PM25_atazar$values))

data_2015_PM25_atazar <- data_2015_PM25_atazar[order(data_2015_PM25_atazar$fecha),]


data_2015_PM25_atazar <- sqldf('select a.fecha, avg(a.PM25) As PM25

                                    from data_2015_PM25_atazar As a

                                    group by a.fecha')



data <- data[data$D1KMAY_ID == id,]                                                          


datah <-             sqldf('select a.*, b.*

                    from data As a

                    left join data_2015_PM25_atazar As b

                    on  a.fecha = b.fecha')

datah <- datah[, !colnames(datah) %in% "d"]

#Collado Villalba 28047002

lon_s <- -4.01425
lat_s <- 40.633089

data_2015_PM25_cm <- read.csv("data_PM2_5_cm.csv")


data <- dff



data_2015_PM25_villalba <- data_2015_PM25_cm[data_2015_PM25_cm$Estacion == 28047002, ]

data <- calculate_distance(data,lon_s=lon_s,lat_s=lat_s)

id <- data[which(data$d == min(data$d))[1], ]$D1KMAY_ID

data_2015_PM25_villalba$D1KMAY_ID <- id



data_2015_PM25_villalba$fecha <- ymd(paste0(data_2015_PM25_villalba$ANO,"/",
                                            data_2015_PM25_villalba$MES,"/",
                                            data_2015_PM25_villalba$DIA))

data_2015_PM25_villalba <- na.omit(data_2015_PM25_villalba)

data$fecha <- ymd(data$fecha)

data$D1KMAY_ID <- as.numeric(data$D1KMAY_ID)


data_2015_PM25_villalba <- data_2015_PM25_villalba[order(data_2015_PM25_villalba$fecha),]


data_2015_PM25_villalba$PM25 <- data_2015_PM25_villalba$values

data_2015_PM25_villalba$values <- as.numeric(as.character(data_2015_PM25_villalba$values))

data_2015_PM25_villalba <- data_2015_PM25_villalba[order(data_2015_PM25_villalba$fecha),]





data_2015_PM25_villalba <- sqldf('select a.fecha, avg(a.PM25) As PM25

                                    from data_2015_PM25_villalba As a

                                    group by a.fecha')


data <- data[data$D1KMAY_ID == id,]                                                          


datai <-             sqldf('select a.*, b.*

                    from data As a

                    left join data_2015_PM25_villalba As b

                    on  a.fecha = b.fecha')

datai <- datai[, !colnames(datai) %in% "d"]


#Leganes 28074007

lon_s <- -3.754508
lat_s <- 40.339762 

data_2015_PM25_cm <- read.csv("data_PM2_5_cm.csv")


data <- dff



data_2015_PM25_leganes <- data_2015_PM25_cm[data_2015_PM25_cm$Estacion == 28074007, ]

data <- calculate_distance(data,lon_s=lon_s,lat_s=lat_s)

id <- data[which(data$d == min(data$d))[1], ]$D1KMAY_ID

data_2015_PM25_leganes$D1KMAY_ID <- id



data_2015_PM25_leganes$fecha <- ymd(paste0(data_2015_PM25_leganes$ANO,"/",
                                           data_2015_PM25_leganes$MES,"/",
                                           data_2015_PM25_leganes$DIA))

data_2015_PM25_leganes <- na.omit(data_2015_PM25_leganes)

data$fecha <- ymd(data$fecha)

data$D1KMAY_ID <- as.numeric(data$D1KMAY_ID)


data_2015_PM25_leganes <- data_2015_PM25_leganes[order(data_2015_PM25_leganes$fecha),]


data_2015_PM25_leganes$PM25 <- data_2015_PM25_leganes$values

data_2015_PM25_leganes$values <- as.numeric(as.character(data_2015_PM25_leganes$values))

data_2015_PM25_leganes <- data_2015_PM25_leganes[order(data_2015_PM25_leganes$fecha),]





data_2015_PM25_leganes <- sqldf('select a.fecha, avg(a.PM25) As PM25

                                    from data_2015_PM25_leganes As a

                                    group by a.fecha')


data <- data[data$D1KMAY_ID == id,]                                                          


dataj <-             sqldf('select a.*, b.*

                    from data As a

                    left join data_2015_PM25_leganes As b

                    on  a.fecha = b.fecha')

dataj <- dataj[, !colnames(dataj) %in% "d"]


#Getafe 28065014

lon_s <- -3.716879
lat_s <- 40.314577 

data_2015_PM25_cm <- read.csv("data_PM2_5_cm.csv")


data <- dff



data_2015_PM25_getafe <- data_2015_PM25_cm[data_2015_PM25_cm$Estacion == 28065014, ]

data <- calculate_distance(data,lon_s=lon_s,lat_s=lat_s)

id <- data[which(data$d == min(data$d))[1], ]$D1KMAY_ID

data_2015_PM25_getafe$D1KMAY_ID <- id



data_2015_PM25_getafe$fecha <- ymd(paste0(data_2015_PM25_getafe$ANO,"/",
                                          data_2015_PM25_getafe$MES,"/",
                                          data_2015_PM25_getafe$DIA))

data_2015_PM25_getafe <- na.omit(data_2015_PM25_getafe)

data$fecha <- ymd(data$fecha)

data$D1KMAY_ID <- as.numeric(data$D1KMAY_ID)


data_2015_PM25_getafe <- data_2015_PM25_getafe[order(data_2015_PM25_getafe$fecha),]


data_2015_PM25_getafe$PM25 <- data_2015_PM25_getafe$values

data_2015_PM25_getafe$values <- as.numeric(as.character(data_2015_PM25_getafe$values))

data_2015_PM25_getafe <- data_2015_PM25_getafe[order(data_2015_PM25_getafe$fecha),]





data_2015_PM25_getafe <- sqldf('select a.fecha, avg(a.PM25) As PM25

                                    from data_2015_PM25_getafe As a

                                    group by a.fecha')


data <- data[data$D1KMAY_ID == id,]                                                          


dataj <-             sqldf('select a.*, b.*

                    from data As a

                    left join data_2015_PM25_getafe As b

                    on  a.fecha = b.fecha')

dataj <- dataj[, !colnames(dataj) %in% "d"]



#Fuenlabrada 28148004

lon_s <- -3.803363
lat_s <- 40.283054 

data_2015_PM25_cm <- read.csv("data_PM2_5_cm.csv")


data <- dff



data_2015_PM25_fuenlabrada <- data_2015_PM25_cm[data_2015_PM25_cm$Estacion == 28148004, ]

data <- calculate_distance(data,lon_s=lon_s,lat_s=lat_s)

id <- data[which(data$d == min(data$d))[1], ]$D1KMAY_ID

data_2015_PM25_fuenlabrada$D1KMAY_ID <- id



data_2015_PM25_fuenlabrada$fecha <- ymd(paste0(data_2015_PM25_fuenlabrada$ANO,"/",
                                               data_2015_PM25_fuenlabrada$MES,"/",
                                               data_2015_PM25_fuenlabrada$DIA))

data_2015_PM25_fuenlabrada <- na.omit(data_2015_PM25_fuenlabrada)

data$fecha <- ymd(data$fecha)

data$D1KMAY_ID <- as.numeric(data$D1KMAY_ID)


data_2015_PM25_fuenlabrada <- data_2015_PM25_fuenlabrada[order(data_2015_PM25_fuenlabrada$fecha),]


data_2015_PM25_fuenlabrada$PM25 <- data_2015_PM25_fuenlabrada$values

data_2015_PM25_fuenlabrada$values <- as.numeric(as.character(data_2015_PM25_fuenlabrada$values))

data_2015_PM25_fuenlabrada <- data_2015_PM25_fuenlabrada[order(data_2015_PM25_fuenlabrada$fecha),]





data_2015_PM25_fuenlabrada <- sqldf('select a.fecha, avg(a.PM25) As PM25

                                    from data_2015_PM25_fuenlabrada As a

                                    group by a.fecha')


data <- data[data$D1KMAY_ID == id,]                                                          


datak <-             sqldf('select a.*, b.*

                    from data As a

                    left join data_2015_PM25_fuenlabrada As b

                    on  a.fecha = b.fecha')

datak <- datak[, !colnames(datak) %in% "d"]

#Algete 28009001

lon_s <- -3.503286111
lat_s <- 40.599819444

data_2015_PM25_cm <- read.csv("data_PM2_5_cm.csv")


data <- dff



data_2015_PM25_algete <- data_2015_PM25_cm[data_2015_PM25_cm$Estacion == 28009001, ]

data <- calculate_distance(data,lon_s=lon_s,lat_s=lat_s)

id <- data[which(data$d == min(data$d))[1], ]$D1KMAY_ID

data_2015_PM25_algete$D1KMAY_ID <- id



data_2015_PM25_algete$fecha <- ymd(paste0(data_2015_PM25_algete$ANO,"/",
                                          data_2015_PM25_algete$MES,"/",
                                          data_2015_PM25_algete$DIA))

data_2015_PM25_algete <- na.omit(data_2015_PM25_algete)

data$fecha <- ymd(data$fecha)

data$D1KMAY_ID <- as.numeric(data$D1KMAY_ID)


data_2015_PM25_algete <- data_2015_PM25_algete[order(data_2015_PM25_algete$fecha),]


data_2015_PM25_algete$PM25 <- data_2015_PM25_algete$values

data_2015_PM25_algete$values <- as.numeric(as.character(data_2015_PM25_algete$values))

data_2015_PM25_algete <- data_2015_PM25_algete[order(data_2015_PM25_algete$fecha),]





data_2015_PM25_algete <- sqldf('select a.fecha, avg(a.PM25) As PM25

                                    from data_2015_PM25_algete As a

                                    group by a.fecha')


data <- data[data$D1KMAY_ID == id,]                                                          


datal <-             sqldf('select a.*, b.*

                    from data As a

                    left join data_2015_PM25_algete As b

                    on  a.fecha = b.fecha')

datal <- datal[, !colnames(datal) %in% "d"]



#### END AOD PM 2.5 ####





#

#

# data <- data[data$Month == 1, ]

# data$day <- day(data$fecha)

# data <- data[data$day== 10, ]

#

# #Para medias mensuales:

# data <- sqldf('select a.lon,a.lat,avg(a.T) As T,avg(a.SLP) As SLP,

# avg(a.ele) As ele, avg(a.AOD) As AOD, avg(a.adjustAOD) As ad_AOD

#                     from data As a

#                     group by a.cord')

#

#

# write.csv(data, "data_20_08_2015.csv")



#Eq(2) first stage mixed model



#data_add <- read.csv("C:/GIS/datos_MAIAC_2015_con_traffic.csv")

#data_add <- data_add[data_add$fecha == "2015-01-01",]

dataa <- dataa[,-26]
datab <- datab[,-26]
datac <- datac[,-26]
datad <- datad[,-26]
datae <- datae[,-26]
dataf <- dataf[,-26]
datah <- datah[,-26]
datai <- datai[,-26]
dataj <- dataj[,-26]
datak <- datak[,-26]
datal <- datal[,-26]

#data_model <- rbind(dataa, datab, datac, datad, datae, dataf)#, dataj) #, datah, datai)

data_model <- rbind(dataa, datab, datac, datad, datae, dataf, datak, datal)

nas <- which(is.na(data_model$AOD))

data_model <- data_model[-nas, ]

data_model$yearday <- factor(yday(data_model$fecha))

data_model <- na.omit(data_model)


#Abr_03_2020: Se trata de recuperar lo que se tenía ahora que no se puede acceder al PC

data_add <- read.csv("C:/Users/jcord/Documents/AIRTEC/PM/NetCDF/data_2015_lmer_applied_with_traffic.csv")

data_add <- data_add[,-c(1:4)]

predicted_PM2.5 <- data_add$PM25
# 
# data_model <- data_model[,-26]


data_add <- data_add[,-29]
#Adding variables

# 
# 
# data_model <- sqldf('select a.*, b.LU_index, b.pop_index, b.NDVI, b.VH_KM
# 
#                         from data_model As a
# 
#                         left join data_add As b
# 
#                         on a.D1KMAY_ID = b.D1KMAY_ID')


data_model <- sqldf('select a.*, b.LU_index, b.pop_index, b.NDVI, b.VH_KM

                        from data_model As a

                        left join data_add As b

                        on a.D1KMAY_ID = b.D1KMAY_ID and a.yearday = b.yearday')

data_model <- data_model[, colnames(data_model) %in% c('adjustAOD','T','pop_index','ele','LU_index','VH_KM','u_wind','v_wind',
                                                       
                                                       'SLP','s_hum','NDVI','PBL','AOD','yearday','PM25')]

data_model <- data_model[, colnames(data_model) %in% c('adjustAOD','T','pop_index','ele','LU_index','VH_KM','u_wind','v_wind',
                                                       
                                                       'SLP','s_hum','NDVI','PBL','AOD','yearday','PM25')]

str(data_model)

yearday <- data_model$yearday

data_model <- data_model[,!colnames(data_model) %in% 'yearday']

data_model_scaled <- scale(data_model)

attributes(data_model_scaled)

att <- attributes(data_model_scaled)

#Save scaling attributes

fe_scales <- att$`scaled:scale`
fe_centers <- att$`scaled:center`

center <- 1.308293e+01

scale <- 7.269190e+00



data_model_scaled <- data.frame(data_model_scaled)



data_model_scaled$yearday <- data_model$yearday


# 
# set.seed(1)
# 
# 
# 
# index <- sample(1:length(data_model_scaled$AOD), round(0.75*length(data_model_scaled$AOD)))
# 
# train <- data_model_scaled[index,]
# 
# test <- data_model_scaled[-index,]
# 
# 



# library(lme4)

# Model1 = lmer(PM2.5 ~ adjustAOD +Temperature+

# populationdensity+elevation+trafficdensity+percentlanduse+emission+#X1

#                        windspeed+visibility+SLP+RH+#X2

#                        NDVI+PBL+#X3

#                        (1+AOT+Temperature|day),#random intercepts+random slopes for AOD and Tem

#                        data = train)#we calibrate PM2.5 for one city,so do not need the g(reg) and h(reg)

#

#

#















# Pruebas



# 
# 
# 
# 
# train <- na.omit(train)
# 
# Model1 = lmer(PM25 ~ adjustAOD +T+
#                 
#                 pop_index+ele+LU_index+VH_KM+#X1
#                 
#                 u_wind+v_wind+SLP+s_hum+#X2
#                 
#                 NDVI+PBL+#X3
#                 
#                 (1+AOD+T|yearday),#random intercepts+random slopes for AOD and Tem
#               
#               data = train)#we calibrate PM2.5 for one city,so do not need the g(reg) and h(reg)

Model1 = lmer(PM25 ~ adjustAOD +T+
                
                pop_index+ele+LU_index+VH_KM+#X1
                
                u_wind+v_wind+SLP+s_hum+#X2
                
                NDVI+PBL+#X3
                
                (1+AOD+T|yearday),#random intercepts+random slopes for AOD and Tem
              
              data = data_model_scaled)#we calibrate PM2.5 for one city,so do not need the g(reg) and h(reg)

preds <- predict(Model1, newdata = data_model_scaled)

preds <-  preds*scale + center

hist(preds)

preds_test <- predict(Model1, newdata = test)

preds_test <-  preds_test*scale + center



cor(data_model$PM25,preds)^2



cor(test$PM25,preds_test)^2

RMSE(test$PM25,preds_test)

MAE(test$PM25,preds_test)

MBE(test$PM25,preds_test)

#Vamos a probar ensemblando una ANN, saltar si no sale bien
# 
# data_model_scaled$preds_lmer <- preds
# 
# data_model_scaled$yearday <- as.numeric(data_model_scaled$yearday)
# 
# 
# train <- data_model_scaled[index,]
# 
# test <- data_model_scaled[-index,]
# 
# nnet_model <- neuralnet(PM25~., hidden = c(20,10), data = train)
# 
# colnames(train)
# 
# Y_train <- train$PM25
# 
# xg_model <- xgboost(data = as.matrix(train[,!colnames(train) %in% "PM25"]), label = Y_train, max.depth = 5,
#                     eta = 0.3, nthread = 10, nround = 10, n_estimators = 1000,
#                     min_child_weight=300,colsample_bytree=0.8,subsample=0.8)
# 
# 
# 
# 
# 
# preds <- predict(xg_model, newdata = as.matrix(data_model_scaled[,!colnames(data_model_scaled) %in% "PM25"]))
# 
# preds <-  preds*scale + center
# 
# 
# 
# preds_test <- predict(xg_model, newdata = as.matrix(test[,!colnames(test) %in% "PM25"]))
# 
# preds_test <-  preds_test*scale + center
# 
# 
# 
# cor(data_model$PM25,preds)^2
# 
# 
# 
# cor(test$PM25,preds_test)^2
# 
# RMSE(test$PM25,preds_test)
# 
# MAE(test$PM25,preds_test)
# 
# MBE(test$PM25,preds_test)
# 


#END Ensembling




#Predecimos PM2.5 en las celdas sin AQS

data_add$yearday <- factor(yday(data_add$fecha))

lon_WRF <- data_add$lon_WRF
lat_WRF <- data_add$lat_WRF


data_add <- data_add[, colnames(data_add) %in% c('adjustAOD','T','pop_index','ele','LU_index','VH_KM','u_wind','v_wind',
                                                 
                                                 'SLP','s_hum','NDVI','PBL','AOD','yearday','D1KMAY_ID')]

data_add_scaled <- scale(data_add[,!colnames(data_add) %in% c('D1KMAY_ID','yearday')],
                         scale = fe_scales[-11], center = fe_centers[-11])

attributes(data_add_scaled)



#Save scaling attributes
# 
# 
# 
# center <- 1.125627e+01
# 
# scale <- 7.060422e+00



data_add_scaled <- data.frame(data_add_scaled)



data_add_scaled$yearday <- data_add$yearday



data_add$PM25 <- predict(Model1, newdata = data_add_scaled, allow.new.levels = TRUE)*scale + center

hist(na.omit(data_add$PM25))

write.csv(data_add, "lmer_08_09_2020_ipw_loop.csv")


#Select one day to represent in GIS

data_add$lon_WRF <- lon_WRF
data_add$lat_WRF <- lat_WRF

data_add2 <- data_add[data_add$yearday == 138,] %>% na.omit() #Valen 135, 232



write.csv(data_add2,"C:/Users/jcord/Documents/AIRTEC/PM/NetCDF/data_2015_lmer_applied_with_traffic_scaled_138.csv")

#Select a day with high Concentration

max(na.omit(dataa$PM25))
which(na.omit(dataa$PM25) == 39)

data_add2 <- data_add[data_add$yearday == 349,] %>% na.omit() 

dataa$yearday <- yday(dataa$fecha)

write.csv(data_add2,"C:/Users/jcord/Documents/AIRTEC/PM/NetCDF/data_2015_lmer_applied_with_traffic_scaled_355.csv")

max(na.omit(datac$PM25))
which(na.omit(datac$PM25) == 37)


#Represent predicted vs observed for some Stations

#

id <- dataa$D1KMAY_ID[1]

data_plot <- data.frame("observed" = dataa$PM25,
                        "predicted" = data_add[data_add$D1KMAY_ID == id, ]$PM25,
                        "d" = densCols(data_add[data_add$D1KMAY_ID == id, ]$PM25, dataa$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot) +
  geom_point(aes(x = observed, y = predicted, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = predicted), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Escuelas Aguirre") +
  theme_bw()

cor(data_plot$observed, data_plot$predicted, use = "pairwise.complete.obs")^2

esc_aguirre_CMAQ <- read_table2("NetCDF/CMAQ/Datos_Horarios_PM10_PM25/CELDA_C22_F18.txt")

esc_aguirre_CMAQ <- esc_aguirre_CMAQ[-1,]

esc_aguirre_CMAQ <- sqldf('select a.FECHA, avg(a.CAPAS) As PM25
                          from esc_aguirre_CMAQ As a
                          group by a.FECHA')

esc_aguirre_CMAQ$yearday <- seq(from = 1, to = nrow(esc_aguirre_CMAQ), by = 1)

dataa$yday <- yday(dataa$fecha)

data_plot <- sqldf('select a.PM25, b.PM25 As PM25_CMAQ
                   from dataa As a
                   left join esc_aguirre_CMAQ As b
                   on a.yday = b.yearday')

data_plot2 <- data.frame("observed" = data_plot$PM25,
                         "CMAQ" = data_plot$PM25_CMAQ,
                         "d" = densCols(data_plot$PM25_CMAQ, data_plot$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot2) +
  geom_point(aes(x = observed, y = CMAQ, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = CMAQ), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Escuelas Aguirre") +
  theme_bw()

cor(data_plot2$observed, data_plot2$CMAQ, use = "pairwise.complete.obs")^2



id <- datab$D1KMAY_ID[1]

data_plot <- data.frame("observed" = datab$PM25,
                        "predicted" = data_add[data_add$D1KMAY_ID == id, ]$PM25,
                        "d" = densCols(data_add[data_add$D1KMAY_ID == id, ]$PM25, dataa$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot) +
  geom_point(aes(x = observed, y = predicted, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = predicted), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Casa de Campo") +
  theme_bw()

cor(data_plot$observed, data_plot$predicted, use = "pairwise.complete.obs")^2


casa_campo_CMAQ <- read_table2("NetCDF/CMAQ/Datos_Horarios_PM10_PM25/CELDA_C17_F18.txt")

casa_campo_CMAQ <- casa_campo_CMAQ[-1,]

casa_campo_CMAQ <- sqldf('select a.FECHA, avg(a.CAPAS) As PM25
                          from casa_campo_CMAQ As a
                          group by a.FECHA')

casa_campo_CMAQ$yearday <- seq(from = 1, to = nrow(casa_campo_CMAQ), by = 1)

datab$yday <- yday(datab$fecha)

data_plot <- sqldf('select a.PM25, b.PM25 As PM25_CMAQ
                   from datab As a
                   left join casa_campo_CMAQ As b
                   on a.yday = b.yearday')

data_plot2 <- data.frame("observed" = data_plot$PM25,
                         "CMAQ" = data_plot$PM25_CMAQ,
                         "d" = densCols(data_plot$PM25_CMAQ, data_plot$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot2) +
  geom_point(aes(x = observed, y = CMAQ, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = CMAQ), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Casa de Campo") +
  theme_bw()

cor(data_plot2$observed, data_plot2$CMAQ, use = "pairwise.complete.obs")^2









id <- datac$D1KMAY_ID[1]

data_plot <- data.frame("observed" = datac$PM25,
                        "predicted" = data_add[data_add$D1KMAY_ID == id, ]$PM25,
                        "d" = densCols(data_add[data_add$D1KMAY_ID == id, ]$PM25, dataa$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot) +
  geom_point(aes(x = observed, y = predicted, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = predicted), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Cuatro Caminos") +
  theme_bw()


cor(data_plot$observed, data_plot$predicted, use = "pairwise.complete.obs")^2


caminos_CMAQ <- read_table2("NetCDF/CMAQ/Datos_Horarios_PM10_PM25/CELDA_C20_F20.txt")

caminos_CMAQ <- caminos_CMAQ[-1,]

caminos_CMAQ <- sqldf('select a.FECHA, avg(a.CAPAS) As PM25
                          from caminos_CMAQ As a
                          group by a.FECHA')

caminos_CMAQ$yearday <- seq(from = 1, to = nrow(caminos_CMAQ), by = 1)

datac$yday <- yday(datac$fecha)

data_plot <- sqldf('select a.PM25, b.PM25 As PM25_CMAQ
                   from datac As a
                   left join caminos_CMAQ As b
                   on a.yday = b.yearday')

data_plot2 <- data.frame("observed" = data_plot$PM25,
                         "CMAQ" = data_plot$PM25_CMAQ,
                         "d" = densCols(data_plot$PM25_CMAQ, data_plot$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot2) +
  geom_point(aes(x = observed, y = CMAQ, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = CMAQ), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Cuatro Caminos") +
  theme_bw()

cor(data_plot2$observed, data_plot2$CMAQ, use = "pairwise.complete.obs")^2












id <- datad$D1KMAY_ID[1]

data_plot <- data.frame("observed" = datac$PM25,
                        "predicted" = data_add[data_add$D1KMAY_ID == id, ]$PM25,
                        "d" = densCols(data_add[data_add$D1KMAY_ID == id, ]$PM25, dataa$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot) +
  geom_point(aes(x = observed, y = predicted, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = predicted), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Pza. Castilla") +
  theme_bw()


cor(data_plot$observed, data_plot$predicted, use = "pairwise.complete.obs")^2

pcastilla_CMAQ <- read_table2("NetCDF/CMAQ/Datos_Horarios_PM10_PM25/CELDA_C22_F22.txt")

pcastilla_CMAQ <- pcastilla_CMAQ[-1,]

pcastilla_CMAQ <- sqldf('select a.FECHA, avg(a.CAPAS) As PM25
                          from pcastilla_CMAQ As a
                          group by a.FECHA')

pcastilla_CMAQ$yearday <- seq(from = 1, to = nrow(pcastilla_CMAQ), by = 1)

datad$yday <- yday(datad$fecha)

data_plot <- sqldf('select a.PM25, b.PM25 As PM25_CMAQ
                   from datad As a
                   left join pcastilla_CMAQ As b
                   on a.yday = b.yearday')

data_plot2 <- data.frame("observed" = data_plot$PM25,
                         "CMAQ" = data_plot$PM25_CMAQ,
                         "d" = densCols(data_plot$PM25_CMAQ, data_plot$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot2) +
  geom_point(aes(x = observed, y = CMAQ, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = CMAQ), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Pza. Castilla") +
  theme_bw()

cor(data_plot2$observed, data_plot2$CMAQ, use = "pairwise.complete.obs")^2













id <- datae$D1KMAY_ID[1]

data_plot <- data.frame("observed" = datac$PM25,
                        "predicted" = data_add[data_add$D1KMAY_ID == id, ]$PM25,
                        "d" = densCols(data_add[data_add$D1KMAY_ID == id, ]$PM25, dataa$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot) +
  geom_point(aes(x = observed, y = predicted, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = predicted), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Mendez Alvaro") +
  theme_bw()


cor(data_plot$observed, data_plot$predicted, use = "pairwise.complete.obs")^2


malvaro_CMAQ <- read_table2("NetCDF/CMAQ/Datos_Horarios_PM10_PM25/CELDA_C22_F15.txt")

malvaro_CMAQ <- malvaro_CMAQ[-1,]

malvaro_CMAQ <- sqldf('select a.FECHA, avg(a.CAPAS) As PM25
                          from malvaro_CMAQ As a
                          group by a.FECHA')

malvaro_CMAQ$yearday <- seq(from = 1, to = nrow(malvaro_CMAQ), by = 1)

datae$yday <- yday(datae$fecha)

data_plot <- sqldf('select a.PM25, b.PM25 As PM25_CMAQ
                   from datae As a
                   left join malvaro_CMAQ As b
                   on a.yday = b.yearday')

data_plot2 <- data.frame("observed" = data_plot$PM25,
                         "CMAQ" = data_plot$PM25_CMAQ,
                         "d" = densCols(data_plot$PM25_CMAQ, data_plot$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot2) +
  geom_point(aes(x = observed, y = CMAQ, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = CMAQ), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Mendez Alvaro") +
  theme_bw()

cor(data_plot2$observed, data_plot2$CMAQ, use = "pairwise.complete.obs")^2








id <- dataf$D1KMAY_ID[1]

data_plot <- data.frame("observed" = datac$PM25,
                        "predicted" = data_add[data_add$D1KMAY_ID == id, ]$PM25,
                        "d" = densCols(data_add[data_add$D1KMAY_ID == id, ]$PM25, dataa$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot) +
  geom_point(aes(x = observed, y = predicted, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = predicted), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Castellana") +
  theme_bw()


cor(data_plot$observed, data_plot$predicted, use = "pairwise.complete.obs")^2


castellana_CMAQ <- read_table2("NetCDF/CMAQ/Datos_Horarios_PM10_PM25/CELDA_C22_F20.txt")

castellana_CMAQ <- castellana_CMAQ[-1,]

castellana_CMAQ <- sqldf('select a.FECHA, avg(a.CAPAS) As PM25
                          from castellana_CMAQ As a
                          group by a.FECHA')

castellana_CMAQ$yearday <- seq(from = 1, to = nrow(castellana_CMAQ), by = 1)

dataf$yday <- yday(dataf$fecha)

data_plot <- sqldf('select a.PM25, b.PM25 As PM25_CMAQ
                   from dataf As a
                   left join castellana_CMAQ As b
                   on a.yday = b.yearday')

data_plot2 <- data.frame("observed" = data_plot$PM25,
                         "CMAQ" = data_plot$PM25_CMAQ,
                         "d" = densCols(data_plot$PM25_CMAQ, data_plot$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot2) +
  geom_point(aes(x = observed, y = CMAQ, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = CMAQ), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Castellana") +
  theme_bw()

cor(data_plot2$observed, data_plot2$CMAQ, use = "pairwise.complete.obs")^2










id <- datak$D1KMAY_ID[1]

data_plot <- data.frame("observed" = datac$PM25,
                        "predicted" = data_add[data_add$D1KMAY_ID == id, ]$PM25,
                        "d" = densCols(data_add[data_add$D1KMAY_ID == id, ]$PM25, dataa$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot) +
  geom_point(aes(x = observed, y = predicted, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = predicted), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Fuenlabrada") +
  theme_bw()


cor(data_plot$observed, data_plot$predicted, use = "pairwise.complete.obs")^2


fuen_CMAQ <- read_table2("NetCDF/CMAQ/Datos_Horarios_PM10_PM25/CELDA_C93_F80_Fuenlabrada.txt")

fuen_CMAQ <- fuen_CMAQ[-1,]

fuen_CMAQ <- sqldf('select a.FECHA, avg(a.CAPAS) As PM25
                          from fuen_CMAQ As a
                          group by a.FECHA')

fuen_CMAQ$yearday <- seq(from = 1, to = nrow(fuen_CMAQ), by = 1)

datak$yday <- yday(datak$fecha)

data_plot <- sqldf('select a.PM25, b.PM25 As PM25_CMAQ
                   from datak As a
                   left join fuen_CMAQ As b
                   on a.yday = b.yearday')

data_plot2 <- data.frame("observed" = data_plot$PM25,
                         "CMAQ" = data_plot$PM25_CMAQ,
                         "d" = densCols(data_plot$PM25_CMAQ, data_plot$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot2) +
  geom_point(aes(x = observed, y = CMAQ, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = CMAQ), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Fuenlabrada") +
  theme_bw()

cor(data_plot2$observed, data_plot2$CMAQ, use = "pairwise.complete.obs")^2







id <- datal$D1KMAY_ID[1]

data_plot <- data.frame("observed" = datac$PM25,
                        "predicted" = data_add[data_add$D1KMAY_ID == id, ]$PM25,
                        "d" = densCols(data_add[data_add$D1KMAY_ID == id, ]$PM25, dataa$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot) +
  geom_point(aes(x = observed, y = predicted, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = predicted), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Algete") +
  theme_bw()


cor(data_plot$observed, data_plot$predicted, use = "pairwise.complete.obs")^2


alge_CMAQ <- read_table2("NetCDF/CMAQ/Datos_Horarios_PM10_PM25/CELDA_C69_F47_Algete.txt")

alge_CMAQ <- alge_CMAQ[-1,]

alge_CMAQ <- sqldf('select a.FECHA, avg(a.CAPAS) As PM25
                          from alge_CMAQ As a
                          group by a.FECHA')

alge_CMAQ$yearday <- seq(from = 1, to = nrow(alge_CMAQ), by = 1)

datal$yday <- yday(datal$fecha)

data_plot <- sqldf('select a.PM25, b.PM25 As PM25_CMAQ
                   from datal As a
                   left join alge_CMAQ As b
                   on a.yday = b.yearday')

data_plot2 <- data.frame("observed" = data_plot$PM25,
                         "CMAQ" = data_plot$PM25_CMAQ,
                         "d" = densCols(data_plot$PM25_CMAQ, data_plot$PM25, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))))

ggplot(data = data_plot2) +
  geom_point(aes(x = observed, y = CMAQ, col = d), size = 1) +
  scale_color_identity() +
  geom_abline() +
  geom_smooth(aes(x = observed, y = CMAQ), method = "lm", se = TRUE) +
  xlim(0,40) +
  ylim(0,40) +
  ggtitle("Algete") +
  theme_bw()

cor(data_plot2$observed, data_plot2$CMAQ, use = "pairwise.complete.obs")^2












#Obtain yearly mean by cell


data_add$lon_WRF <- lon_WRF
data_add$lat_WRF <- lat_WRF

data_add <- data_add[data_add$PM25 < 100, ]
data_add <- data_add[data_add$PM25 > 0, ]

data_add <- na.omit(data_add)

data_year_2015_average <- sqldf('select a.lon_WRF, a.lat_WRF, a.D1KMAY_ID, avg(a.PM25) As PM25
                                from data_add As a
                                group by a.D1KMAY_ID')


data_year_2015_average$PM25 <- as.double(data_year_2015_average$PM25)
str(data_year_2015_average)

write.csv(data_year_2015_average, "C:/Users/jcord/Documents/AIRTEC/PM/NetCDF/data_year_2015_average_ipw_loop.csv")


#Obtain map of missing values percetanges

data_add <- data_add[data_add$PM25 < 100, ]

data_add <- na.omit(data_add)

data_year_2015_cnt <- sqldf('select a.lon_WRF, a.lat_WRF, a.D1KMAY_ID, count(a.PM25) As has_data
                                from data_add As a
                                group by a.D1KMAY_ID')

data_year_2015_cnt$NA_prg <- data_year_2015_cnt$has_data/365*100
str(data_year_2015_cnt)
write.csv2(data_year_2015_cnt, "C:/Users/jcord/Documents/AIRTEC/PM/NetCDF/data_year_2015_cnt.csv")


#Compare with CMAQ values

data_MAIAC <- read.csv("C:/Users/jcord/Documents/AIRTEC/PM/NetCDF/Libro1.csv", header = TRUE,
                       sep = ";",
                       stringsAsFactors = FALSE)


data_CMAQ <- read_excel("C:/Users/jcord/Documents/AIRTEC/PM/NetCDF/CMAQ/Media_anual_PM25_CMAQ.xls")

data_MAIAC$cord <- paste0(round(data_MAIAC$lon_WRF,2),"_",round(data_MAIAC$lat_WRF,2))
data_CMAQ$cord <- paste0(round(data_CMAQ$lon,2),"_",round(data_CMAQ$lat,2))


data_join <- sqldf('select a.*, b.PM25 As PM25_CMAQ
                    from data_MAIAC As a
                    left join data_CMAQ As b
                    on a.cord = b.cord
                   ')

data_join <- na.omit(data_join)
cor(data_join$PM25, data_join$PM25_CMAQ)^2

ggplot(data = data_join) +
  geom_point(aes(x = PM25_CMAQ, y = PM25), col = 'red') +
  geom_abline() +
  stat_smooth(aes(x = PM25_CMAQ, y = PM25), method = "lm", se = TRUE) +
  xlim(0,20) +
  ylim(0,20) +
  theme_bw() 

######################END##############################################################

data <- read.csv("C:/GIS/datos_MAIAC_2015_con_traffic.csv")



data_add$lon <- data$lon_WRF

data_add$lat <- data$lat_WRF



data_add2 <- data_add[data_add$yearday == 233,] %>% na.omit()



# write.csv(data_add2,"C:/Users/jcord/Documents/AIRTEC/PM/NetCDF/data_2015_lmer_applied_with_traffic_scaled_20_08_2015.csv")



#10-fold cross validation









folds <- createFolds(train$PM25, k = 10)



init <- Sys.time()

r2_f <- vector()

CV <- lapply(folds, function(x) {
  
  training_fold = train[-x, ]
  
  test_fold = train[x, ]
  
  Model = lmer(PM25 ~ adjustAOD +T+
                 
                 pop_index+ele+LU_index+VH_KM+#X1
                 
                 u_wind+v_wind+SLP+s_hum+#X2
                 
                 NDVI+PBL+#X3
                 
                 (1+AOD+T|yearday),#random intercepts+random slopes for AOD and Tem
               
               data = training_fold)#we calibrate PM2.5 for one city,so do not need the g(reg) and h(reg)
  
  y_preds = predict(Model, newdata = test_fold)
  
  r2 = cor(test_fold$PM25, y_preds)^2
  
  r2_f <- c(r2_f,r2)
  
  r2_f
  
})

end <- Sys.time()



# r2_f <- vector()







mean(as.numeric(as.character(CV)))

sd(as.numeric(CV))







init <- Sys.time()

rmse_f <- vector()

CV <- lapply(folds, function(x) {
  
  training_fold = train[-x, ]
  
  test_fold = train[x, ]
  
  Model = lmer(PM25 ~ adjustAOD +T+
                 
                 pop_index+ele+LU_index+VH_KM+#X1
                 
                 u_wind+v_wind+SLP+s_hum+#X2
                 
                 NDVI+PBL+#X3
                 
                 (1+AOD+T|yearday),#random intercepts+random slopes for AOD and Tem
               
               data = training_fold)#we calibrate PM2.5 for one city,so do not need the g(reg) and h(reg)
  
  y_preds = predict(Model, newdata = test_fold)
  
  r2 = cor(test_fold$PM25, y_preds)^2
  
  rmse_f <- RMSE(test_fold$PM25, y_preds)
  
  rmse_f
  
})

end <- Sys.time()



# r2_f <- vector()







mean(as.numeric(as.character(CV)))

sd(as.numeric(CV))



init <- Sys.time()

mbe_f <- vector()

CV <- lapply(folds, function(x) {
  
  training_fold = train[-x, ]
  
  test_fold = train[x, ]
  
  Model = lmer(PM25 ~ adjustAOD +T+
                 
                 pop_index+ele+LU_index+VH_KM+#X1
                 
                 u_wind+v_wind+SLP+s_hum+#X2
                 
                 NDVI+PBL+#X3
                 
                 (1+AOD+T|yearday),#random intercepts+random slopes for AOD and Tem
               
               data = training_fold)#we calibrate PM2.5 for one city,so do not need the g(reg) and h(reg)
  
  y_preds = predict(Model, newdata = test_fold)
  
  r2 = cor(test_fold$PM25, y_preds)^2
  
  mbe_f <- MBE(test_fold$PM25, y_preds)
  
  mbe_f
  
})

end <- Sys.time()


mean(as.numeric(as.character(CV)))

sd(as.numeric(CV))













































data(algae)

normData <- scale(algae[,4:12])

t <- rpartXse(a1 ~ .,as.data.frame(normData[1:100,]))

normPs <- predict(t,as.data.frame(normData[101:nrow(normData),]))

ps <- unscale(normPs,normData)