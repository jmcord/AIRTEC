rm(list=ls())

library(AeRobiology)
library(xlsx)
library(openxlsx)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(sqldf)
library(readr)
library(stringr)
library(rjson)
library(neuralnet)
library(randomForest)
library(e1071)
library(xgboost)


PATH_METEO <- "C:/Users/Jose María/Documents/AIRTEC/Meteorología/"
setwd(PATH_METEO)

#Define the functions

haversine <- function(lat1, lat2, lon1, lon2) {
  
  phi_1 = lat1*pi/180
  phi_2 = lat2*pi/180
  R = 6371000  # radius of Earth in meters
  delta_phi = (lat2 - lat1)*pi/180
  delta_lambda = (lon2 - lon1)*pi/180
  
  a = sin(delta_phi / 2.0) ** 2 + cos(phi_1) * cos(phi_2) * sin(delta_lambda / 2.0) ** 2
  
  c = 2 * atan2(sqrt(a), sqrt(1 - a))
  
  meters = R * c  # output distance in meters
  meters
}


station_code <- data.frame(read_table2("~/AIRTEC/Meteorología/station_code.txt"))
meteo_data_noaa <- data.frame(read_csv("~/AIRTEC/Meteorología/2010_2019dat.csv", skip = 1))
head(meteo_data_noaa)

#Delete unuseful variables

variables_delete <- str_detect(colnames(meteo_data_noaa), "Q_")
variables_delete <- colnames(meteo_data_noaa)[variables_delete]

meteo_data_noaa_1f <- meteo_data_noaa[, !colnames(meteo_data_noaa) %in% variables_delete]


#Join station code

meteo_data_noaa_1f_sq <- sqldf("select a.*, b.Station
from meteo_data_noaa_1f As a
left join station_code As b
where a.USAF = b.Station_ID
                         ") 


#Refinar









# 1 line names

stations_first <- c("colmenar", "universitaria", "retiro", "getafe")

meteo_F_first <- meteo_data_noaa_1f_sq[meteo_data_noaa_1f_sq$Station %in% stations_first, ]
meteo_F_first$date <- ymd(meteo_F_first$Date)
meteo_F_first$year <- factor(year(meteo_F_first$date))
meteo_F_first$month <- factor(month(meteo_F_first$date))

meteo_F_first <- meteo_F_first[, !colnames(meteo_F_first) %in% "Date"]
#create a list of df for each of the four locations

meteo_first_list <- lapply(stations_first, function(x) {
  df = meteo_F_first[meteo_F_first$Station == x,]
})
names(meteo_first_list) <- stations_first

aggr_Tmean <- function(df) {
  df_agg = sqldf("select *, avg(df.Temp) As T_mounthly
                 from df 
                 group by df.month, df.year
                order by df.month desc, df.year
                 ")
}

# p <- meteo_first_list[[1]]
# p <- aggr_Tmean(p)
# 
# 
# str(p)

meteo_F_aggr_list <- lapply(meteo_first_list, aggr_Tmean)


#Try 2018 comparison of Tmean along year

meteo_2018_first_list <- lapply(meteo_F_aggr_list, function(x) {
  x <- x[x$year == 2018,]
})

island_2018_first <- bind_rows(meteo_2018_first_list)

ggplot() +
  geom_line(aes(x = month, y = T_mounthly, group = Station, colour = Station), data =island_2018_first)



#from URL-AEMET download retiro data
download.file("https://opendata.aemet.es/opendata/sh/e1d38785", destfile = "retiro_1990_1994.json")


download.file("https://opendata.aemet.es/opendata/sh/82094459", destfile = "retiro_1995_1999.json")


download.file("https://opendata.aemet.es/opendata/sh/8b9af965", destfile = "retiro_2000_2003.json")


download.file("https://opendata.aemet.es/opendata/sh/0343cfd6", destfile = "retiro_2004.json")


download.file("https://opendata.aemet.es/opendata/sh/ac860e6c", destfile = "retiro_2005_2009.json")


download.file("https://opendata.aemet.es/opendata/sh/b4ee4667", destfile = "retiro_2010_2014.json")


download.file("https://opendata.aemet.es/opendata/sh/1462be4c", destfile = "retiro_2015_2018.json")


#from URL-AEMET download colmenar data

download.file("https://opendata.aemet.es/opendata/sh/9bde3b0f", destfile = "colmenar_1990_1994.json")


download.file("https://opendata.aemet.es/opendata/sh/bb2d7f51", destfile = "colmenar_1995_1999.json")


download.file("https://opendata.aemet.es/opendata/sh/9b7cab34", destfile = "colmenar_2000_2003.json")


download.file("https://opendata.aemet.es/opendata/sh/c44d2e49", destfile = "colmenar_2004.json")


download.file("https://opendata.aemet.es/opendata/sh/798fd52a", destfile = "colmenar_2005_2009.json")


download.file("https://opendata.aemet.es/opendata/sh/b6e6268b", destfile = "colmenar_2010_2014.json")


download.file("https://opendata.aemet.es/opendata/sh/e1e2082c", destfile = "colmenar_2015_2018.json")


#from URL-AEMET download universitaria data

#download.file("https://opendata.aemet.es/opendata/sh/9bde3b0f", destfile = "universitaria_1990_1994.json")


download.file("https://opendata.aemet.es/opendata/sh/5e7ee638", destfile = "universitaria_1995_1999.json")


download.file("https://opendata.aemet.es/opendata/sh/f805a8ae", destfile = "universitaria_2000_2003.json")


download.file("https://opendata.aemet.es/opendata/sh/0142c21d", destfile = "universitaria_2004.json")


download.file("https://opendata.aemet.es/opendata/sh/1bd9f720", destfile = "universitaria_2005_2009.json")


download.file("https://opendata.aemet.es/opendata/sh/f9f7d7df", destfile = "universitaria_2010_2014.json")


download.file("https://opendata.aemet.es/opendata/sh/22ef9da0", destfile = "universitaria_2015_2018.json")


#from URL-AEMET download getafe data

download.file("https://opendata.aemet.es/opendata/sh/24b02e8c", destfile = "getafe_1990_1994.json")


download.file("https://opendata.aemet.es/opendata/sh/05ba7ca7", destfile = "getafe_1995_1999.json")


download.file("https://opendata.aemet.es/opendata/sh/af26bc4a", destfile = "getafe_2000_2003.json")


download.file("https://opendata.aemet.es/opendata/sh/100df56c", destfile = "getafe_2004.json")


download.file("https://opendata.aemet.es/opendata/sh/b27b3d5c", destfile = "getafe_2005_2009.json")


download.file("https://opendata.aemet.es/opendata/sh/8ec725c0", destfile = "getafe_2010_2014.json")


download.file("https://opendata.aemet.es/opendata/sh/828223d6", destfile = "getafe_2015_2018.json")


files_json <- list.files(pattern = "json")

files_F <- lapply(files_json, function(x) {fromJSON(file = x)})

files_F <- lapply(files_F, bind_rows)

meteo_F <- bind_rows(files_F)

str(meteo_F)

#Set adequate

meteo_F$date <- ymd(meteo_F$fecha)
meteo_F$altitud <- str_replace(meteo_F$altitud, ",", ".") %>%  
  as.numeric()
meteo_F$tmed <- str_replace(meteo_F$tmed, ",", ".") %>%  
  as.numeric()
meteo_F$prec <- str_replace(meteo_F$prec, ",", ".") %>%
  as.numeric()
meteo_F$tmin <- str_replace(meteo_F$tmin, ",", ".") %>% 
  as.numeric()
meteo_F$tmax <- str_replace(meteo_F$tmax, ",", ".") %>%
  as.numeric()
meteo_F$dir <- str_replace(meteo_F$dir, ",", ".") %>%
  as.numeric()
meteo_F$velmedia <- str_replace(meteo_F$velmedia, ",", ".") %>%
  as.numeric()
meteo_F$racha <- str_replace(meteo_F$racha, ",", ".") %>%
  as.numeric()
meteo_F$sol <- str_replace(meteo_F$sol, ",", ".") %>%
  as.numeric()
meteo_F$presMax <- str_replace(meteo_F$presMax, ",", ".") %>%
  as.numeric()
meteo_F$presMin <- str_replace(meteo_F$presMin, ",", ".") %>%
  as.numeric()
meteo_F$nombre <- factor(meteo_F$nombre)

#Extract time units

meteo_F$year <- year(meteo_F$date) %>%
  factor()
meteo_F$month <- month(meteo_F$date) %>%
  factor()
meteo_F$day <- day(meteo_F$date)
meteo_F$hour <- hour(meteo_F$date)

str(meteo_F)
#Drop unused columns

meteo_F <- meteo_F[, !colnames(meteo_F) %in% c("estado","descripcion")]  
  

#Aggregate T by mounth in a first Tmounthly to gain insight in the heat island



meteo_F_mounthly <- sqldf("select *, avg(meteo_F.tmed) As T_med, avg(meteo_F.tmax) As T_max, avg(meteo_F.tmin) As T_min
                 from meteo_F
                 group by meteo_F.nombre, meteo_F.year, meteo_F.month
                 order by meteo_F.month desc, meteo_F.year
                 ")


meteo_F_mounthly$nombre <- factor(meteo_F_mounthly$nombre)
str(meteo_F_mounthly)
#plots

meteo_F_mounthly_2018 <- meteo_F_mounthly[meteo_F_mounthly$year == 2018, ]
meteo_F_mounthly_2018 <- meteo_F_mounthly_2018[-37,]

str(meteo_F_mounthly_2018)

ggplot() +
  geom_line(aes(x = as.numeric(month), y = T_med, group = nombre, colour = nombre), data =meteo_F_mounthly_2018)

ggplot() +
  geom_line(aes(x = as.numeric(month), y = T_min, group = nombre, colour = nombre), data =meteo_F_mounthly_2018)

ggplot() +
  geom_line(aes(x = as.numeric(month), y = T_max, group = nombre, colour = nombre), data =meteo_F_mounthly_2018)


#Cambios
retiro <- meteo_F[meteo_F$nombre == "MADRID, RETIRO", ]
colmenar <- meteo_F[meteo_F$nombre == "COLMENAR VIEJO", ]
universitaria <- meteo_F[meteo_F$nombre == "MADRID, CIUDAD UNIVERSITARIA", ]
getafe <- meteo_F[meteo_F$nombre == "GETAFE", ]


colmenar$tmed_col <- colmenar$tmed
universitaria$tmed_uni <- universitaria$tmed
getafe$tmed_get <- getafe$tmed

isla <- sqldf("select r.*, c.tmed_col
              from retiro As r
              left join colmenar As c
              where c.date = r.date")

isla <- sqldf("select r.*, u.tmed_uni
              from isla As r
              left join universitaria As u
              where u.date = r.date")

isla <- sqldf("select r.*, c.tmed_get
              from isla As r
              left join getafe As c
              where c.date = r.date")


isla <- na.omit(isla)
#deltaT

isla$deltaT <- isla$tmed-isla$tmed_col
plot(isla$date, isla$deltaT, type = "l")

isla <- isla[isla$deltaT < 6, ]
isla <- isla[isla$deltaT > -2, ]
#Would be OK to analize these time series

#FE



#acumT


#SeasonDummy

meteo_F$season <- season


#Model set

model_set <- isla[, !colnames(isla) %in% c("fecha", "nombre", "provincia", "indicativo", "altitud", "tmed", "horatmin", "horatmax", "horaracha", "horaPresMax",
                                                 "horaPresMin", "date", "year", "hour", "month", "day")]

model_set <- na.omit(model_set)


set.seed(1)

index <- sample(c(1:length(model_set$tmax)), round(length(model_set$tmax)*.75))


#Try neuralnet


#would be nice to study imputation with mice here

maxs <- apply(model_set, 2, max) 

mins <- apply(model_set, 2, min)


scaled <- as.data.frame(scale(model_set, center = mins, scale = maxs - mins))

train_set <- scaled[index,]
test_set <- scaled[-index,]


f=as.formula(deltaT~.)

nn <- neuralnet(f,data=train_set, hidden=c(11,6), linear.output=T)


#Try xgboost

target <- train_set$deltaT

dtrain <- xgb.DMatrix(data=as.matrix(train_set[,-10]),label=target, missing=NA)

dtest <- xgb.DMatrix(data=as.matrix(test_set[,-10]), missing=NA)


xgb <- xgb.train(#params = param
                  data = dtrain
                 , nrounds = 100
                 , verbose = 1
                 , print_every_n = 5
)


preds <- predict(xgb,dtest)
preds <- preds*(maxs[10]-mins[10])+mins[10]
trues <- test_set$deltaT*(maxs[10]-mins[10])+mins[10]

rmse <- sqrt(sum((preds-trues)^2)/length(preds))

plot(preds, trues)

plot(preds, type="l")
lines(trues,col = "red")
