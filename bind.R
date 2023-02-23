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
library(rjson)





memory.limit(size=40000)

PATH <- "C:/Users/jcord/Documents/AIRTEC/R/"
setwd(PATH)

#First bind, ciudad universitaria

#Bacteria: load ciu subsetting farmacia from bacterias_long (microb.R)

# Ejecutar si queremos clases
# 
# bacterias_long <- bacterias_long_class
# hongos_long <- hongos_long_class

bacterias_ciu <- bacterias_long[bacterias_long$Place == "Farmacia", ]
bacterias_alc <- bacterias_long[bacterias_long$Place == "Alcala", ]
bacterias_ara <- bacterias_long[bacterias_long$Place == "Aranjuez", ]
bacterias_ret <- bacterias_long[bacterias_long$Place == "Arganzuela", ]
bacterias_bar <- bacterias_long[bacterias_long$Place == "Coslada", ]
bacterias_vil <- bacterias_long[bacterias_long$Place == "Villalba", ]
bacterias_alco <- bacterias_long[bacterias_long$Place == "Alcobendas", ]
bacterias_leg <- bacterias_long[bacterias_long$Place == "Leganes", ]
bacterias_ets <- bacterias_long[bacterias_long$Place == "ETSII", ]
bacterias_roz <- bacterias_long[bacterias_long$Place == "LasRozas", ]
bacterias_get <- bacterias_long[bacterias_long$Place == "Getafe", ]
bacterias_etsii <- bacterias_long[bacterias_long$Place == "ETSII", ]

#load hongos

hongos_ciu <- hongos_long[hongos_long$Place == "Farmacia", ]
hongos_alc <- hongos_long[hongos_long$Place == "Alcala", ]
hongos_ara <- hongos_long[hongos_long$Place == "Aranjuez", ]
hongos_ret <- hongos_long[hongos_long$Place == "Arganzuela", ]
hongos_bar <- hongos_long[hongos_long$Place == "Coslada", ]
hongos_vil <- hongos_long[hongos_long$Place == "Villalba", ]
hongos_alco <- hongos_long[hongos_long$Place == "Alcobendas", ]
hongos_leg <- hongos_long[hongos_long$Place == "Leganes", ]
hongos_ets <- hongos_long[hongos_long$Place == "ETSII", ]
hongos_roz <- hongos_long[hongos_long$Place == "LasRozas", ]
hongos_get <- hongos_long[hongos_long$Place == "Getafe", ]
hongos_etsii <- hongos_long[hongos_long$Place == "ETSII", ]

#Polen: load ciu subsetting FACULTAD from polen (polen.R)

polen_ciu <- polen[["FACULTAD"]]
polen_ciu <- polen_ciu[colnames(polen_ciu) %in% c("FECHA","CUPR","OLEA", "PLAN", "PLAT", "POAC", "PTOTAL")]

polen_alc <- polen[["ALCALA"]]
polen_alc <- polen_alc[colnames(polen_alc) %in% c("FECHA","CUPR","OLEA", "PLAN", "PLAT", "POAC", "PTOTAL")]

polen_ara <- polen[["ARANJUEZ"]]
polen_ara <- polen_ara[colnames(polen_ara) %in% c("FECHA","CUPR","OLEA", "PLAN", "PLAT", "POAC", "PTOTAL")]

polen_ret <- polen[["MADRID"]]
polen_ret <- polen_ret[colnames(polen_ret) %in% c("FECHA","CUPR","OLEA", "PLAN", "PLAT", "POAC", "PTOTAL")]

polen_bar <- polen[["COSLADA"]]
polen_bar <- polen_bar[colnames(polen_bar) %in% c("FECHA","CUPR","OLEA", "PLAN", "PLAT", "POAC", "PTOTAL")]

polen_vil <- polen[["VILLALBA"]]
polen_vil <- polen_vil[colnames(polen_vil) %in% c("FECHA","CUPR","OLEA", "PLAN", "PLAT", "POAC", "PTOTAL")]

polen_alco <- polen[["ALCOBENDAS"]]
polen_alco <- polen_alco[colnames(polen_alco) %in% c("FECHA","CUPR","OLEA", "PLAN", "PLAT", "POAC", "PTOTAL")]

polen_leg <- polen[["LEGANES"]]
polen_leg <- polen_leg[colnames(polen_leg) %in% c("FECHA","CUPR","OLEA", "PLAN", "PLAT", "POAC", "PTOTAL")]

polen_ets <- polen[["MADRID"]]
polen_ets <- polen_ets[colnames(polen_ets) %in% c("FECHA","CUPR","OLEA", "PLAN", "PLAT", "POAC", "PTOTAL")]

polen_roz <- polen[["ROZAS"]]
polen_roz <- polen_roz[colnames(polen_roz) %in% c("FECHA","CUPR","OLEA", "PLAN", "PLAT", "POAC", "PTOTAL")]

polen_get <- polen[["GETAFE"]]
polen_get <- polen_get[colnames(polen_get) %in% c("FECHA","CUPR","OLEA", "PLAN", "PLAT", "POAC", "PTOTAL")]


#AQS: load ciu subsetting CUATRO_CAMINOS from aqs (aq.R)

aqs_ciu <- aqs[aqs$NOMBRE == "CASA DE CAMPO", ]
aqs_ciu <- aqs_ciu[colnames(aqs_ciu) %in% c("AQISD","Station_type","year","month","day","date","weekday","poll","avg","max","unit")]
aqs_ciu <- dcast(aqs_ciu, AQISD + Station_type + year + month + day + date + weekday ~ poll + unit, value.var = "avg")

aqs_alc <- aqs[aqs$NOMBRE == "ALCALA DE HENARES", ]
aqs_alc <- aqs_alc[colnames(aqs_alc) %in% c("AQISD","Station_type","year","month","day","date","weekday","poll","avg","max","unit")]
aqs_alc <- dcast(aqs_alc, AQISD + Station_type + year + month + day + date + weekday ~ poll+unit, value.var = "avg")

aqs_ara <- aqs[aqs$NOMBRE == "ARANJUEZ", ]
aqs_ara <- aqs_ara[colnames(aqs_ara) %in% c("AQISD","Station_type","year","month","day","date","weekday","poll","avg","max","unit")]
aqs_ara <- dcast(aqs_ara, AQISD + Station_type + year + month + day + date + weekday + unit ~ poll, value.var = "avg")

aqs_ret <- aqs[aqs$NOMBRE == "RAMîN Y CAJAL", ]
aqs_ret <- aqs_ret[colnames(aqs_ret) %in% c("AQISD","Station_type","year","month","day","date","weekday","poll","avg","max","unit")]
aqs_ret <- dcast(aqs_ret, AQISD + Station_type + year + month + day + date + weekday ~ poll+unit, value.var = "avg")

aqs_bar <- aqs[aqs$NOMBRE == "COSLADA", ]
aqs_bar <- aqs_bar[colnames(aqs_bar) %in% c("AQISD","Station_type","year","month","day","date","weekday","poll","avg","max","unit")]
aqs_bar <- dcast(aqs_bar, AQISD + Station_type + year + month + day + date + weekday + unit ~ poll, value.var = "avg")

aqs_vil <- aqs[aqs$NOMBRE == "COLLADO VILLALBA", ]
aqs_vil <- aqs_vil[colnames(aqs_vil) %in% c("AQISD","Station_type","year","month","day","date","weekday","poll","avg","max","unit")]
aqs_vil <- dcast(aqs_vil, AQISD + Station_type + year + month + day + date + weekday + unit ~ poll, value.var = "avg")

aqs_alco <- aqs[aqs$NOMBRE == "ALCOBENDAS", ]
aqs_alco <- aqs_alco[colnames(aqs_alco) %in% c("AQISD","Station_type","year","month","day","date","weekday","poll","avg","max","unit")]
aqs_alco <- dcast(aqs_alco, AQISD + Station_type + year + month + day + date + weekday ~ poll+unit, value.var = "avg")

aqs_leg <- aqs[aqs$NOMBRE == "LEGANES", ]
aqs_leg <- aqs_leg[colnames(aqs_leg) %in% c("AQISD","Station_type","year","month","day","date","weekday","poll","avg","max","unit")]
aqs_leg <- dcast(aqs_leg, AQISD + Station_type + year + month + day + date + weekday ~ poll+unit, value.var = "avg")

aqs_ets <- aqs[aqs$NOMBRE == "Castellana", ]
aqs_ets <- aqs_ets[colnames(aqs_ets) %in% c("AQISD","Station_type","year","month","day","date","weekday","poll","avg","max","unit")]
aqs_ets <- dcast(aqs_ets, AQISD + Station_type + year + month + day + date + weekday ~ poll+unit, value.var = "avg")

aqs_roz <- aqs[aqs$NOMBRE == "MAJADAHONDA", ]
aqs_roz <- aqs_roz[colnames(aqs_roz) %in% c("AQISD","Station_type","year","month","day","date","weekday","poll","avg","max","unit")]
aqs_roz <- dcast(aqs_roz, AQISD + Station_type + year + month + day + date + weekday ~ poll + unit, value.var = "avg")

aqs_get <- aqs[aqs$NOMBRE == "Getafe", ]
aqs_get <- aqs_get[colnames(aqs_get) %in% c("AQISD","Station_type","year","month","day","date","weekday","poll","avg","max","unit")]
aqs_get$date <- paste0(aqs_get$year,"/",aqs_get$month,"/",aqs_get$day) %>% ymd() 
aqs_get <- dcast(aqs_get, AQISD + Station_type + year + month + day + date + weekday ~ poll , value.var = "avg", fun.aggregate=mean)


#meteo: load from AEMET OPEN DATA

#ciu

# download.file("https://opendata.aemet.es/opendata/sh/22ef9da0", destfile = "ciu_2015_2018.json")
# download.file("https://opendata.aemet.es/opendata/sh/22ef9da0", destfile = "ciu_1990_1994.json")
# download.file("https://opendata.aemet.es/opendata/sh/f805a8ae", destfile = "ciu_2000_2003.json")
# download.file("https://opendata.aemet.es/opendata/sh/0142c21d", destfile = "ciu_2004.json")
# download.file("https://opendata.aemet.es/opendata/sh/1bd9f720", destfile = "ciu_2005_2009.json")
# download.file("https://opendata.aemet.es/opendata/sh/880435f6", destfile = "ciu_2010_2013.json")
# download.file("https://opendata.aemet.es/opendata/sh/7b4ba2e9", destfile = "ciu_2014.json")

#alc
# 
# download.file("https://opendata.aemet.es/opendata/sh/96f9d1cb", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/alc/alc_1990_1994.json")
# download.file("https://opendata.aemet.es/opendata/sh/59ef5cc4", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/alc/alc_1995_1999.json")
# download.file("https://opendata.aemet.es/opendata/sh/f9916942", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/alc/alc_2000_2003.json")
# download.file("https://opendata.aemet.es/opendata/sh/fd8e4840", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/alc/alc_2004.json")
# download.file("https://opendata.aemet.es/opendata/sh/648c5ca8", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/alc/alc_2005_2009.json")
# download.file("https://opendata.aemet.es/opendata/sh/9cfa5538", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/alc/alc_2010_2014.json")
# download.file("https://opendata.aemet.es/opendata/sh/00854e04", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/alc/alc_2015_2018.json")

lista <- list.files("C:/Users/Jose María/Documents/AIRTEC/R/alc/", pattern = ".json")

setwd("C:/Users/Jose María/Documents/AIRTEC/R/alc/")

meteo_alc <- lapply(lista, function(x) fromJSON(file = x) %>% bind_rows()) %>% bind_rows()

meteo_alc$fecha <- ymd(meteo_alc$fecha)

#write.csv(meteo_alc, "meteo_alc.csv")


#ara

download.file("https://opendata.aemet.es/opendata/sh/309b847b", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/ara/ara_1995_1999.json")
download.file("https://opendata.aemet.es/opendata/sh/859c3b61", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/ara/ara_2000_2003.json")
download.file("https://opendata.aemet.es/opendata/sh/8b911cc9", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/ara/ara_2004.json")
download.file("https://opendata.aemet.es/opendata/sh/792d519c", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/ara/ara_2005_2009.json")
download.file("https://opendata.aemet.es/opendata/sh/329762af", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/ara/ara_2010_2014.json")
download.file("https://opendata.aemet.es/opendata/sh/4e5a3d77", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/ara/ara_2015_2018.json")

lista <- list.files("C:/Users/Jose María/Documents/AIRTEC/R/ara/", pattern = ".json")

setwd("C:/Users/Jose María/Documents/AIRTEC/R/ara/")

meteo_ara <- lapply(lista, function(x) fromJSON(file = x) %>% bind_rows()) %>% bind_rows()

meteo_ara$fecha <- ymd(meteo_ara$fecha)

#write.csv(meteo_ara, "meteo_ara.csv")



#ret

download.file("https://opendata.aemet.es/opendata/sh/e1d38785", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/ret/ret_1990_1994.json")
download.file("https://opendata.aemet.es/opendata/sh/82094459", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/ret/ret_1995_1999.json")
download.file("https://opendata.aemet.es/opendata/sh/8b9af965", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/ret/ret_2000_2003.json")
download.file("https://opendata.aemet.es/opendata/sh/0343cfd6", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/ret/ret_2004.json")
download.file("https://opendata.aemet.es/opendata/sh/ac860e6c", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/ret/ret_2005_2009.json")
download.file("https://opendata.aemet.es/opendata/sh/b4ee4667", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/ret/ret_2010_2014.json")
download.file("https://opendata.aemet.es/opendata/sh/1462be4c", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/ret/ret_2015_2018.json")
download.file("https://opendata.aemet.es/opendata/sh/f4520630", destfile = "C:/Users/jcord/Documents/AIRTEC/R/ret/ret_2019.json")
download.file("https://opendata.aemet.es/opendata/sh/467ea8e8", destfile = "C:/Users/jcord/Documents/AIRTEC/R/ret/ret_2020.json")
#download.file("https://opendata.aemet.es/opendata/sh/a6dff697", destfile = "C:/Users/jcord/Documents/AIRTEC/R/ret/ret_2021_dic.json")


lista <- list.files("C:/Users/jcord/Documents/AIRTEC/R/ret/", pattern = ".json")

setwd("C:/Users/jcord/Documents/AIRTEC/R/ret/")

meteo_ret <- lapply(lista, function(x) fromJSON(file = x) %>% bind_rows()) %>% bind_rows()

meteo_ret$fecha <- ymd(meteo_ret$fecha)

#write.csv(meteo_ret, "meteo_ret.csv")



#bar

download.file("https://opendata.aemet.es/opendata/sh/53780665", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/bar/bar_1990_1994.json")
download.file("https://opendata.aemet.es/opendata/sh/c22ebb5c", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/bar/bar_1995_1999.json")
download.file("https://opendata.aemet.es/opendata/sh/c5dc9e12", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/bar/bar_2000_2003.json")
download.file("https://opendata.aemet.es/opendata/sh/7ffff39a", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/bar/bar_2004.json")
download.file("https://opendata.aemet.es/opendata/sh/ee50ae34", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/bar/bar_2005_2009.json")
download.file("https://opendata.aemet.es/opendata/sh/95371f65", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/bar/bar_2010_2014.json")
download.file("https://opendata.aemet.es/opendata/sh/6eeb0852", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/bar/bar_2015_2018.json")

lista <- list.files("C:/Users/Jose María/Documents/AIRTEC/R/bar/", pattern = ".json")

setwd("C:/Users/Jose María/Documents/AIRTEC/R/bar/")

meteo_bar <- lapply(lista, function(x) fromJSON(file = x) %>% bind_rows()) %>% bind_rows()

meteo_bar$fecha <- ymd(meteo_bar$fecha)

#write.csv(meteo_bar, "meteo_bar.csv")



#vil colmenar

download.file("https://opendata.aemet.es/opendata/sh/9bde3b0f", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/vil/vil_1990_1994.json")
download.file("https://opendata.aemet.es/opendata/sh/bb2d7f51", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/vil/vil_1995_1999.json")
download.file("https://opendata.aemet.es/opendata/sh/9b7cab34", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/vil/vil_2000_2003.json")
download.file("https://opendata.aemet.es/opendata/sh/c44d2e49", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/vil/vil_2004.json")
download.file("https://opendata.aemet.es/opendata/sh/798fd52a", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/vil/vil_2005_2009.json")
download.file("https://opendata.aemet.es/opendata/sh/b6e6268b", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/vil/vil_2010_2014.json")
download.file("https://opendata.aemet.es/opendata/sh/e1e2082c", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/vil/vil_2015_2018.json")

lista <- list.files("C:/Users/Jose María/Documents/AIRTEC/R/vil/", pattern = ".json")

setwd("C:/Users/Jose María/Documents/AIRTEC/R/vil/")

meteo_vil <- lapply(lista, function(x) fromJSON(file = x) %>% bind_rows()) %>% bind_rows()

meteo_vil$fecha <- ymd(meteo_vil$fecha)

#write.csv(meteo_vil, "meteo_vil.csv")




#cuatro vientos

download.file("https://opendata.aemet.es/opendata/sh/f7bd5f30", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/leg/leg_1990_1994.json")
download.file("https://opendata.aemet.es/opendata/sh/e59787a7", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/leg/leg_1995_1999.json")
download.file("https://opendata.aemet.es/opendata/sh/24ebd2d6", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/leg/leg_2000_2003.json")
download.file("https://opendata.aemet.es/opendata/sh/0555d5bb", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/leg/leg_2004.json")
download.file("https://opendata.aemet.es/opendata/sh/4c0a1139", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/leg/leg_2005_2009.json")
download.file("https://opendata.aemet.es/opendata/sh/d5384bc2", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/leg/leg_2010_2014.json")
download.file("https://opendata.aemet.es/opendata/sh/943389a0", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/leg/leg_2015_2018.json")

lista <- list.files("C:/Users/Jose María/Documents/AIRTEC/R/leg/", pattern = ".json")

setwd("C:/Users/Jose María/Documents/AIRTEC/R/leg/")

meteo_leg <- lapply(lista, function(x) fromJSON(file = x) %>% bind_rows()) %>% bind_rows()

meteo_leg$fecha <- ymd(meteo_leg$fecha)

#write.csv(meteo_leg, "meteo_leg.csv")




#Getafe

download.file("https://opendata.aemet.es/opendata/sh/24b02e8c", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/get/get_1990_1994.json")
download.file("https://opendata.aemet.es/opendata/sh/05ba7ca7", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/get/get_1995_1999.json")
download.file("https://opendata.aemet.es/opendata/sh/af26bc4a", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/get/get_2000_2003.json")
download.file("https://opendata.aemet.es/opendata/sh/100df56c", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/get/get_2004.json")
download.file("https://opendata.aemet.es/opendata/sh/b27b3d5c", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/get/get_2005_2009.json")
download.file("https://opendata.aemet.es/opendata/sh/8ec725c0", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/get/get_2010_2014.json")
download.file("https://opendata.aemet.es/opendata/sh/828223d6", destfile = "C:/Users/Jose María/Documents/AIRTEC/R/get/get_2015_2018.json")

lista <- list.files("C:/Users/Jose María/Documents/AIRTEC/R/get/", pattern = ".json")

setwd("C:/Users/Jose María/Documents/AIRTEC/R/get/")

meteo_get <- lapply(lista, function(x) fromJSON(file = x) %>% bind_rows()) %>% bind_rows()

meteo_get$fecha <- ymd(meteo_get$fecha)

#write.csv(meteo_get, "meteo_get.csv")






setwd("C:/Users/Jose María/Documents/AIRTEC/R/ciu/")

lista <- list.files(pattern = ".json")

meteo_ciu <- lapply(lista, function(x) fromJSON(file = x) %>% bind_rows()) %>% bind_rows()


meteo_ciu$fecha <- ymd(meteo_ciu$fecha)

#write.csv(meteo_ciu, "meteo_ciu.csv")

meteo_ciu <- meteo_ciu[colnames(meteo_ciu) %in% c("fecha","tmed","prec","tmin","tmax","dir","velmedia")]




#Start joining

humedad <- read_csv("C:/Users/jcord/Documents/AIRTEC/OBJETIVO_1/Polen/2019_DATOS_METEO/humedad_juntos_bacterias.csv")

colnames(humedad) <- c("year","month","day","NOMBRE","RH")

humedad$FECHA <- paste0(humedad$year,"/",humedad$month,"/",humedad$day) %>% ymd()


#ciu

polen_aqs_ciu <- merge(polen_ciu, aqs_ciu, by.x = "FECHA", by.y = "date")

polen_aqs_meteo_ciu <- merge(polen_aqs_ciu, meteo_ciu, by.x = "FECHA", by.y = "fecha")
str(polen_aqs_meteo_ciu)
summary(polen_aqs_meteo_ciu)



polen_aqs_meteo_ciu$campaign <- 0

for (i in c(1:length(polen_aqs_meteo_ciu$FECHA))) {
  if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2015-11-23") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2015-11-30"))
    polen_aqs_meteo_ciu$campaign[i] = "Fall1"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2015-07-20") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2015-07-27"))
    polen_aqs_meteo_ciu$campaign[i] = "Summer1"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2016-02-26") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2016-03-04"))
    polen_aqs_meteo_ciu$campaign[i] = "Winter1"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2016-04-29") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2016-05-06"))
    polen_aqs_meteo_ciu$campaign[i] = "Spring1"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2016-07-11") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2016-07-18"))
    polen_aqs_meteo_ciu$campaign[i] = "Summer2"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2016-11-14") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2016-11-21"))
    polen_aqs_meteo_ciu$campaign[i] = "Fall2"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2017-02-06") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2017-02-13"))
    polen_aqs_meteo_ciu$campaign[i] = "Winter2"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2017-04-28") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2017-05-05"))
    polen_aqs_meteo_ciu$campaign[i] = "Spring2"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2015-03-02") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2015-03-09"))
    polen_aqs_meteo_ciu$campaign[i] = "mar2015"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2015-04-21") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2015-04-28"))
    polen_aqs_meteo_ciu$campaign[i] = "abr2015"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2017-05-22") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2017-05-28"))
    polen_aqs_meteo_ciu$campaign[i] = "may2017"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2017-03-14") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2017-03-21"))
    polen_aqs_meteo_ciu$campaign[i] = "mar2017"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2016-06-22") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2016-06-29"))
    polen_aqs_meteo_ciu$campaign[i] = "jun2016"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2015-12-02") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2015-12-08"))
    polen_aqs_meteo_ciu$campaign[i] = "dic2015-1"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2015-12-09") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2015-12-15"))
    polen_aqs_meteo_ciu$campaign[i] = "dic2015-2"
  else if (polen_aqs_meteo_ciu$FECHA[i] >= ymd("2015-12-16") && polen_aqs_meteo_ciu$FECHA[i] <= ymd("2015-12-23"))
    polen_aqs_meteo_ciu$campaign[i] = "dic2015-3"
}

p <- humedad[humedad$NOMBRE=='MADRID, C. UNIVERSITARIA',]


polen_aqs_meteo_ciu <- sqldf("select a.*, b.RH
                              from polen_aqs_meteo_ciu As a
                              left join p As b
                              on a.FECHA = b.FECHA
                             ")

# write.csv(polen_aqs_meteo_ciu, "polen_aqs_meteo_ciu.csv")

polen_aqs_meteo_ciu <- polen_aqs_meteo_ciu[polen_aqs_meteo_ciu$campaign != 0, ]

ciu_complete <- merge(polen_aqs_meteo_ciu, bacterias_ciu, type = "full outer", by = "campaign")

ciu_complete <- merge(ciu_complete, hongos_ciu, type = "full outer", by = "campaign")

ciu_complete <- ciu_complete[order(ciu_complete$campaign, ciu_complete$FECHA), ]

# write.csv(ciu_complete, "ciu_complete_class.csv")

ciu_complete <- read.csv("ciu_complete.csv")




#alc

polen_aqs_alc <- merge(polen_alc, aqs_alc, by.x = "FECHA", by.y = "date")

polen_aqs_meteo_alc <- merge(polen_aqs_alc, meteo_alc, by.x = "FECHA", by.y = "fecha")
str(polen_aqs_meteo_alc)
summary(polen_aqs_meteo_alc)



polen_aqs_meteo_alc$campaign <- 0

for (i in c(1:length(polen_aqs_meteo_alc$FECHA))) {
  if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2015-11-23") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2015-11-30"))
    polen_aqs_meteo_alc$campaign[i] = "Fall1"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2015-07-20") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2015-07-27"))
    polen_aqs_meteo_alc$campaign[i] = "Summer1"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2016-02-26") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2016-03-04"))
    polen_aqs_meteo_alc$campaign[i] = "Winter1"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2016-04-29") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2016-05-06"))
    polen_aqs_meteo_alc$campaign[i] = "Spring1"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2016-07-11") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2016-07-18"))
    polen_aqs_meteo_alc$campaign[i] = "Summer2"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2016-11-14") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2016-11-21"))
    polen_aqs_meteo_alc$campaign[i] = "Fall2"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2017-02-06") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2017-02-13"))
    polen_aqs_meteo_alc$campaign[i] = "Winter2"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2017-04-28") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2017-05-05"))
    polen_aqs_meteo_alc$campaign[i] = "Spring2"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2015-03-02") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2015-03-09"))
    polen_aqs_meteo_alc$campaign[i] = "mar2015"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2015-04-21") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2015-04-28"))
    polen_aqs_meteo_alc$campaign[i] = "abr2015"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2017-05-22") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2017-05-28"))
    polen_aqs_meteo_alc$campaign[i] = "may2017"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2017-03-14") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2017-03-21"))
    polen_aqs_meteo_alc$campaign[i] = "mar2017"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2016-06-22") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2016-06-29"))
    polen_aqs_meteo_alc$campaign[i] = "jun2016"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2015-12-02") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2015-12-08"))
    polen_aqs_meteo_alc$campaign[i] = "dic2015-1"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2015-12-09") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2015-12-15"))
    polen_aqs_meteo_alc$campaign[i] = "dic2015-2"
  else if (polen_aqs_meteo_alc$FECHA[i] >= ymd("2015-12-16") && polen_aqs_meteo_alc$FECHA[i] <= ymd("2015-12-23"))
    polen_aqs_meteo_alc$campaign[i] = "dic2015-3"
}

p <- humedad[humedad$NOMBRE=="MADRID/BARAJAS",]


polen_aqs_meteo_alc <- sqldf("select a.*, b.RH
                              from polen_aqs_meteo_alc As a
                              left join p As b
                              on a.FECHA = b.FECHA
                             ")

write.csv(polen_aqs_meteo_alc, "polen_aqs_meteo_alc.csv")

polen_aqs_meteo_alc <- polen_aqs_meteo_alc[polen_aqs_meteo_alc$campaign != 0, ]



alc_complete <- merge(polen_aqs_meteo_alc, bacterias_alc, type = "full outer", by = "campaign")

alc_complete <- merge(alc_complete, hongos_alc, type = "full outer", by = "campaign")

alc_complete <- alc_complete[order(alc_complete$campaign, alc_complete$FECHA), ]

write.csv(alc_complete, "alc_complete_class.csv")

alc_complete <- read.csv("alc_complete.csv")




#ara

polen_aqs_ara <- merge(polen_ara, aqs_ara, by.x = "FECHA", by.y = "date")

polen_aqs_meteo_ara <- merge(polen_aqs_ara, meteo_ara, by.x = "FECHA", by.y = "fecha")
str(polen_aqs_meteo_ara)
summary(polen_aqs_meteo_ara)



polen_aqs_meteo_ara$campaign <- 0

for (i in c(1:length(polen_aqs_meteo_ara$FECHA))) {
  if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2015-11-23") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2015-11-30"))
    polen_aqs_meteo_ara$campaign[i] = "Fall1"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2015-07-20") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2015-07-27"))
    polen_aqs_meteo_ara$campaign[i] = "Summer1"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2016-02-26") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2016-03-04"))
    polen_aqs_meteo_ara$campaign[i] = "Winter1"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2016-04-29") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2016-05-06"))
    polen_aqs_meteo_ara$campaign[i] = "Spring1"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2016-07-11") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2016-07-18"))
    polen_aqs_meteo_ara$campaign[i] = "Summer2"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2016-11-14") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2016-11-21"))
    polen_aqs_meteo_ara$campaign[i] = "Fall2"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2017-02-06") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2017-02-13"))
    polen_aqs_meteo_ara$campaign[i] = "Winter2"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2017-04-28") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2017-05-05"))
    polen_aqs_meteo_ara$campaign[i] = "Spring2"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2015-03-02") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2015-03-09"))
    polen_aqs_meteo_ara$campaign[i] = "mar2015"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2015-04-21") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2015-04-28"))
    polen_aqs_meteo_ara$campaign[i] = "abr2015"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2017-05-22") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2017-05-28"))
    polen_aqs_meteo_ara$campaign[i] = "may2017"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2017-03-14") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2017-03-21"))
    polen_aqs_meteo_ara$campaign[i] = "mar2017"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2016-06-22") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2016-06-29"))
    polen_aqs_meteo_ara$campaign[i] = "jun2016"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2015-12-02") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2015-12-08"))
    polen_aqs_meteo_ara$campaign[i] = "dic2015-1"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2015-12-09") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2015-12-15"))
    polen_aqs_meteo_ara$campaign[i] = "dic2015-2"
  else if (polen_aqs_meteo_ara$FECHA[i] >= ymd("2015-12-16") && polen_aqs_meteo_ara$FECHA[i] <= ymd("2015-12-23"))
    polen_aqs_meteo_ara$campaign[i] = "dic2015-3"
}

p <- humedad[humedad$NOMBRE=="ARANJUEZ",]


polen_aqs_meteo_ara <- sqldf("select a.*, b.RH
                             from polen_aqs_meteo_ara As a
                             left join p As b
                             on a.FECHA = b.FECHA
                             ")

write.csv(polen_aqs_meteo_ara, "polen_aqs_meteo_ara.csv")

polen_aqs_meteo_ara <- polen_aqs_meteo_ara[polen_aqs_meteo_ara$campaign != 0, ]



ara_complete <- merge(polen_aqs_meteo_ara, bacterias_ara, type = "full outer", by = "campaign")

ara_complete <- merge(ara_complete, hongos_ara, type = "full outer", by = "campaign")

ara_complete <- ara_complete[order(ara_complete$campaign, ara_complete$FECHA), ]

write.csv(ara_complete, "ara_complete_v2.csv")




#bar

polen_aqs_bar <- merge(polen_bar, aqs_bar, by.x = "FECHA", by.y = "date")

polen_aqs_meteo_bar <- merge(polen_aqs_bar, meteo_bar, by.x = "FECHA", by.y = "fecha")
str(polen_aqs_meteo_bar)
summary(polen_aqs_meteo_bar)



polen_aqs_meteo_bar$campaign <- 0

for (i in c(1:length(polen_aqs_meteo_bar$FECHA))) {
  if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2015-11-23") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2015-11-30"))
    polen_aqs_meteo_bar$campaign[i] = "Fall1"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2015-07-20") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2015-07-27"))
    polen_aqs_meteo_bar$campaign[i] = "Summer1"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2016-02-26") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2016-03-04"))
    polen_aqs_meteo_bar$campaign[i] = "Winter1"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2016-04-29") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2016-05-06"))
    polen_aqs_meteo_bar$campaign[i] = "Spring1"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2016-07-11") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2016-07-18"))
    polen_aqs_meteo_bar$campaign[i] = "Summer2"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2016-11-14") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2016-11-21"))
    polen_aqs_meteo_bar$campaign[i] = "Fall2"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2017-02-06") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2017-02-13"))
    polen_aqs_meteo_bar$campaign[i] = "Winter2"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2017-04-28") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2017-05-05"))
    polen_aqs_meteo_bar$campaign[i] = "Spring2"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2015-03-02") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2015-03-09"))
    polen_aqs_meteo_bar$campaign[i] = "mar2015"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2015-04-21") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2015-04-28"))
    polen_aqs_meteo_bar$campaign[i] = "abr2015"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2017-05-22") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2017-05-28"))
    polen_aqs_meteo_bar$campaign[i] = "may2017"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2017-03-14") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2017-03-21"))
    polen_aqs_meteo_bar$campaign[i] = "mar2017"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2016-06-22") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2016-06-29"))
    polen_aqs_meteo_bar$campaign[i] = "jun2016"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2015-12-02") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2015-12-08"))
    polen_aqs_meteo_bar$campaign[i] = "dic2015-1"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2015-12-09") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2015-12-15"))
    polen_aqs_meteo_bar$campaign[i] = "dic2015-2"
  else if (polen_aqs_meteo_bar$FECHA[i] >= ymd("2015-12-16") && polen_aqs_meteo_bar$FECHA[i] <= ymd("2015-12-23"))
    polen_aqs_meteo_bar$campaign[i] = "dic2015-3"
}

p <- humedad[humedad$NOMBRE=="MADRID/BARAJAS",]


polen_aqs_meteo_bar <- sqldf("select a.*, b.RH
                             from polen_aqs_meteo_bar As a
                             left join p As b
                             on a.FECHA = b.FECHA
                             ")

write.csv(polen_aqs_meteo_bar, "polen_aqs_meteo_bar.csv")

polen_aqs_meteo_bar <- polen_aqs_meteo_bar[polen_aqs_meteo_bar$campaign != 0, ]



bar_complete <- merge(polen_aqs_meteo_bar, bacterias_bar, type = "full outer", by = "campaign")

bar_complete <- merge(bar_complete, hongos_bar, type = "full outer", by = "campaign")

bar_complete <- bar_complete[order(bar_complete$campaign, bar_complete$FECHA), ]

write.csv(bar_complete, "bar_complete_class.csv")




#ret

polen_aqs_ret <- merge(polen_ret, aqs_ret, by.x = "FECHA", by.y = "date")

polen_aqs_meteo_ret <- merge(polen_aqs_ret, meteo_ret, by.x = "FECHA", by.y = "fecha")
str(polen_aqs_meteo_ret)
summary(polen_aqs_meteo_ret)



polen_aqs_meteo_ret$campaign <- 0

for (i in c(1:length(polen_aqs_meteo_ret$FECHA))) {
  if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2015-11-23") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2015-11-30"))
    polen_aqs_meteo_ret$campaign[i] = "Fall1"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2015-07-20") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2015-07-27"))
    polen_aqs_meteo_ret$campaign[i] = "Summer1"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2016-02-26") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2016-03-04"))
    polen_aqs_meteo_ret$campaign[i] = "Winter1"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2016-04-29") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2016-05-06"))
    polen_aqs_meteo_ret$campaign[i] = "Spring1"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2016-07-11") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2016-07-18"))
    polen_aqs_meteo_ret$campaign[i] = "Summer2"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2016-11-14") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2016-11-21"))
    polen_aqs_meteo_ret$campaign[i] = "Fall2"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2017-02-06") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2017-02-13"))
    polen_aqs_meteo_ret$campaign[i] = "Winter2"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2017-04-28") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2017-05-05"))
    polen_aqs_meteo_ret$campaign[i] = "Spring2"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2015-03-02") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2015-03-09"))
    polen_aqs_meteo_ret$campaign[i] = "mar2015"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2015-04-21") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2015-04-28"))
    polen_aqs_meteo_ret$campaign[i] = "abr2015"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2017-05-22") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2017-05-28"))
    polen_aqs_meteo_ret$campaign[i] = "may2017"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2017-03-14") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2017-03-21"))
    polen_aqs_meteo_ret$campaign[i] = "mar2017"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2016-06-22") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2016-06-29"))
    polen_aqs_meteo_ret$campaign[i] = "jun2016"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2015-12-02") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2015-12-08"))
    polen_aqs_meteo_ret$campaign[i] = "dic2015-1"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2015-12-09") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2015-12-15"))
    polen_aqs_meteo_ret$campaign[i] = "dic2015-2"
  else if (polen_aqs_meteo_ret$FECHA[i] >= ymd("2015-12-16") && polen_aqs_meteo_ret$FECHA[i] <= ymd("2015-12-23"))
    polen_aqs_meteo_ret$campaign[i] = "dic2015-3"
}

p <- humedad[humedad$NOMBRE=="MADRID, RETIRO" | humedad$NOMBRE=="MADRID,RETIRO",]


polen_aqs_meteo_ret <- sqldf("select a.*, b.RH
                             from polen_aqs_meteo_ret As a
                             left join p As b
                             on a.FECHA = b.FECHA
                             ")

write.csv(polen_aqs_meteo_ret, "polen_aqs_meteo_ret.csv")

polen_aqs_meteo_ret <- polen_aqs_meteo_ret[polen_aqs_meteo_ret$campaign != 0, ]



ret_complete <- merge(polen_aqs_meteo_ret, bacterias_ret, type = "full outer", by = "campaign")

ret_complete <- merge(ret_complete, hongos_ret, type = "full outer", by = "campaign")

ret_complete <- ret_complete[order(ret_complete$campaign, ret_complete$FECHA), ]

write.csv(ret_complete, "ret_complete_class.csv")



#vil

polen_aqs_vil <- merge(polen_vil, aqs_vil, by.x = "FECHA", by.y = "date")

polen_aqs_meteo_vil <- merge(polen_aqs_vil, meteo_vil, by.x = "FECHA", by.y = "fecha")
str(polen_aqs_meteo_vil)
summary(polen_aqs_meteo_vil)



polen_aqs_meteo_vil$campaign <- 0

for (i in c(1:length(polen_aqs_meteo_vil$FECHA))) {
  if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2015-11-23") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2015-11-30"))
    polen_aqs_meteo_vil$campaign[i] = "Fall1"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2015-07-20") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2015-07-27"))
    polen_aqs_meteo_vil$campaign[i] = "Summer1"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2016-02-26") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2016-03-04"))
    polen_aqs_meteo_vil$campaign[i] = "Winter1"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2016-04-29") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2016-05-06"))
    polen_aqs_meteo_vil$campaign[i] = "Spring1"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2016-07-11") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2016-07-18"))
    polen_aqs_meteo_vil$campaign[i] = "Summer2"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2016-11-14") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2016-11-21"))
    polen_aqs_meteo_vil$campaign[i] = "Fall2"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2017-02-06") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2017-02-13"))
    polen_aqs_meteo_vil$campaign[i] = "Winter2"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2017-04-28") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2017-05-05"))
    polen_aqs_meteo_vil$campaign[i] = "Spring2"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2015-03-02") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2015-03-09"))
    polen_aqs_meteo_vil$campaign[i] = "mar2015"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2015-04-21") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2015-04-28"))
    polen_aqs_meteo_vil$campaign[i] = "abr2015"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2017-05-22") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2017-05-28"))
    polen_aqs_meteo_vil$campaign[i] = "may2017"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2017-03-14") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2017-03-21"))
    polen_aqs_meteo_vil$campaign[i] = "mar2017"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2016-06-22") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2016-06-29"))
    polen_aqs_meteo_vil$campaign[i] = "jun2016"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2015-12-02") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2015-12-08"))
    polen_aqs_meteo_vil$campaign[i] = "dic2015-1"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2015-12-09") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2015-12-15"))
    polen_aqs_meteo_vil$campaign[i] = "dic2015-2"
  else if (polen_aqs_meteo_vil$FECHA[i] >= ymd("2015-12-16") && polen_aqs_meteo_vil$FECHA[i] <= ymd("2015-12-23"))
    polen_aqs_meteo_vil$campaign[i] = "dic2015-3"
}

p <- humedad[humedad$NOMBRE=="ALPEDRETE",]


polen_aqs_meteo_vil <- sqldf("select a.*, b.RH
                             from polen_aqs_meteo_vil As a
                             left join p As b
                             on a.FECHA = b.FECHA
                             ")

write.csv(polen_aqs_meteo_vil, "polen_aqs_meteo_vil.csv")

polen_aqs_meteo_vil <- polen_aqs_meteo_vil[polen_aqs_meteo_vil$campaign != 0, ]



vil_complete <- merge(polen_aqs_meteo_vil, bacterias_vil, type = "full outer", by = "campaign")

vil_complete <- merge(vil_complete, hongos_vil, type = "full outer", by = "campaign")

vil_complete <- vil_complete[order(vil_complete$campaign, vil_complete$FECHA), ]

write.csv(vil_complete, "vil_complete_class.csv")





#alco

polen_aqs_alco <- merge(polen_alco, aqs_alco, by.x = "FECHA", by.y = "date")

polen_aqs_meteo_alco <- merge(polen_aqs_alco, meteo_bar, by.x = "FECHA", by.y = "fecha")
str(polen_aqs_meteo_alco)
summary(polen_aqs_meteo_alco)



polen_aqs_meteo_alco$campaign <- 0

for (i in c(1:length(polen_aqs_meteo_alco$FECHA))) {
  if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2015-11-23") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2015-11-30"))
    polen_aqs_meteo_alco$campaign[i] = "Fall1"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2015-07-20") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2015-07-27"))
    polen_aqs_meteo_alco$campaign[i] = "Summer1"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2016-02-26") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2016-03-04"))
    polen_aqs_meteo_alco$campaign[i] = "Winter1"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2016-04-29") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2016-05-06"))
    polen_aqs_meteo_alco$campaign[i] = "Spring1"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2016-07-11") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2016-07-18"))
    polen_aqs_meteo_alco$campaign[i] = "Summer2"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2016-11-14") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2016-11-21"))
    polen_aqs_meteo_alco$campaign[i] = "Fall2"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2017-02-06") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2017-02-13"))
    polen_aqs_meteo_alco$campaign[i] = "Winter2"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2017-04-28") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2017-05-05"))
    polen_aqs_meteo_alco$campaign[i] = "Spring2"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2015-03-02") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2015-03-09"))
    polen_aqs_meteo_alco$campaign[i] = "mar2015"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2015-04-21") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2015-04-28"))
    polen_aqs_meteo_alco$campaign[i] = "abr2015"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2017-05-22") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2017-05-28"))
    polen_aqs_meteo_alco$campaign[i] = "may2017"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2017-03-14") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2017-03-21"))
    polen_aqs_meteo_alco$campaign[i] = "mar2017"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2016-06-22") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2016-06-29"))
    polen_aqs_meteo_alco$campaign[i] = "jun2016"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2015-12-02") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2015-12-08"))
    polen_aqs_meteo_alco$campaign[i] = "dic2015-1"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2015-12-09") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2015-12-15"))
    polen_aqs_meteo_alco$campaign[i] = "dic2015-2"
  else if (polen_aqs_meteo_alco$FECHA[i] >= ymd("2015-12-16") && polen_aqs_meteo_alco$FECHA[i] <= ymd("2015-12-23"))
    polen_aqs_meteo_alco$campaign[i] = "dic2015-3"
}

library(stringr)
mask <- str_detect(humedad$NOMBRE, "SAN")

p <- humedad[mask,]


polen_aqs_meteo_alco <- sqldf("select a.*, b.RH
                             from polen_aqs_meteo_alco As a
                             left join p As b
                             on a.FECHA = b.FECHA
                             ")

write.csv(polen_aqs_meteo_alco, "polen_aqs_meteo_alco.csv")

polen_aqs_meteo_alco <- polen_aqs_meteo_alco[polen_aqs_meteo_alco$campaign != 0, ]



alco_complete <- merge(polen_aqs_meteo_alco, bacterias_alco, type = "full outer", by = "campaign")

alco_complete <- merge(alco_complete, hongos_alco, type = "full outer", by = "campaign")

alco_complete <- alco_complete[order(alco_complete$campaign, alco_complete$FECHA), ]

write.csv(alco_complete, "alco_complete_class.csv")

alco_complete <- read.csv("alco_complete.csv")




#leg

polen_aqs_leg <- merge(polen_leg, aqs_leg, by.x = "FECHA", by.y = "date")

polen_aqs_meteo_leg <- merge(polen_aqs_leg, meteo_leg, by.x = "FECHA", by.y = "fecha")
str(polen_aqs_meteo_leg)
summary(polen_aqs_meteo_leg)



polen_aqs_meteo_leg$campaign <- 0

for (i in c(1:length(polen_aqs_meteo_leg$FECHA))) {
  if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2015-11-23") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2015-11-30"))
    polen_aqs_meteo_leg$campaign[i] = "Fall1"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2015-07-20") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2015-07-27"))
    polen_aqs_meteo_leg$campaign[i] = "Summer1"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2016-02-26") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2016-03-04"))
    polen_aqs_meteo_leg$campaign[i] = "Winter1"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2016-04-29") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2016-05-06"))
    polen_aqs_meteo_leg$campaign[i] = "Spring1"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2016-07-11") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2016-07-18"))
    polen_aqs_meteo_leg$campaign[i] = "Summer2"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2016-11-14") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2016-11-21"))
    polen_aqs_meteo_leg$campaign[i] = "Fall2"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2017-02-06") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2017-02-13"))
    polen_aqs_meteo_leg$campaign[i] = "Winter2"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2017-04-28") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2017-05-05"))
    polen_aqs_meteo_leg$campaign[i] = "Spring2"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2015-03-02") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2015-03-09"))
    polen_aqs_meteo_leg$campaign[i] = "mar2015"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2015-04-21") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2015-04-28"))
    polen_aqs_meteo_leg$campaign[i] = "abr2015"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2017-05-22") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2017-05-28"))
    polen_aqs_meteo_leg$campaign[i] = "may2017"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2017-03-14") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2017-03-21"))
    polen_aqs_meteo_leg$campaign[i] = "mar2017"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2016-06-22") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2016-06-29"))
    polen_aqs_meteo_leg$campaign[i] = "jun2016"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2015-12-02") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2015-12-08"))
    polen_aqs_meteo_leg$campaign[i] = "dic2015-1"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2015-12-09") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2015-12-15"))
    polen_aqs_meteo_leg$campaign[i] = "dic2015-2"
  else if (polen_aqs_meteo_leg$FECHA[i] >= ymd("2015-12-16") && polen_aqs_meteo_leg$FECHA[i] <= ymd("2015-12-23"))
    polen_aqs_meteo_leg$campaign[i] = "dic2015-3"
}

p <- humedad[humedad$NOMBRE=="MADRID, RETIRO" | humedad$NOMBRE=="MADRID,RETIRO",]


polen_aqs_meteo_leg <- sqldf("select a.*, b.RH
                             from polen_aqs_meteo_leg As a
                             left join p As b
                             on a.FECHA = b.FECHA
                             ")

write.csv(polen_aqs_meteo_leg, "polen_aqs_meteo_leg.csv")

polen_aqs_meteo_leg <- polen_aqs_meteo_leg[polen_aqs_meteo_leg$campaign != 0, ]



leg_complete <- merge(polen_aqs_meteo_leg, bacterias_leg, type = "full outer", by = "campaign")

leg_complete <- merge(leg_complete, hongos_leg, type = "full outer", by = "campaign")

leg_complete <- leg_complete[order(leg_complete$campaign, leg_complete$FECHA), ]

write.csv(leg_complete, "leg_complete_class.csv")

leg_complete <- read.csv("leg_complete.csv")




#roz

# setwd("C:/Users/Jose María/Documents/AIRTEC/R/roz")

polen_aqs_roz <- merge(polen_roz, aqs_roz, by.x = "FECHA", by.y = "date")

polen_aqs_meteo_roz <- merge(polen_aqs_roz, meteo_vil, by.x = "FECHA", by.y = "fecha")
str(polen_aqs_meteo_roz)
summary(polen_aqs_meteo_roz)



polen_aqs_meteo_roz$campaign <- 0

for (i in c(1:length(polen_aqs_meteo_roz$FECHA))) {
  if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2015-11-23") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2015-11-30"))
    polen_aqs_meteo_roz$campaign[i] = "Fall1"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2015-07-20") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2015-07-27"))
    polen_aqs_meteo_roz$campaign[i] = "Summer1"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2016-02-26") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2016-03-04"))
    polen_aqs_meteo_roz$campaign[i] = "Winter1"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2016-04-29") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2016-05-06"))
    polen_aqs_meteo_roz$campaign[i] = "Spring1"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2016-07-11") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2016-07-18"))
    polen_aqs_meteo_roz$campaign[i] = "Summer2"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2016-11-14") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2016-11-21"))
    polen_aqs_meteo_roz$campaign[i] = "Fall2"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2017-02-06") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2017-02-13"))
    polen_aqs_meteo_roz$campaign[i] = "Winter2"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2017-04-28") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2017-05-05"))
    polen_aqs_meteo_roz$campaign[i] = "Spring2"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2015-03-02") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2015-03-09"))
    polen_aqs_meteo_roz$campaign[i] = "mar2015"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2015-04-21") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2015-04-28"))
    polen_aqs_meteo_roz$campaign[i] = "abr2015"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2017-05-22") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2017-05-28"))
    polen_aqs_meteo_roz$campaign[i] = "may2017"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2017-03-14") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2017-03-21"))
    polen_aqs_meteo_roz$campaign[i] = "mar2017"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2016-06-22") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2016-06-29"))
    polen_aqs_meteo_roz$campaign[i] = "jun2016"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2015-12-02") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2015-12-08"))
    polen_aqs_meteo_roz$campaign[i] = "dic2015-1"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2015-12-09") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2015-12-15"))
    polen_aqs_meteo_roz$campaign[i] = "dic2015-2"
  else if (polen_aqs_meteo_roz$FECHA[i] >= ymd("2015-12-16") && polen_aqs_meteo_roz$FECHA[i] <= ymd("2015-12-23"))
    polen_aqs_meteo_roz$campaign[i] = "dic2015-3"
}

p <- humedad[humedad$NOMBRE=="MADRID, RETIRO" | humedad$NOMBRE=="MADRID,RETIRO",]


polen_aqs_meteo_roz <- sqldf("select a.*, b.RH
                             from polen_aqs_meteo_roz As a
                             left join p As b
                             on a.FECHA = b.FECHA
                             ")

write.csv(polen_aqs_meteo_roz, "polen_aqs_meteo_roz.csv")

polen_aqs_meteo_roz <- polen_aqs_meteo_roz[polen_aqs_meteo_roz$campaign != 0, ]



roz_complete <- merge(polen_aqs_meteo_roz, bacterias_roz, type = "full outer", by = "campaign")

roz_complete <- merge(roz_complete, hongos_roz, type = "full outer", by = "campaign")

roz_complete <- roz_complete[order(roz_complete$campaign, roz_complete$FECHA), ]

write.csv(roz_complete, "roz_complete_class.csv")

roz_complete <- read.csv("roz_complete.csv")




#get

polen_aqs_get <- merge(polen_get, aqs_get, by.x = "FECHA", by.y = "date")

polen_aqs_meteo_get <- merge(polen_aqs_get, meteo_get, by.x = "FECHA", by.y = "fecha")
str(polen_aqs_meteo_get)
summary(polen_aqs_meteo_get)



polen_aqs_meteo_get$campaign <- 0

for (i in c(1:length(polen_aqs_meteo_get$FECHA))) {
  if (polen_aqs_meteo_get$FECHA[i] >= ymd("2015-11-23") && polen_aqs_meteo_get$FECHA[i] <= ymd("2015-11-30"))
    polen_aqs_meteo_get$campaign[i] = "Fall1"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2015-07-20") && polen_aqs_meteo_get$FECHA[i] <= ymd("2015-07-27"))
    polen_aqs_meteo_get$campaign[i] = "Summer1"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2016-02-26") && polen_aqs_meteo_get$FECHA[i] <= ymd("2016-03-04"))
    polen_aqs_meteo_get$campaign[i] = "Winter1"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2016-04-29") && polen_aqs_meteo_get$FECHA[i] <= ymd("2016-05-06"))
    polen_aqs_meteo_get$campaign[i] = "Spring1"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2016-07-11") && polen_aqs_meteo_get$FECHA[i] <= ymd("2016-07-18"))
    polen_aqs_meteo_get$campaign[i] = "Summer2"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2016-11-14") && polen_aqs_meteo_get$FECHA[i] <= ymd("2016-11-21"))
    polen_aqs_meteo_get$campaign[i] = "Fall2"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2017-02-06") && polen_aqs_meteo_get$FECHA[i] <= ymd("2017-02-13"))
    polen_aqs_meteo_get$campaign[i] = "Winter2"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2017-04-28") && polen_aqs_meteo_get$FECHA[i] <= ymd("2017-05-05"))
    polen_aqs_meteo_get$campaign[i] = "Spring2"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2015-03-02") && polen_aqs_meteo_get$FECHA[i] <= ymd("2015-03-09"))
    polen_aqs_meteo_get$campaign[i] = "mar2015"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2015-04-21") && polen_aqs_meteo_get$FECHA[i] <= ymd("2015-04-28"))
    polen_aqs_meteo_get$campaign[i] = "abr2015"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2017-05-22") && polen_aqs_meteo_get$FECHA[i] <= ymd("2017-05-28"))
    polen_aqs_meteo_get$campaign[i] = "may2017"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2017-03-14") && polen_aqs_meteo_get$FECHA[i] <= ymd("2017-03-21"))
    polen_aqs_meteo_get$campaign[i] = "mar2017"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2016-06-22") && polen_aqs_meteo_get$FECHA[i] <= ymd("2016-06-29"))
    polen_aqs_meteo_get$campaign[i] = "jun2016"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2015-12-02") && polen_aqs_meteo_get$FECHA[i] <= ymd("2015-12-08"))
    polen_aqs_meteo_get$campaign[i] = "dic2015-1"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2015-12-09") && polen_aqs_meteo_get$FECHA[i] <= ymd("2015-12-15"))
    polen_aqs_meteo_get$campaign[i] = "dic2015-2"
  else if (polen_aqs_meteo_get$FECHA[i] >= ymd("2015-12-16") && polen_aqs_meteo_get$FECHA[i] <= ymd("2015-12-23"))
    polen_aqs_meteo_get$campaign[i] = "dic2015-3"
}

p <- humedad[humedad$NOMBRE=="MADRID,RETIRO" | humedad$NOMBRE=="MADRID, RETIRO",]


polen_aqs_meteo_get <- sqldf("select a.*, b.RH
                              from polen_aqs_meteo_get As a
                              left join p As b
                              on a.FECHA = b.FECHA
                             ")

write.csv(polen_aqs_meteo_get, "polen_aqs_meteo_get.csv")

polen_aqs_meteo_get <- polen_aqs_meteo_get[polen_aqs_meteo_get$campaign != 0, ]



get_complete <- merge(polen_aqs_meteo_get, bacterias_get, type = "full outer", by = "campaign")

get_complete <- merge(get_complete, hongos_get, type = "full outer", by = "campaign")

get_complete <- get_complete[order(get_complete$campaign, get_complete$FECHA), ]

write.csv(get_complete, "get_complete_class.csv")

get_complete <- read.csv("get_complete.csv")



#etsii

polen_aqs_etsii <- merge(polen_ret, aqs_ciu, by.x = "FECHA", by.y = "date")

polen_aqs_meteo_etsii <- merge(polen_aqs_etsii, meteo_ret, by.x = "FECHA", by.y = "fecha")
str(polen_aqs_meteo_etsii)
summary(polen_aqs_meteo_etsii)



polen_aqs_meteo_etsii$campaign <- 0

for (i in c(1:length(polen_aqs_meteo_etsii$FECHA))) {
  if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2015-11-23") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2015-11-30"))
    polen_aqs_meteo_etsii$campaign[i] = "Fall1"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2015-07-20") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2015-07-27"))
    polen_aqs_meteo_etsii$campaign[i] = "Summer1"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2016-02-26") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2016-03-04"))
    polen_aqs_meteo_etsii$campaign[i] = "Winter1"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2016-04-29") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2016-05-06"))
    polen_aqs_meteo_etsii$campaign[i] = "Spring1"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2016-07-11") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2016-07-18"))
    polen_aqs_meteo_etsii$campaign[i] = "Summer2"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2016-11-14") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2016-11-21"))
    polen_aqs_meteo_etsii$campaign[i] = "Fall2"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2017-02-06") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2017-02-13"))
    polen_aqs_meteo_etsii$campaign[i] = "Winter2"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2017-04-28") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2017-05-05"))
    polen_aqs_meteo_etsii$campaign[i] = "Spring2"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2015-03-02") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2015-03-09"))
    polen_aqs_meteo_etsii$campaign[i] = "mar2015"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2015-04-21") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2015-04-28"))
    polen_aqs_meteo_etsii$campaign[i] = "abr2015"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2017-05-22") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2017-05-28"))
    polen_aqs_meteo_etsii$campaign[i] = "may2017"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2017-03-14") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2017-03-21"))
    polen_aqs_meteo_etsii$campaign[i] = "mar2017"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2016-06-22") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2016-06-29"))
    polen_aqs_meteo_etsii$campaign[i] = "jun2016"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2015-12-02") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2015-12-08"))
    polen_aqs_meteo_etsii$campaign[i] = "dic2015-1"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2015-12-09") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2015-12-15"))
    polen_aqs_meteo_etsii$campaign[i] = "dic2015-2"
  else if (polen_aqs_meteo_etsii$FECHA[i] >= ymd("2015-12-16") && polen_aqs_meteo_etsii$FECHA[i] <= ymd("2015-12-23"))
    polen_aqs_meteo_etsii$campaign[i] = "dic2015-3"
}

p <- humedad[humedad$NOMBRE=="MADRID,RETIRO" | humedad$NOMBRE=="MADRID, RETIRO",]


polen_aqs_meteo_etsii <- sqldf("select a.*, b.RH
                              from polen_aqs_meteo_etsii As a
                              left join p As b
                              on a.FECHA = b.FECHA
                             ")

write.csv(polen_aqs_meteo_etsii, "polen_aqs_meteo_etsii.csv")

polen_aqs_meteo_etsii <- polen_aqs_meteo_etsii[polen_aqs_meteo_etsii$campaign != 0, ]



etsii_complete <- merge(polen_aqs_meteo_etsii, bacterias_etsii, type = "full outer", by = "campaign")

etsii_complete <- merge(etsii_complete, hongos_etsii, type = "full outer", by = "campaign")

etsii_complete <- etsii_complete[order(etsii_complete$campaign, etsii_complete$FECHA), ]

write.csv(etsii_complete, "etsii2_complete.csv")

etsii_complete <- read.csv("etsii_complete.csv")






















#polen EDA date limited to bacteria availability

ggplot(ciu_complete, aes(x=campaign, y=as.numeric(PLAT), color=campaign)) +
  geom_boxplot() +
  ylim(0,50)

#polen EDA whole ciu (date from 2015)

polen_aqs_meteo_ciu$CUPR <- as.numeric(polen_aqs_meteo_ciu$CUPR)
polen_aqs_meteo_ciu$OLEA <- as.numeric(polen_aqs_meteo_ciu$OLEA)
polen_aqs_meteo_ciu$PLAN <- as.numeric(polen_aqs_meteo_ciu$PLAN)
polen_aqs_meteo_ciu$PLAT <- as.numeric(polen_aqs_meteo_ciu$PLAN)
polen_aqs_meteo_ciu$POAC <- as.numeric(polen_aqs_meteo_ciu$POAC)

cupr_ps <- plot_ps(polen_aqs_meteo_ciu, year = 2015, pollen.type = "CUPR")

analyse_trend(polen_aqs_meteo_ciu[, c(1:6)], result = "plot")

pollen_calendar(polen_aqs_meteo_ciu[, c(1:6)])




#POLEN_AQS_METEO_CIUDAD_UNIVERSITARIA

data <- read.csv("polen_aqs_meteo_ciu.csv")
head(data)

data_polen <- data[, colnames(data) %in% c("FECHA","CUPR","OLEA","PLAN","PLAT","POAC","PTOTAL")]

data_polen_melt <- melt(data_polen, id.vars = "FECHA") %>% na.omit()

sum(is.na(data_polen_melt))

data_polen_melt$year <- year(data_polen_melt$FECHA)

ggplot(data_polen_melt[data_polen_melt$variable != 'PTOTAL', ], aes(x = FECHA, y = value, color = variable, group = variable)) +
  geom_line() +
  ylim(0, 1000)

ggplot(data = data_polen_melt[data_polen_melt$variable != 'PTOTAL', ], aes(x = variable, y = value, color = variable, group = variable, fill = variable))+
  geom_violin(trim=FALSE)+
  ylim(0, 10)+
  theme_bw()

#Use AeroBiology

data_polen <- data_polen[, !colnames(data_polen) %in% c("PTOTAL")]

str(data_polen)

data_polen$FECHA <- ymd(data_polen$FECHA)
data_polen$CUPR <- as.numeric(data_polen$CUPR)
data_polen$OLEA <- as.numeric(data_polen$OLEA)
data_polen$PLAN <- as.numeric(data_polen$PLAN)
data_polen$PLAT <- as.numeric(data_polen$PLAN)
data_polen$POAC <- as.numeric(data_polen$POAC)

plot_ps(data_polen, year = 2015, pollen.type = "PLAT")

analyse_trend(data_polen, result = "plot")

pollen_calendar(data_polen)


#modeling

str(data)


data$tmax <- as.character(data$tmax) %>% str_replace(",",".") %>% as.numeric()
data$tmed <- as.character(data$tmed) %>% str_replace(",",".") %>% as.numeric()
data$tmin <- as.character(data$tmin) %>% str_replace(",",".") %>% as.numeric()
data$velmedia <- as.character(data$velmedia) %>% str_replace(",",".") %>% as.numeric()

X <- data[, colnames(data) %in% c("FECHA","year","month","tmed","tmin","tmax","dir","velmedia","PTOTAL")]
X$FECHA <- ymd(X$FECHA)

#FE

X = na.omit(X)

suma <- 0
X$Tmed_acum <- 0
for (i in c(1:length(X$FECHA))){
  
  if (X$year[i+1] == X$year[i]){
    suma = suma + X$tmed[i]
    X$Tmed_acum[i] = suma
  }
  else {
    X$Tmed_acum[i] = suma
    suma = 0
  }
}
X$Tmed_acum[length(X$FECHA)] <- X$Tmed_acum[length(X$FECHA)-1] + X$Tmed_acum[length(X$FECHA)]

str(X)

X <- X[, !colnames(X) %in% "FECHA"]

target <- X$PTOTAL

X <- X[, !colnames(X) %in% "PTOTAL"]



set.seed(1)

index <- sample(length(X$year), round(length(X$year))*0.85)

Xtrain <- X[index, ]
Xtest <- X[-index, ]
ytrain <- target[index]
ytest <- target[-index]


dtrain <- xgb.DMatrix(data=as.matrix(Xtrain),label=ytrain, missing=NA)

dtest <- xgb.DMatrix(data=as.matrix(Xtest), missing=NA)

dtot <- xgb.DMatrix(data=as.matrix(X), missing=NA)

xgb <- xgb.train(#params = param
  data = dtrain
  , nrounds = 10
  , verbose = 0
  , print_every_n = 5
)


preds <- predict(xgb,dtest)
trues <- ytest

plot(preds, trues)
plot(preds, type= "l")
lines(trues, col = "red")

rmse <- sqrt(sum((preds-trues)^2)/length(preds))
cor(preds, trues)^2


results <- predict(xgb,dtot)
plot(target, type= "l")
lines(results, col = "red")


results <- cbind(X,results)
ggplot(aes(x = X$OLEA, y = preds)) +
  geom_line()

xgb.importance(colnames(X), model = xgb)
#RF
X_RF <- cbind(Xtrain,ytrain)
X_RF_test <- cbind(Xtest,ytest)

modelRF <- randomForest(ytrain~., data = X_RF)

preds <- predict(modelRF, X_RF_test)

cor(preds, trues)^2

plot(preds, type = "l")
lines(trues, col = "red")

preds <- predict(modelRF, X)

plot(preds, type = "l")
lines(target, col = "red")

##ANN

X <- cbind(X,target)

maxs <- apply(X, 2, max) 
mins <- apply(X, 2, min)

X <- X[, !colnames(X) %in% "PLAT"]

scaled <- as.data.frame(scale(X, center = mins, scale = maxs - mins))


train_ <- scaled[index,]
test_ <- scaled[-index,]

modelANN <- neuralnet(target~., hidden = c(10,5), data = train_)

preds <- neuralnet::compute(modelANN, Xtest)

pred <- preds$net.result*(max(target)-min(target))+min(target)
test.r<-(test_$target)*(max(target)-min(target))+min(target)

plot(test.r,pred)
abline(a=0,b=1,col="red")

cor(test.r,pred)^2

##scaled xgboost

dtrain <- xgb.DMatrix(data=as.matrix(train_[, -9]),label=train_$target, missing=NA)

dtest <- xgb.DMatrix(data=as.matrix(test_[, -9]), missing=NA)

dtot <- xgb.DMatrix(data=as.matrix(X), missing=NA)

xgb <- xgb.train(#params = param
  data = dtrain
  , nrounds = 5
  , verbose = 0
  , print_every_n = 5
)


preds <- predict(xgb,dtest)
trues <- ytest

plot(preds, trues)

rmse <- sqrt(sum((preds-trues)^2)/length(preds))
cor(preds, trues)^2
