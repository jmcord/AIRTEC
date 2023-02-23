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
library(RColorBrewer)
library(leaflet)
library(DT)
library(ggthemes)
library(forecast)
library(fmsb)


PATH_POLEN <- "C:/Users/jcord/Documents/AIRTEC/OBJETIVO_1/Calidad_aire/"
setwd(PATH_POLEN)

files <- list.files()
files

aqs <- read.csv("daily_poll_MD.csv", stringsAsFactors = FALSE)
str(aqs)

aqs$date <- mdy(aqs$date)


estaciones <- read.csv("stations_long.csv", stringsAsFactors = FALSE) # %>% na.omit()

aqs <- merge(x = aqs, y = estaciones, type = "left", by.x = "AQISD", by.y = "AQISD")

aqs2 <- sqldf('select a.*, b.avg As PM25
              from pm10 As a
              left join pm25 As b
              on a.AQISD = b.AQISD and a.date = b.date')

ggplot(aqs2) +
  geom_line(aes(x=date, y=avg), col = 'blue') +
  geom_line(aes(x=date, y=PM25), col = 'black') +
  theme_bw()


# 
# aqs <- sqldf("select a.*, b.*
#              from aqs As a
#              left join estaciones As b
#              where b.AQISD = a.AQISD")

#lista de DF por contaminante

lista_poll <- list() 
i=1
  for (poll in unique(aqs$poll)) {
    lista_poll[[i]] <- aqs[aqs$poll == poll,]
    i = i+1
  }

names(lista_poll) <- unique(aqs$poll)

#lista de DF por estaciones


lista_est <- list() 
i=1
for (estacion in unique(aqs$AQISD)) {
  lista_est[[i]] <- aqs[aqs$AQISD == estacion,]
  i = i+1
}

names(lista_est) <- unique(aqs$AQISD)

#
lista_etc_año <- lapply(lista_est, function(x) {
  x = x[x$year >= 2000,]
  x = x[x$year <= 2007,]
})

aqs <- lista_poll[[1]]

estaciones <- unique(aqs$AQISD)



aqs <- aqs[aqs$AQISD == as.character(estaciones[1]),]

join <- sqldf("select a.*, d.*
        from aqs As a
          left join data As d
          on d.FECHA = a.date")




#Kaggle

PATH_POLEN <- "C:/Users/Jose María/Documents/AIRTEC/OBJETIVO_1/Calidad_aire/csvs_per_year"
setwd(PATH_POLEN)

files <- list.files()
files

aqs <- lapply(files, function(x) read.csv(x))
str(aqs)


aqs_binded <- bind_rows(aqs_binded)
aqs_binded$date <- ymd_hms(aqs_binded$date)
aqs_binded$station <- factor(aqs_binded$station)

str(aqs_binded)

una_estacion <- aqs_binded[aqs_binded$station == "28079001",]
plot(una_estacion$NO_2, type = "l")
