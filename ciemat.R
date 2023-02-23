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


PATH_CIEMAT <- "C:/Users/jcord/Documents/AIRTEC/OBJETIVO_1/Particulas"
setwd(PATH_CIEMAT)

files <- list.files(pattern = "2004-2011.csv")
files

datos_NO3 <- read.csv(files, sep = ';')

datos_NO3$date <- dmy_hm(datos_NO3$Fecha.hora)

datos_NO3$day <- day(datos_NO3$date)
datos_NO3$month <- month(datos_NO3$date)
datos_NO3$year <- year(datos_NO3$date)

datos_NO3$hour <- hour(datos_NO3$date)

colnames(datos_NO3) <- c("datetime","NO3","date","day","month","year","hour")

datos_NO3_hourly <- sqldf("select*, avg(a.NO3) As conc
                          from datos_NO3 As a
                          group by a.day, a.hour, a.month, a.year
                          order by a.date
                          ")

# datos_NO3_hourly <- aggregate(datos_NO3$NO3, by = list("day","hour","month","year"), FUN = "mean", data = datos_NO3)


files <- list.files(pattern = "2009-2011.csv")
files

datos_SO2 <- read.csv(files, sep = ';')

datos_SO2$date <- dmy_hm(datos_SO2$Fecha.hora)

datos_SO2$day <- day(datos_SO2$date)
datos_SO2$month <- month(datos_SO2$date)
datos_SO2$year <- year(datos_SO2$date)

datos_SO2$hour <- hour(datos_SO2$date)

colnames(datos_SO2) <- c("datetime","SO2","date","day","month","year","hour")

datos_SO2_hourly <- sqldf("select*, avg(a.SO2) As conc
                          from datos_SO2 As a
                          group by a.day, a.hour, a.month, a.year
                          order by a.date
                          ")


#Fractions SMPS

smps_2016 <- read.csv('SMPS_2016_AIRTEC.csv', sep = ';')


#Graphics for CCT AIRTEC 17_03_2021
library(openair)

datos_NO3_hourly_2010 <- datos_NO3_hourly[datos_NO3_hourly$year == 2010, ]
datos_SO2_hourly_2010 <- datos_SO2_hourly[datos_SO2_hourly$year == 2010, ]


timeVariation(datos_NO3_hourly_2010, pollutant = c('NO3')) 
timeVariation(datos_SO2_hourly_2010, pollutant = c('SO2'), color = 'orange') 

smps_2016_melt <- melt(smps_2016[1,])
smps_2016_melt <- smps_2016_melt[-108,]

smps_2016_melt$value <- smps_2016_melt$value/(sum(smps_2016_melt$value))*100

ggplot(data = smps_2016_melt) +
  geom_bar(aes(x=variable,y=value), fill = 'steelblue', stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylim(0,2.5) +
  ylab('fraction %') +
  xlab('fraction')
