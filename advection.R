rm(list=ls())

setwd('C:/Users/Jose María/Documents/AIRTEC/OBJETIVO_1/Advection')


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
library(cluster)
library(car)
library(mice)
library(randomForest)
library(factoextra)
library(forecast)
library(corrplot)
library(dunn.test)
library(clinfun)
library(vegan)
library(gam)
library(psych)
library(caret)
library(openair)


memory.limit(size=40000)


lista <- list.files(pattern = 'csv')

advection <- lapply(lista, read.csv)

advection <- data.frame(advection)
str(advection)

advection$FECHA <- dmy(advection$FECHA)

colnames(advection) <- c("FECHA","advection")

datos <- sqldf('select a.*, b.advection
               from datos As a
               left join advection As b
               on a.FECHA = b.FECHA')


datos_class <- sqldf('select a.*, b.advection
               from datos_class As a
               left join advection As b
               on a.FECHA = b.FECHA')

