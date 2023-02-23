---
  title: "Madrid's Air Quality Forecasting with ARIMA Forecasting"

author: "Glorious Christian"

Date : "August 1, 2018"

output:
  html_document:
  fig_width: 10
fig_height: 7
toc: yes
number_sections : yes
code_folding: show
---
  
  ```{r setup, include=FALSE, echo=FALSE}
knitr::opts_chunk$set(echo=TRUE, error=FALSE)
```

<center><img src="https://triathlonworld.com/files/styles/large/public/media/image/file/Madrid.jpg?itok=JHeLm4MW"></center>
  
  
  # Introduction
  
  I'm not a meteorologist so I can't make a good analysis for the air quality condition. But I will show the fact about Air Quality in Madrid using this dataset. Air pollution can cause both short term and long term effects on health and many people are concerned about pollution in the air that they breathe.

There are some interesting things in this dataset:
  
  * time series data with a length of 18 years.
* data recorded every 1 hour.
* there are 17 pollutants recorded, but I will use only 10 of it (Benzene, Carbon Monoxide, Ethylbenzene, Non-Methane Hydrocarbons, Nitrogen Dioxide, Ozone, particles smaller than 10 ??m, Sulphur Dioxide, total hydrocarbons, Toluene).


#Required R - Packages

There are several R packages that useful for analyzing this dataset.

* DT, *tool for creating attractive dataframe as output*.
* dplyr, *tool frome processing dataset*.
* ggplot2, *creating graphics*.
* RColorBrewer, *to help choose sensible colour schemes for figures in R*.
* tidyr, *create a tidy data*.
* leaflet, *libraries for make an interactive maps*
  * ggthemes, *make an interactive themes for chart*
  * forecast, *make a data forecast*
  * fmsb, *make a radar chart*
  
  ```{r, message = FALSE, echo = FALSE} 
#load package
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(leaflet)
library(DT)
library(ggthemes)
library(forecast)
library(fmsb)
```


#Dataset
This dataset defines stations as the higher hierarchical level: each individual station history can be individually extracted from the file for further study. Inside each station's DataFrame, all the particles measurements that such station has registered in the period of 2001/01 - 2018/04 (if active this whole time). Not every station has the same equipment, therefore each station can measure only a certain subset of particles.

##Input data
There are 19 separate data as input. 18 csv data for each year (2001 - 2018) and 1 xls data for stations. All I need to do is binding the data and joining the stations dataset.

```{r, message = FALSE, echo = FALSE} 
stations <- read.csv("../input/stations.csv")

a2001 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2001.csv")
a2002 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2002.csv") 
a2003 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2003.csv")
a2004 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2004.csv")
a2005 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2005.csv")
a2006 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2006.csv")
a2007 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2007.csv")
a2008 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2008.csv")
a2009 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2009.csv")
a2010 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2010.csv")
a2011 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2011.csv")
a2012 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2012.csv")
a2013 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2013.csv")
a2014 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2014.csv")
a2015 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2015.csv")
a2016 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2016.csv")
a2017 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2017.csv")
a2018 <- read.csv("../input/csvs_per_year/csvs_per_year/madrid_2018.csv")
```

##Change date format and selecting the 10 variables (pollutants)
Since the date format is in **factor type**, I change those type into Posixct and selecting only 10 pollutants to analyze.

```{r, message = FALSE, echo = FALSE} 
a2001$date <- as.POSIXct(a2001$date, format = "%Y-%m-%d %H:%M:%S")
a2001$Date <- format(a2001$date, "%Y-%m-%d")
a2001$Time <- format(a2001$date, "%T")
a2001$Date <- as.POSIXct(a2001$Date, format = "%Y-%m-%d")

a2001$day <- format(a2001$date, "%d")
a2001$month <- format(a2001$date, "%m")
a2001$year <- format(a2001$date, "%Y")
a2001$hour <- format(a2001$date, "%H")

madrid_2001 <- a2001 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2002$date <- as.POSIXct(a2002$date, format = "%Y-%m-%d %H:%M:%S")
a2002$Date <- format(a2002$date, "%Y-%m-%d")
a2002$Time <- format(a2002$date, "%T")
a2002$Date <- as.POSIXct(a2002$Date, format = "%Y-%m-%d")

a2002$day <- format(a2002$date, "%d")
a2002$month <- format(a2002$date, "%m")
a2002$year <- format(a2002$date, "%Y")
a2002$hour <- format(a2002$date, "%H")

madrid_2002 <- a2002 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2003$date <- as.POSIXct(a2003$date, format = "%Y-%m-%d %H:%M:%S")
a2003$Date <- format(a2003$date, "%Y-%m-%d")
a2003$Time <- format(a2003$date, "%T")
a2003$Date <- as.POSIXct(a2003$Date, format = "%Y-%m-%d")

a2003$day <- format(a2003$date, "%d")
a2003$month <- format(a2003$date, "%m")
a2003$year <- format(a2003$date, "%Y")
a2003$hour <- format(a2003$date, "%H")

madrid_2003 <- a2003 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2004$date <- as.POSIXct(a2004$date, format = "%Y-%m-%d %H:%M:%S")
a2004$Date <- format(a2004$date, "%Y-%m-%d")
a2004$Time <- format(a2004$date, "%T")
a2004$Date <- as.POSIXct(a2004$Date, format = "%Y-%m-%d")

a2004$day <- format(a2004$date, "%d")
a2004$month <- format(a2004$date, "%m")
a2004$year <- format(a2004$date, "%Y")
a2004$hour <- format(a2004$date, "%H")

madrid_2004 <- a2004 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2005$date <- as.POSIXct(a2005$date, format = "%Y-%m-%d %H:%M:%S")
a2005$Date <- format(a2005$date, "%Y-%m-%d")
a2005$Time <- format(a2005$date, "%T")
a2005$Date <- as.POSIXct(a2005$Date, format = "%Y-%m-%d")

a2005$day <- format(a2005$date, "%d")
a2005$month <- format(a2005$date, "%m")
a2005$year <- format(a2005$date, "%Y")
a2005$hour <- format(a2005$date, "%H")

madrid_2005 <- a2005 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2006$date <- as.POSIXct(a2006$date, format = "%Y-%m-%d %H:%M:%S")
a2006$Date <- format(a2006$date, "%Y-%m-%d")
a2006$Time <- format(a2006$date, "%T")
a2006$Date <- as.POSIXct(a2006$Date, format = "%Y-%m-%d")

a2006$day <- format(a2006$date, "%d")
a2006$month <- format(a2006$date, "%m")
a2006$year <- format(a2006$date, "%Y")
a2006$hour <- format(a2006$date, "%H")

madrid_2006 <- a2006 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2007$date <- as.POSIXct(a2007$date, format = "%Y-%m-%d %H:%M:%S")
a2007$Date <- format(a2007$date, "%Y-%m-%d")
a2007$Time <- format(a2007$date, "%T")
a2007$Date <- as.POSIXct(a2007$Date, format = "%Y-%m-%d")

a2007$day <- format(a2007$date, "%d")
a2007$month <- format(a2007$date, "%m")
a2007$year <- format(a2007$date, "%Y")
a2007$hour <- format(a2007$date, "%H")

madrid_2007 <- a2007 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2008$date <- as.POSIXct(a2008$date, format = "%Y-%m-%d %H:%M:%S")
a2008$Date <- format(a2008$date, "%Y-%m-%d")
a2008$Time <- format(a2008$date, "%T")
a2008$Date <- as.POSIXct(a2008$Date, format = "%Y-%m-%d")

a2008$day <- format(a2008$date, "%d")
a2008$month <- format(a2008$date, "%m")
a2008$year <- format(a2008$date, "%Y")
a2008$hour <- format(a2008$date, "%H")

madrid_2008 <- a2008 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2009$date <- as.POSIXct(a2009$date, format = "%Y-%m-%d %H:%M:%S")
a2009$Date <- format(a2009$date, "%Y-%m-%d")
a2009$Time <- format(a2009$date, "%T")
a2009$Date <- as.POSIXct(a2009$Date, format = "%Y-%m-%d")

a2009$day <- format(a2009$date, "%d")
a2009$month <- format(a2009$date, "%m")
a2009$year <- format(a2009$date, "%Y")
a2009$hour <- format(a2009$date, "%H")

madrid_2009 <- a2009 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2010$date <- as.POSIXct(a2010$date, format = "%Y-%m-%d %H:%M:%S")
a2010$Date <- format(a2010$date, "%Y-%m-%d")
a2010$Time <- format(a2010$date, "%T")
a2010$Date <- as.POSIXct(a2010$Date, format = "%Y-%m-%d")

a2010$day <- format(a2010$date, "%d")
a2010$month <- format(a2010$date, "%m")
a2010$year <- format(a2010$date, "%Y")
a2010$hour <- format(a2010$date, "%H")

madrid_2010 <- a2010 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2011$date <- as.POSIXct(a2011$date, format = "%Y-%m-%d %H:%M:%S")
a2011$Date <- format(a2011$date, "%Y-%m-%d")
a2011$Time <- format(a2011$date, "%T")
a2011$Date <- as.POSIXct(a2011$Date, format = "%Y-%m-%d")

a2011$day <- format(a2011$date, "%d")
a2011$month <- format(a2011$date, "%m")
a2011$year <- format(a2011$date, "%Y")
a2011$hour <- format(a2011$date, "%H")

madrid_2011 <- a2011 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2012$date <- as.POSIXct(a2012$date, format = "%Y-%m-%d %H:%M:%S")
a2012$Date <- format(a2012$date, "%Y-%m-%d")
a2012$Time <- format(a2012$date, "%T")
a2012$Date <- as.POSIXct(a2012$Date, format = "%Y-%m-%d")

a2012$day <- format(a2012$date, "%d")
a2012$month <- format(a2012$date, "%m")
a2012$year <- format(a2012$date, "%Y")
a2012$hour <- format(a2012$date, "%H")

madrid_2012 <- a2012 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2013$date <- as.POSIXct(a2013$date, format = "%Y-%m-%d %H:%M:%S")
a2013$Date <- format(a2013$date, "%Y-%m-%d")
a2013$Time <- format(a2013$date, "%T")
a2013$Date <- as.POSIXct(a2013$Date, format = "%Y-%m-%d")

a2013$day <- format(a2013$date, "%d")
a2013$month <- format(a2013$date, "%m")
a2013$year <- format(a2013$date, "%Y")
a2013$hour <- format(a2013$date, "%H")

madrid_2013 <- a2013 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2014$date <- as.POSIXct(a2014$date, format = "%Y-%m-%d %H:%M:%S")
a2014$Date <- format(a2014$date, "%Y-%m-%d")
a2014$Time <- format(a2014$date, "%T")
a2014$Date <- as.POSIXct(a2014$Date, format = "%Y-%m-%d")

a2014$day <- format(a2014$date, "%d")
a2014$month <- format(a2014$date, "%m")
a2014$year <- format(a2014$date, "%Y")
a2014$hour <- format(a2014$date, "%H")

madrid_2014 <- a2014 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2015$date <- as.POSIXct(a2015$date, format = "%Y-%m-%d %H:%M:%S")
a2015$Date <- format(a2015$date, "%Y-%m-%d")
a2015$Time <- format(a2015$date, "%T")
a2015$Date <- as.POSIXct(a2015$Date, format = "%Y-%m-%d")

a2015$day <- format(a2015$date, "%d")
a2015$month <- format(a2015$date, "%m")
a2015$year <- format(a2015$date, "%Y")
a2015$hour <- format(a2015$date, "%H")

madrid_2015 <- a2015 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2016$date <- as.POSIXct(a2016$date, format = "%Y-%m-%d %H:%M:%S")
a2016$Date <- format(a2016$date, "%Y-%m-%d")
a2016$Time <- format(a2016$date, "%T")
a2016$Date <- as.POSIXct(a2016$Date, format = "%Y-%m-%d")

a2016$day <- format(a2016$date, "%d")
a2016$month <- format(a2016$date, "%m")
a2016$year <- format(a2016$date, "%Y")
a2016$hour <- format(a2016$date, "%H")

madrid_2016 <- a2016 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2017$date <- as.POSIXct(a2017$date, format = "%Y-%m-%d %H:%M:%S")
a2017$Date <- format(a2017$date, "%Y-%m-%d")
a2017$Time <- format(a2017$date, "%T")
a2017$Date <- as.POSIXct(a2017$Date, format = "%Y-%m-%d")

a2017$day <- format(a2017$date, "%d")
a2017$month <- format(a2017$date, "%m")
a2017$year <- format(a2017$date, "%Y")
a2017$hour <- format(a2017$date, "%H")

madrid_2017 <- a2017 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)


a2018$date <- as.POSIXct(a2018$date, format = "%Y-%m-%d %H:%M:%S")
a2018$Date <- format(a2018$date, "%Y-%m-%d")
a2018$Time <- format(a2018$date, "%T")
a2018$Date <- as.POSIXct(a2018$Date, format = "%Y-%m-%d")

a2018$day <- format(a2018$date, "%d")
a2018$month <- format(a2018$date, "%m")
a2018$year <- format(a2018$date, "%Y")
a2018$hour <- format(a2018$date, "%H")

madrid_2018 <- a2018 %>%
select(date, Date, Time, day, month, year, hour, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL)
```


##Binding data and show the dataset

```{r, message = FALSE, echo = FALSE} 
madrid <- madrid_2001 %>%
rbind(madrid_2002) %>%
rbind(madrid_2003) %>%
rbind(madrid_2004) %>%
rbind(madrid_2005) %>%
rbind(madrid_2006) %>%
rbind(madrid_2007) %>%
rbind(madrid_2008) %>%
rbind(madrid_2009) %>%
rbind(madrid_2010) %>%
rbind(madrid_2011) %>%
rbind(madrid_2012) %>%
rbind(madrid_2013) %>%
rbind(madrid_2014) %>%
rbind(madrid_2015) %>%
rbind(madrid_2016) %>%
rbind(madrid_2017) %>%
rbind(madrid_2018) 

radarstations <- stations %>%
select(id, name)

madrid <-  right_join(madrid, radarstations, by = c("station" = "id"))
```

```{r, message = FALSE, echo = FALSE} 
datatable(head(madrid, 100), class = 'cell-border stripe', 
colnames = c('Full Date', 'Date', 'Time', 'Day', 'Month', 'Year', 'Hour',
'Station', 'BEN', 'CO', 'EBE', 'NMHC', 'NO_2', 'O_3', 'PM10', 
'SO_2', 'TCH', 'TOL', 'Station Name'))
```

##Air condition in each stations{.tabset .tabset-fade .tabset-pills}
I compare the air condition in each stations with average condition in Madrid. I use radarchart to visualize. It shows that Nitrogen Dioxide is the largest contributor for air pollution in Madrid.

```{r, message = FALSE, echo = FALSE} 
makeColors <- function(myteam){
colClasses<-data.frame(col2rgb(c('gray50','steelblue'))/255)
colnames(colClasses) <- c('mean all teams', myteam)
colClasses <- data.frame(rbind(colClasses,'alpha'=c(1,1,1)))
colors_border=c(
rgb(colClasses[1,1],colClasses[2,1],colClasses[3,1],colClasses[4,1]),
rgb(colClasses[1,2],colClasses[2,2],colClasses[3,2],colClasses[4,2]))
colors_border
}

makeRadar <-function(myteam){
meanAll <- data.frame(madrid %>% 
select_if(is.numeric) %>% 
select(-station) %>%
summarise_all(funs(mean(., na.rm = TRUE))) %>% 
mutate_all(round, 2))

max <- data.frame(madrid %>% 
select_if(is.numeric) %>% 
select(-station) %>%
summarise_all(funs(max(., na.rm = TRUE))) %>% 
mutate_all(round, 2)) %>% 
as.vector

temp <- data.frame(madrid %>% 
filter(station == myteam) %>% 
select_if(is.numeric) %>%
select(-station) %>%
summarise_all(funs(mean(., na.rm = TRUE))) %>% 
mutate_all(round, 2))

min <-data.frame(madrid %>% 
select_if(is.numeric) %>%
select(-station) %>%
summarise_all(funs(min(., na.rm = TRUE))) %>% 
mutate_all(round, 2)) %>% 
as.vector

tempRadar<-data.frame(rbind(max, min, meanAll, temp))

rownames(tempRadar) <-NULL

rownames(tempRadar) = make.names(c("","", "mean",myteam), unique=TRUE)

mycols <- makeColors(myteam)

radarchart(tempRadar,pcol=mycols ,
axistype=2 , 
plwd=4 , 
plty=1,
cglcol="grey", 
cglty=2, 
axislabcol="black", 
caxislabels=seq(0,20,5), 
cglwd=1,
vlcex=.8,palcex=1,
title=paste0(toupper(myteam)))
legend(x=.75, y=1.25, legend = rownames(tempRadar[-c(1,2),]), bty = "n", 
pch=20 , col= mycols , text.col = "black", cex=1.0, pt.cex=1)
}
```

### Station 1: Pza. de EspÃna

Station number : 28079004
Station name : Pza. de EspÃna
Station address: Plaza de EspÃna
Longitude : -3.712247
Latitude : 40.42385
Elevation : 635

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079004)
```

### Station 2: Escuelas Aguirre

Station number : 28079008
Station name : Escuelas Aguirre
Station address: Entre C/ AlcalÃ¡ y C/ Oâ???T Donell
Longitude : -3.682319
Latitude : 40.42156
Elevation : 670

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079008)
```

### Station 3: Avda. RamÃ³n y Cajal

Station number : 28079011
Station name : Avda. RamÃ³n y Cajal
Station address: Avda. RamÃ³n y Cajal esq. C/ PrÃncipe de Vergara
Longitude : -3.677356
Latitude : 40.45148
Elevation : 708

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079011)
```

### Station 4: Arturo Soria

Station number : 28079016
Station name : Arturo Soria
Station address: C/ Arturo Soria esq. C/ Vizconde de los Asilos
Longitude : -3.639233
Latitude : 40.44005
Elevation : 693

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079016)
```

### Station 5: Villaverde

Station number : 28079017
Station name : Villaverde
Station address: C/. Juan PeÃ±alver
Longitude : -3.713322
Latitude : 40.34714
Elevation : 604

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079017)
```

### Station 6: Farolillo

Station number : 28079018
Station name : Farolillo
Station address: Calle Farolillo - C/Ervigio
Longitude : -3.731853
Latitude : 40.39478
Elevation : 630

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079018)
```

### Station 7: Casa de Campo

Station number : 28079024
Station name : Casa de Campo
Station address: Casa de Campo (Terminal del TelefÃ©rico)
Longitude : -3.747347
Latitude : 40.41936
Elevation : 642

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079024)
```

### Station 8: Barajas Pueblo

Station number : 28079027
Station name : Barajas Pueblo
Station address: C/. JÃºpiter, 21 (Barajas)
Longitude : -3.580031
Latitude : 40.47693
Elevation : 621

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079027)
```

### Station 9: Pza. del Carmen

Station number : 28079035
Station name : Pza. del Carmen
Station address: Plaza del Carmen esq. Tres Cruces.
Longitude : -3.703172
Latitude : 40.41921
Elevation : 659

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079035)
```

### Station 10: Moratalaz

Station number : 28079036
Station name : Moratalaz
Station address: Avd. Moratalaz esq. Camino de los Vinateros
Longitude : -3.645306
Latitude : 40.40795
Elevation : 685

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079036)
```

### Station 11: Cuatro Caminos

Station number : 28079038
Station name : Cuatro Caminos
Station address: Avda. Pablo Iglesias esq. C/ MarquÃ©s de Lema
Longitude : -3.707128
Latitude : 40.44554
Elevation : 698

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079038)
```

### Station 12: Barrio del Pilar

Station number : 28079039
Station name : Barrio del Pilar
Station address: Avd. Betanzos esq. C/ Monforte de Lemos
Longitude : -3.711542
Latitude : 40.47823
Elevation : 674

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079039)
```

### Station 13: Vallecas

Station number : 28079040
Station name : Vallecas
Station address: C/ Arroyo del Olivar esq. C/ RÃo Grande.
Longitude : -3.651522
Latitude : 40.38815
Elevation : 677

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079040)
```

### Station 14: Mendez Alvaro

Station number : 28079047
Station name : Mendez Alvaro
Station address: C/ Juan de Mariana / Pza. Amanecer Mendez Alvaro
Longitude : -3.686825
Latitude : 40.39811
Elevation : 599

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079047)
```

### Station 15: Castellana

Station number : 28079048
Station name : Castellana
Station address: C/ Jose Gutierrez Abascal
Longitude : -3.690367
Latitude : 40.43990
Elevation : 676

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079048)
```

### Station 16: Parque del Retiro

Station number : 28079049
Station name : Parque del Retiro
Station address: Paseo Venezuela- Casa de Vacas
Longitude : -3.682583
Latitude : 40.41444
Elevation : 662

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079049)
```

### Station 17: Plaza Castilla

Station number : 28079050
Station name : Plaza Castilla
Station address: Plaza Castilla (Canal)
Longitude : -3.688769
Latitude : 40.46557
Elevation : 728

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079050)
```

### Station 18: Ensanche de Vallecas

Station number : 28079054
Station name : Ensanche de Vallecas
Station address: Avda La Gavia / Avda. Las Suertes
Longitude : -3.612117
Latitude : 40.37293
Elevation : 627

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079054)
```

### Station 19: Urb. Embajada

Station number : 28079055
Station name : Urb. Embajada
Station address: C/ RiaÃ±o (Barajas)
Longitude : -3.580747
Latitude : 40.46253
Elevation : 618

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079055)
```

### Station 20: Pza. FernÃndez Ladreda

Station number : 28079056
Station name : Pza. FernÃndez Ladreda
Station address: Pza. FernÃndez Ladreda - Avda. Oporto
Longitude : -3.718728
Latitude : 40.38496
Elevation : 604

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079056)
```

### Station 21: Sanchinarro

Station number : 28079057
Station name : Sanchinarro
Station address: C/ Princesa de Eboli esq C/ Maria Tudor
Longitude : -3.660503
Latitude : 40.49421
Elevation : 700

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079057)
```

### Station 22: El Pardo

Station number : 28079058
Station name : El Pardo
Station address: Avda. La Guardia
Longitude : -3.774611
Latitude : 40.51806
Elevation : 615

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079058)
```

### Station 23: Juan Carlos I

Station number : 28079059
Station name : Juan Carlos I
Station address: Parque Juan Carlos I (frente oficinas mantenimiento)
Longitude : -3.609072
Latitude : 40.46525
Elevation : 660

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079059)
```

### Station 24: Tres Olivos

Station number : 28079060
Station name : Tres Olivos
Station address: Plaza Tres Olivos
Longitude : -3.689761
Latitude : 40.50059
Elevation : 715

```{r, message = FALSE, echo = FALSE} 
makeRadar(28079060)
```


#Station Map
This is the location of Madrid's Air Quality Station

```{r, message = FALSE, echo = FALSE} 
#Make a stations map
stations_map <- leaflet(stations) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=stations$lon, lat=stations$lat, popup=stations$name)

stations_map  
```


#Daily Air Quality in Madrid

I sum up 10 polutants (mentioned before) as a total polutions in Madrid.

```{r, message = FALSE, echo = FALSE} 
madrid_mean_daily <- madrid %>%
  group_by(Date) %>%
  summarise(BEN_mean = mean(BEN, na.rm = TRUE),
            CO_mean = mean(CO, na.rm = TRUE),
            EBE_mean = mean(EBE, na.rm = TRUE),
            NMHC_mean = mean(NMHC, na.rm = TRUE),
            NO_2_mean = mean(NO_2, na.rm = TRUE),
            O_3_mean = mean(O_3, na.rm = TRUE),
            PM10_mean = mean(PM10, na.rm = TRUE),
            SO_2_mean = mean(SO_2, na.rm = TRUE),
            TCH_mean = mean(TCH, na.rm = TRUE),
            TOL_mean = mean(TOL, na.rm = TRUE))

madrid_mean_daily <- mutate(madrid_mean_daily, tot_pol = rowSums(madrid_mean_daily[,-1], na.rm = TRUE))

ggplot(madrid_mean_daily, aes(x = as.Date(Date), y = tot_pol)) + geom_line(color = 'blue') +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "gray94", colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.background = element_rect(fill = "gray94"),
        plot.background = element_rect(fill = "gray94"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),  
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black')) +
  labs(x = '', y = 'Total Polution (??g/m³)', title='Air Quality in Madrid', subtitle='BEN, CO, EBE, NMHC, NO2, O3, PM10, SO2, TCH, TOL') +
  theme(axis.text.y =element_text(angle = 30),axis.text.x=element_text(size=11)) +
  scale_x_date(breaks = seq(as.Date("2001-01-01"), as.Date("2018-07-01"), by="12 months"), date_labels = "%Y")
```


#Monthly Moving Average of Madrid's Air Quality

I sum up 10 polutants (mentioned before) as a total polutions in Madrid and make an average each month.

```{r, message = FALSE, echo = FALSE} 
madrid_mean_monthly <- madrid %>%
  group_by(year, month) %>%
  summarise(BEN_mean = mean(BEN, na.rm = TRUE),
            CO_mean = mean(CO, na.rm = TRUE),
            EBE_mean = mean(EBE, na.rm = TRUE),
            NMHC_mean = mean(NMHC, na.rm = TRUE),
            NO_2_mean = mean(NO_2, na.rm = TRUE),
            O_3_mean = mean(O_3, na.rm = TRUE),
            PM10_mean = mean(PM10, na.rm = TRUE),
            SO_2_mean = mean(SO_2, na.rm = TRUE),
            TCH_mean = mean(TCH, na.rm = TRUE),
            TOL_mean = mean(TOL, na.rm = TRUE)) %>%
  mutate(time = paste(year, "-", month, "- 01"))

madrid_mean_monthly$tot <- rowSums(madrid_mean_monthly[,3:12], na.rm = TRUE)

madrid_mean_monthly$time <- as.Date(madrid_mean_monthly$time, format = "%Y - %m - %d")

ggplot(madrid_mean_monthly, aes(x = time, y = tot)) + geom_line(color = 'red') + 
  geom_point(aes(x = time, y = tot), size = 0.5, color = 'blue') +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        panel.background = element_rect(fill = "gray94", colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.background = element_rect(fill = "gray94"),
        plot.background = element_rect(fill = "gray94"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),  
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black')) +
  labs(x = '', y = 'Total Polution (??g/m³)', title='Air Quality in Madrid', subtitle='BEN, CO, EBE, NMHC, NO2, O3, PM10, SO2, TCH, TOL') +
  theme(axis.text.y =element_text(angle = 30),axis.text.x=element_text(size=11)) +
  scale_x_date(breaks = seq(as.Date("2001-01-01"), as.Date("2018-07-01"), by="6 months"), date_labels = "%b-%y") + 
  scale_y_continuous(breaks = seq(0,250,50), lim = c(0,250))
```


#Yearly Moving Average of Madrid's Air Quality

I sum up 10 polutants (mentioned before) as a total polutions in Madrid and make an average each year.

```{r, message = FALSE, echo = FALSE} 
madrid_mean_yearly  <- madrid %>%
  select(year, station, BEN, CO, EBE, NMHC, NO_2, O_3, PM10, SO_2, TCH, TOL) %>%
  group_by(year) %>%
  summarise(BEN_mean = mean(BEN, na.rm = TRUE),
            CO_mean = mean(CO, na.rm = TRUE),
            EBE_mean = mean(EBE, na.rm = TRUE),
            NMHC_mean = mean(NMHC, na.rm = TRUE),
            NO_2_mean = mean(NO_2, na.rm = TRUE),
            O_3_mean = mean(O_3, na.rm = TRUE),
            PM10_mean = mean(PM10, na.rm = TRUE),
            SO_2_mean = mean(SO_2, na.rm = TRUE),
            TCH_mean = mean(TCH, na.rm = TRUE),
            TOL_mean = mean(TOL, na.rm = TRUE))

madrid_mean_yearly$tots <- rowSums(madrid_mean_yearly[,2:11], na.rm = TRUE)

ggplot(madrid_mean_yearly, aes(x = year, y = tots, group = 1)) + geom_line(color = 'deeppink') + 
  geom_col(aes(x = year, y = tots), size = 2, fill = 'blue', alpha = 0.2) +
  geom_point(aes(x = year, y = tots), size = 2, color = 'darkmagenta') +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "gray94", colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.background = element_rect(fill = "gray94"),
        plot.background = element_rect(fill = "gray94"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),  
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black')) +
  labs(x = 'Year', y = 'Total Polution (??g/m³)', title='Air Quality in Madrid', subtitle='BEN, CO, EBE, NMHC, NO2, O3, PM10, SO2, TCH, TOL') +
  theme(axis.text.y =element_text(angle = 30),axis.text.x=element_text(size=11)) +
  scale_y_continuous(breaks = seq(0,250,50), lim = c(0,250))
```


#Investigate the Increasing Behavior of Air Quality

The idea is averaging polutions each month in every year. From this condition, we will know in which month the polutions increase.

```{r, message = FALSE, echo = FALSE} 
bulan <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

madrid_mean_monthly %>%
  group_by(month) %>%
  summarise(huy=mean(tot)) %>%
  ggplot(aes(x=month, y= huy, group=1)) + 
  geom_point(aes(x = month, y = huy), size = 2, color = 'darkmagenta') + 
  geom_line(size = 3, alpha = 0.5, color = 'blue') +
  scale_x_discrete(labels=bulan) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "gray94", colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.background = element_rect(fill = "gray94"),
        plot.background = element_rect(fill = "gray94"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),  
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black')) +
  labs(x = 'Month', y = 'Total Polution (??g/m³)', title='Monthly Average of Air Quality in Madrid', subtitle='BEN, CO, EBE, NMHC, NO2, O3, PM10, SO2, TCH, TOL') +
  theme(axis.text.y =element_text(angle = 30),axis.text.x=element_text(size=11)) 
```

From these graph, we know that air quality is getting worse in summer (June, July, and August). Maybe there are a lot of outdoor activity in Madrid in this season.


#Forecast the Air Quality in next 2 years (2020)

I using ARIMA method to find prediction for next 2 years. This is the ARIMA coefficients used for forecasting:
  
  ```{r, message = FALSE, echo = FALSE} 
monthly_ts <- ts(madrid_mean_monthly[,14],start=c(2001,1), end=c(2018,5), frequency = 12)
fit_ts <- auto.arima(monthly_ts)
fit_ts
```

```{r, message = FALSE, echo = FALSE} 
fit_ts %>%
  forecast(h=24) %>%
  autoplot() + 
  theme(axis.text.x = element_text(angle = 0),
        panel.background = element_rect(fill = "gray94", colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.background = element_rect(fill = "gray94"),
        plot.background = element_rect(fill = "gray94"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),  
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black')) +
  labs(x = '', y = 'Total Polution (??g/m³)', title='Forecasting of Air Quality using ARIMA', subtitle='BEN, CO, EBE, NMHC, NO2, O3, PM10, SO2, TCH, TOL') +
  scale_y_continuous(breaks = seq(0,250,50), lim = c(0,250))
```

From this forecasting, we know that in next two years the total polution in Madrid will about 110 ??g/m³. Not much different from the previous 2 years.