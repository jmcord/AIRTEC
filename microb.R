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
library(reshape2)
library(readr)

PATH_POLEN <- "C:/Users/jcord/Documents/AIRTEC/OBJETIVO_1/Bacterias"
setwd(PATH_POLEN)

files <- list.files()
files

# fechas <- read.csv2("~/AIRTEC/OBJETIVO_1/Bacterias/Fecha.csv", header=TRUE)
# bacterias <- read.csv2("~/AIRTEC/OBJETIVO_1/Bacterias/Bacterias_AR.csv", header=TRUE)

fechas <- read.csv2("Fecha.csv", header=TRUE)
bacterias <- read_delim("Bacterias_AR_total2.csv", 
                                              ";", escape_double = FALSE, trim_ws = TRUE)

bacterias <- data.frame(bacterias)

colnames(bacterias)[101] <- "Family"

bacterias_long <- melt(bacterias, id.vars = c("Kingdom","Phylum","Class","Order","Family"))

bacterias_long <- separate(bacterias_long, col = "variable", into = c("Place", "campaign"), remove = TRUE)

bacterias_long <- merge(bacterias_long, fechas, type = "left", by.x = "campaign", by.y = "Fecha", all.x = TRUE)

bacterias_long$campaign <- factor(bacterias_long$campaign)

bacterias_long <- bacterias_long[colnames(bacterias_long) %in% c("campaign","Phylum","value", "Place")]

#Filo y Clase
# 
# bacterias_long <- bacterias_long[colnames(bacterias_long) %in% c("campaign","Phylum","Class","value", "Place")]

########################IMPORTANTE##############################
#Para utilizar clase, no se puede hacer como está ahora, creo que lo mejor sería juntar las columnas tipo caracter Phylum y Class, y rehacer de la
#misma manera


# bacterias_long <- aggregate(bacterias_long, by = list("Phylum", "campaign"), sum)

#Añadiendo Clase

# bacterias_long <- sqldf("select a.Phylum, a.Class, a.campaign, a.Place, sum(a.value) As value
#                         from bacterias_long As a
#                         group by a.Phylum, a.campaign, a.Place")

bacterias_long <- sqldf("select a.Phylum, a.campaign, a.Place, sum(a.value) As value
                         from bacterias_long As a
                         group by a.Phylum, a.campaign, a.Place")

bacterias_long <- dcast(bacterias_long, campaign + Place ~ Phylum, value.var = "value")

# bacterias_long <- dcast(bacterias_long, campaign + Place ~ Phylum + Class, value.var = "value")

hongos <- read_delim("Hongos_AR_total.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

hongos <- data.frame(hongos)

colnames(hongos)[101] <- "Family"

hongos_long <- melt(hongos, id.vars = c("Kingdom","Phylum","Class","Order","Family"))

hongos_long <- separate(hongos_long, col = "variable", into = c("Place", "campaign"), remove = TRUE)

hongos_long <- merge(hongos_long, fechas, type = "left", by.x = "campaign", by.y = "Fecha", all.x = TRUE)

hongos_long$campaign <- factor(hongos_long$campaign)



hongos_long <- hongos_long[colnames(hongos_long) %in% c("campaign","Phylum","value", "Place")]

# hongos_long <- aggregate(hongos_long, by = list("Phylum", "campaign"), sum)

hongos_long <- sqldf("select a.Phylum, a.campaign, a.Place, sum(a.value) As value
                        from hongos_long As a
                        group by a.Phylum, a.campaign, a.Place")

hongos_long <- dcast(hongos_long, campaign + Place ~ Phylum, value.var = "value")





# For CLASS

hongos_long <- hongos_long[colnames(hongos_long) %in% c("campaign","Phylum","Class","value", "Place")]

# hongos_long <- aggregate(hongos_long, by = list("Phylum", "campaign"), sum)

hongos_long <- sqldf("select a.Phylum, a.Class, a.campaign, a.Place, sum(a.value) As value
                        from hongos_long As a
                        group by a.Phylum, a.campaign, a.Place")

hongos_long <- dcast(hongos_long, campaign + Place ~ Phylum + Class, value.var = "value")


