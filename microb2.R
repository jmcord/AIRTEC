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
library(stringr)

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

bacterias_long_class <- bacterias_long[colnames(bacterias_long) %in% c("campaign","Phylum","Class", "value", "Place")]

bacterias_long <- bacterias_long[colnames(bacterias_long) %in% c("campaign","Phylum","value", "Place")]

bacterias_long$value <- str_replace(bacterias_long$value, ",",".") %>% as.numeric

bacterias_long_class$value <- str_replace(bacterias_long_class$value, ",",".") %>% as.numeric

bacterias_long <- sqldf("select a.Phylum, a.campaign, a.Place, sum(a.value) As value
                         from bacterias_long As a
                         group by a.Phylum, a.campaign, a.Place")



bacterias_long_class$phylum_class <- paste0(bacterias_long_class$Phylum,"_",bacterias_long_class$Class)

bacterias_long_class <- sqldf("select a.Phylum, a.phylum_class, a.campaign, a.Place, sum(a.value) As value
                         from bacterias_long_class As a
                         group by a.phylum_class, a.campaign, a.Place")

bacterias_long <- dcast(bacterias_long, campaign + Place ~ Phylum, value.var = "value")

bacterias_long_class <- dcast(bacterias_long_class, campaign + Place ~ phylum_class, value.var = "value")


apply(bacterias_long_class[,c(3:165)], 1, sum)

# Hongos

hongos <- read_delim("Hongos_AR_total.csv", 
                     ";", escape_double = FALSE, trim_ws = TRUE)

hongos <- data.frame(hongos)

colnames(hongos)[101] <- "Family"

hongos_long <- melt(hongos, id.vars = c("Kingdom","Phylum","Class","Order","Family"))

hongos_long <- separate(hongos_long, col = "variable", into = c("Place", "campaign"), remove = TRUE)

hongos_long <- merge(hongos_long, fechas, type = "left", by.x = "campaign", by.y = "Fecha", all.x = TRUE)

hongos_long$campaign <- factor(hongos_long$campaign)

hongos_long_class <- hongos_long[colnames(hongos_long) %in% c("campaign","Phylum","Class", "value", "Place")]

hongos_long <- hongos_long[colnames(hongos_long) %in% c("campaign","Phylum","Class","value", "Place")]

# hongos_long <- aggregate(hongos_long, by = list("Phylum", "campaign"), sum)

hongos_long$value <- str_replace(hongos_long$value, ",",".") %>% as.numeric

hongos_long_class$value <- str_replace(hongos_long_class$value, ",",".") %>% as.numeric

hongos_long <- sqldf("select a.Phylum, a.campaign, a.Place, sum(a.value) As value
                        from hongos_long As a
                        group by a.Phylum, a.campaign, a.Place")

hongos_long_class$phylum_class <- paste0(hongos_long_class$Phylum,"_",hongos_long_class$Class)

hongos_long_class <- sqldf("select a.Phylum, a.phylum_class, a.campaign, a.Place, sum(a.value) As value
                         from hongos_long_class As a
                         group by a.phylum_class, a.campaign, a.Place")

hongos_long <- dcast(hongos_long, campaign + Place ~ Phylum, value.var = "value")


hongos_long_class <- dcast(hongos_long_class, campaign + Place ~ phylum_class, value.var = "value")


apply(hongos_long_class[,c(3:38)], 1, sum)



#15_03_2021 para el CCT AIRTEC

bacterias_long_melt <- melt(bacterias_long[1,])
bacterias_long_melt <- bacterias_long_melt[-46,]


ggplot(data = bacterias_long_melt) +
  geom_bar(aes(x=variable,y=value), fill = 'steelblue', stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylim(0,50) +
  ylab('Bacteria % Relative Abundance') +
  xlab('Bacteria Phylum')



hongos_long_melt <- melt(hongos_long[1,])
hongos_long_melt <- hongos_long_melt[-46,]


ggplot(data = hongos_long_melt) +
  geom_bar(aes(x=variable,y=value), fill = 'steelblue', stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylim(0,100) +
  ylab('hongos % Relative Abundance') +
  xlab('hongos Phylum')
