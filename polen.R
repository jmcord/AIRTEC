rm(list=ls())

library(AeRobiology)
library(xlsx)


setwd("~/AIRTEC/R")

ficheros <- list.files("C:/Users/jcord/Documents/AIRTEC/OBJETIVO_1/Polen")

polen_list <- lapply(ficheros, function(x) 
  read.xlsx(paste0("C:/Users/jcord/Documents/AIRTEC/OBJETIVO_1/Polen/",x)))

