
library(needs)
needs(tidyverse, magrittr, rgdal, grid, gridExtra, mapproj, rgeos) 
library(stringr)
library(data.table)
library(maptools)
library(sqldf)
library(raster)
jalisco<-read.csv("C:/Users/FR501780/Documents/TAFA-Book-master/Datos/M?ltiples periodos/IC_electorales_jalisco.csv")
are
ruta<-paste0("C:/Users/FR501780/Documents/TAFA-Book-master/shape_files/14/MUNICIPIO.shp")

mun<-readOGR(ruta,encoding = 'UTF-8')
area_jalisco<-area(mun)/1000000
mun<-mun@data
mun$area<-area_jalisco

jalisco<-jalisco[,c(4,3,48)]

densidad<-sqldf("select a.*, b.area from jalisco as a inner join mun as b 
                on a.PRES12_ID_MUNICIPIO= b.municipio ")

#esta es la base final
densidad$densidad<-densidad$JAL15_Boletas/densidad$area



edomex<-read.csv("C:/Users/FR501780/Documents/TAFA-Book-master/Datos/M?ltiples periodos/IC_electorales_edomex.csv")

ruta<-paste0("C:/Users/FR501780/Documents/TAFA-Book-master/shape_files/15/MUNICIPIO.shp")

mun<-readOGR(ruta,encoding = 'UTF-8')
area_edomex<-area(mun)/1000000
mun<-mun@data
mun$area<-area_edomex

edomex<-edomex[,c(3,4,9)]

densidad_EDOMEX<-sqldf("select a.*, b.area from edomex as a inner join mun as b 
                on a.EM_17_ID_MUNICIPIO= b.municipio ")
densidad_EDOMEX$densidad<-densidad_EDOMEX$EM_17_LISTA_NOMINAL/densidad$area

