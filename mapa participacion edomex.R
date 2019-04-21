library(dplyr)
library(magrittr)
library(knitr)
library(rgeos)
library(rgdal)
library(raster)
library(ggplot2)
library(dplyr)
library(gtable)
library(grid)
library(stringr)
library(data.table)
library(viridis)
library(tidyr)
library(kableExtra)
library(gridExtra)
library(lattice)
library(PerformanceAnalytics)#para correlaciones del voto 
library(broom) #tidy en vez de fortify
library(foreign )
library(spatstat)

#cargar mapa de municipios del Estado de México

Mun_EDOMEX <- read.csv(file = "D:/Proyectos R/TAFA-Book/Datos/2019 shapefiles/MUNICIPIO/MUNICIPIO.shp")

library(rgdal)
library(plyr)   
library(ggplot2)

#data.shape<-readOGR(dsn="D:/Proyectos R/TAFA-Book/Datos/2019 shapefiles/MUNICIPIO",layer="MUNICIPIO") # parece que no tiene una capa con el nombre del municipio

MEXMAP <- readOGR(dsn = "D:/Proyectos R/TAFA-Book/Datos/2019 shapefiles/MUNICIPIO/2012", layer = "Muni_2012gw", use_iconv = TRUE, encoding = "UTF-8") #con esto carga bien los acentos
DataMapEdomex <- MEXMAP[MEXMAP$CVE_ENT == 15,]@data #subseteamos los datos, MEXMAP[[3]] capa de los municipios
MEXMAPf <- fortify(MEXMAP) #fortify a todo
head(MEXMAPf)
plot(MEXMAPf)

MEXMAP[[3]] #nombre del municipio, abrir con algo que permita acentos
MEXMAP <- MEXMAP[MEXMAP$CVE_ENT == 15,]#subset a datos del Estado de México

plot(world.map)

#reducir la resolución del mapa de municipios


#



#fortificar
#añadir a la base 
#mapa de municipios de México 2012


mapa <- fortify(data.shape) #esto es para todo el país

map@data$id <- rownames(mapa@data)
mapa@data   <- join(mapa@data, data, by="CD_GEOCODI")
mapa.df     <- fortify(mapa)
mapa.df     <- join(mapa.df,mapa@data, by="id")




colnames(MUNdf)

MUNdf[1]#reducir para tener sólo EDOMEX
MUNdf@data


mapa <- readOGR(dsn=".",layer="shapefile name w/o .shp extension")
map@data$id <- rownames(mapa@data)
mapa@data   <- join(mapa@data, data, by="CD_GEOCODI")
mapa.df     <- fortify(mapa)
mapa.df     <- join(mapa.df,mapa@data, by="id")

ggplot(mapa.df, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=Population))+
  coord_fixed()





head(data.shape)
plot(data.shape)








Municipios_Map <- read.csv(file = "C:/Proyectos R/TAFA-Book/Datos/Electorales/Jalisco/MunMapJal2015.csv")

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank()
    )
}

####    etiquetas para los mapas
no_classes <- 6
labels <- c()
quantiles <-quantile(Municipios_Map$Por_Part,
                     probs = seq(0,1,length.out = no_classes + 1 ))
labels <- c()
for(idx in 1:length(quantiles)) {
  labels<-c(labels, paste0(round(quantiles[idx],2),
                           "-",
                           round(quantiles[idx + 1],2)))
}
labels <- labels[1:length(labels)-1]
Municipios_Map$Por_Part2 <- cut(Municipios_Map$Por_Part,
                                breaks = quantiles, 
                                labels = labels, 
                                include.lowest = T)
#####
Plot_Map <- ggplot() +
  geom_polygon(data = Municipios_Map, aes(fill = Por_Part2, 
                                          x = long,
                                          y = lat, 
                                          group = group)) +
  geom_path(data = Municipios_Map, aes( x = long, 
                                        y = lat, 
                                        group = group),
            color = "white", size = 0.2) +
  coord_equal() +
  theme_map() +
  labs( x = NULL, 
        y = NULL, NULL,
        title = "Participación electoral Municipal Guadalajara 2015") +
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis",
    name = "Participación Electoral Municipal Guadalajara 2015",
    discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    ))
Plot_Map
```
