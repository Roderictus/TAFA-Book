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
library(viridis)
library(dplyr)


#data.shape<-readOGR(dsn="D:/Proyectos R/TAFA-Book/Datos/2019 shapefiles/MUNICIPIO",layer="MUNICIPIO") # parece que no tiene una capa con el nombre del municipio
MEXMAP <- readOGR(dsn = "D:/Proyectos R/TAFA-Book/Datos/2019 shapefiles/MUNICIPIO/2012", layer = "Muni_2012gw", use_iconv = TRUE, encoding = "UTF-8") #con esto carga bien los acentos
DataMapEdomex <- MEXMAP[MEXMAP$CVE_ENT == 15,]@data #subseteamos los datos, MEXMAP[[3]] capa de los municipios
DataMapEdomex$id <- DataMapEdomex$CVE_MUN #con esta variable vamos a unir los datos con el mapa
#head(DataMapEdomex)
#subseteo a todo
MEXMAP2 <-MEXMAP[MEXMAP$CVE_ENT == 15,] #esto funciona
MEXMAPf <- fortify(MEXMAP2, region = "CVE_MUN") #fortify sólo a EDOMEX, region se muestra como "id"
MEXMAPf <- join(x = MEXMAPf, y = DataMapEdomex)
head(MEXMAPf)

#salvar data geográfica del edomex
#write.csv(x = MEXMAPf, file = "Datos/2019 shapefiles/MEXMAPF.csv") #152,141
#length(table(MEXMAPf$id)) #125 que es lo que esperamos 
#unir con datos de la base principal, unir por nombre de municipio
#subset de la variable que nos interesa para hacer el mapa
#colnames(EMENSAYO)
#tomamos número de municipio para ver si el join funciona con eso
head(MEXMAPf)#CVE_MUN, para el join
#parece que funciona
temp <- left_join(MEXMAPf, 
                  EMENSAYO %>% select(MUN, NOM_MUN, Por_Ingreso_Gobierno, DLNominal17,
                                      EM_12_Por_Part,EM_12_Por_Part))
#length(table(temp$NOM_MUN))
#table(is.na(temp$NOM_MUN))#todo embona 

#################################################################################
##############    Código para Mapa    ###########################################
#################################################################################
mapa.df <- temp
head(mapa.df)

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

no_classes <- 6
labels <- c()
quantiles <-quantile(mapa.df$EM_12_Por_Part,
                     probs = seq(0,1,length.out = no_classes + 1 ))
labels <- c()
for(idx in 1:length(quantiles)) {
  labels<-c(labels, paste0(round(quantiles[idx],2),
                           "-",
                           round(quantiles[idx + 1],2)))
}
labels <- labels[1:length(labels)-1]

mapa.df$EM_12_Por_Part2 <- cut(mapa.df$EM_12_Por_Part,
                                breaks = quantiles, 
                                labels = labels, 
                                include.lowest = T)


#ggplot(mapa.df, aes(x=long, y=lat, group=group))+
#  geom_polygon(aes(fill=EM_12_Por_Part))+
#  coord_fixed()

####    etiquetas para los mapas
#####
Plot_Map <- ggplot() +
  geom_polygon(data = mapa.df, aes(fill = EM_12_Por_Part2, 
                                          x = long,
                                          y = lat, 
                                          group = group)) +
  geom_path(data = mapa.df, aes( x = long, 
                                        y = lat, 
                                        group = group),
            color = "white", size = 0.2) +
  coord_equal() +
  theme_map() +
  labs( x = NULL, 
        y = NULL, NULL,
        title = "Participación electoral municipal 2017 Estado de México") +
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "viridis",
    name = "placeholder",
    discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    ))
Plot_Map
```

##########################    Otra versión de Mapa    #################


#nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
ggplot(nc) +
  geom_sf(aes(fill = AREA)) +
  scale_fill_viridis("Area") +
  ggtitle("Area of counties in North Carolina") +
  theme_bw()

st_read()