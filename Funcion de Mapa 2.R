#Función de mapa

genera_etiquetas<-function(labels)
{
  labels<-gsub('\\(','',gsub('\\]','',labels))
  lab1<-labels[seq(from=1,to=length(labels),by=2)] %>% as.numeric %>%sort %>%as.character
  lab1<-paste0('(',lab1)
  lab2<-labels[seq(from=2,to=length(labels),by=2)] %>% as.numeric %>%sort %>%as.character
  lab2<-paste0(lab2,']')
  labels<-character(length(labels)/2)
    for(i in 1:length(labels))
  {
    labels[i]<-paste0(lab1[i],",",lab2[i])
  }
  labels
}

grafica_basica<-function(base,variable,main,color_inicial="white",color_fin="red",cortes=10) {
  #cambiarlo por labels
  labels<-cut(base[,variable],breaks=cortes)%>%unique%>%as.character%>%strsplit(',')%>%unlist
  labels<-genera_etiquetas(labels)
  fill<-colorRampPalette(c(color_inicial,color_fin))(cortes)[as.numeric(cut(base[,variable],breaks=cortes))]
  grafica<-ggplot(data=base,aes(x=long, y=lat, group=group)) +
    geom_polygon(aes(fill=fill), show.legend = T)+
    geom_polygon(data=base, aes(x=long, y=lat, group=group), fill=NA, color="black", size=0.4) +
    #scale_fill_gradient(space="Lab",na.value="white",name=variable,label=c(1,2,3,4,5),low="white",high=color,guide=F) +
    theme_void() + # remove axes
    coord_equal() +
    labs( x = NULL, y = NULL, NULL,title = main) +
    theme(legend.position = "bottom") +
    scale_fill_identity("escala",labels=labels,breaks=sort(unique(fill),decreasing=T),guide="legend")
  grafica
}


grafica_basica(base = Municipios_Map, 
               variable = PAN_por, 
               main = "Titulo tituloso", 
               cortes = 10)
base <- read.csv("Datos/Electorales/Jalisco/MunMapJal2015.csv")
variable<- "MC_por"

head( )

grafica_basica(base, variable, main = "Porcentaje de voto MC, Elecciones 2015", cortes = 5 )
grafica_basica(base, variable, main = "Porcentaje de voto MC, Elecciones 2015", cortes = 5 )


table(base$MC_por, exclude = FALSE)
