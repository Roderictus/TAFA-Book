#### Manejo de datos y planteamiento de un modelo de regresión con datos de múltiples periodos
library(tidyverse)
#library(readxl)
library(stringr)
library(foreign)
library(data.table)
library(AER)

##EDOMEX

#Cargar EDOMEXSECC2017



#cambio participación
#cambio voto PRI

par(mfrow = c(3,1))
hist((MEX$EM_12_PRI_por + MEX$EM_12_PRI_PVEM_por), breaks  = 20)
hist(MEX$EM_17_POR_PRI, breaks = 20)
hist(MEX$PRI_17_12, breaks = 20)
summary(MEX$PRI_17_12)

head(EDOMEXSECC2017)
#######   Correlación Secciones EDOMEX2017      ############################
EDOMEXSECC2017 <- read.csv(file = "Datos/Electorales/Edomex/EDOMEXSECC2017.csv")
chart.Correlation(EDOMEXSECC2017[, c(30,31,32,33,34,36)], histogram = TRUE, pch = "+", alpha = .1)
head(EDOMEXSECC2017)
############################################################################
#######   Correlación Municipios EDOMEX2017      ############################
EDOMEXMUN2017 <- read.csv(file = "Datos/Electorales/Edomex/EDOMEXMUN2017.csv")
chart.Correlation(EDOMEXMUN2017[, c(28,30,29,31,34)], pch = "+")

head(MEX)


chart.Correlation(
  MEX %>% 
    dplyr::select(EM_17_POR_PRI,EM_12_PRI_por, IC_Por_Ingreso_Gobierno,EM_17_POR_PART)
)

############################################################################
MEX<-read.csv(file = "Datos/Múltiples periodos/IC_electorales_edomex.csv")
MEX$PRI_17_12 <- MEX$EM_17_POR_PRI - (MEX$EM_12_PRI_por + MEX$EM_12_PRI_PVEM_por) # cambio porcentual
#lista de los municipios 
#distribución del 80% de la lista nominal 
head(MEX)
#                                 Min     1st Q   Median  Mean    3rd Q   Max
summary(MEX$PRI_17_12)#          -26.350 -15.449 -11.692 -10.547  -6.646   15.004, Min, 1st Q, Median, Mean, 3rd qu, Max
summary(MEX$EM_17_LISTA_NOMINAL)# 3251    15809   27083   90503    66,211  1,206,075 
head(MEX)
chart.Correlation(MEX[, c(56:67, 72)])








Mex_lm  <- lm (PRI_17_12 ~              
                 IC_Por_Ingreso_Gobierno +
                 IC_Por_Ingreso_otro_Pais +
                 IC_Por_Poca_Variedad_Alimentos +
                 EM_17_TOTAL_VOTOS, data = MEX)
head(MEX)

MEX$EM_17_POR_PAN <- (MEX$EM_17_PAN/MEX$EM_17_TOTAL_VOTOS)*100
MEX$EM_17_POR_MORENA <- (MEX$EM_17_MORENA/MEX$EM_17_TOTAL_VOTOS)*100

Mex_lm  <- lm (EM_17_POR_PRI ~              
                 IC_Por_Ingreso_Gobierno +
                 EM_17_TOTAL_VOTOS, data = MEX)

summary(Mex_lm)

#cambio neto 
#Cambio voto PAN
#Cambio voto MOrena
#Diferencia entre primer lugar y segundo lugar
#concentración de votos

#correlaciones secciones edomex 2012
write.csv(x = P2012Secc, file = "Datos/Electorales/P2012Secc.csv")

#correlaciones municipio edomex 2012
write.csv(x = P2012Mun, file = "Datos/Electorales/P2012Mun.csv")# N

#correlaciones sección edomex 2017
#correlaciones municipio edomex 2017
write.csv(x = EDOMEXMUN2017, file = "Datos/Electorales/Edomex/EDOMEXMUN2017.csv") #correlación edomex 2017





####Municipio


#Varianza municipal de participación


#gráfica de bases variables 

library(ggplot2)
p  <- ggplot(df, aes(ymin = 0))
p1 <- p + geom_rect(aes(xmin = wm, xmax = w, ymax = height, fill = x))
library(grid) # needed for arrow function
p1 + geom_text(aes(x = wt, y = height * 0.8, label = x)) + 
  theme_bw() + labs(x = NULL, y = NULL) + 
  theme(axis.ticks = element_blank(),axis.text.x = element_blank(), 
        axis.text.y = element_blank(), legend.position = "none") + 
  annotate("text", x = 120, y = 83, label = "a Beta block") + 
  geom_segment(aes(x = 100, y = 80, xend = 80, yend = 75), 
               arrow = arrow(length = unit(0.5, "cm")))

dfplot <- ggplot(df, aes(x = Date, y = Value, fill = model)) + 
  geom_bar(position = "dodge",stat = "identity", aes(fill= model)) +
  scale_fill_manual(values = c("red","blue", "dark green", "purple"))+
  geom_text(aes(label= Value),position = position_dodge(width = 0.9),
            vjust = ifelse(df[,3]>=0, -0.5, 1) , size= 3)
#variable de votos perdidos


####Mapa Cambio Municipio Edomex 2017
####% votación PRI


#gráfica con anchos dependiendo de un valor

#variable de peso relativo respecto al total de la votación 2017

head(MEX)
EM_17_TOTAL_VOTOS

MEX <- MEX %>%
  mutate(Part_Total = EM_17_TOTAL_VOTOS/ sum(EM_17_TOTAL_VOTOS) * 100) %>% 
  arrange(desc(Part_Total))




MEX$NOM_MUN_JOIN<-NULL
MEX[,1:3]<-NULL
MEX$EM_12_ID_ESTADO<-NULL
MEX$EM_12_NOMBRE_ESTADO<-NULL

#MEX$EM_17_POR_PAN<-MEX$EM_17_PAN/MEX$EM_17_TOTAL_VOTOS*100
#MEX$EM_17_POR_PRD<-MEX$EM_17_PRD/MEX$EM_17_TOTAL_VOTOS*100
#MEX$EM_17_POR_MORENA<-MEX$EM_17_MORENA/MEX$EM_17_TOTAL_VOTOS*100

GANADOR_POR<-numeric(nrow(MEX))
GANADOR_PARTIDO<-character(nrow(MEX))
#asi est?n ordenados en la bbdd
PARTIDOS<-c("PRI","PAN","PRD","MORENA")
for(i in 1:nrow(MEX))
{
  j<-which.max(MEX[i,2:5])
  GANADOR_POR[i]<-MEX[i,(j+2)]/MEX$EM_17_TOTAL_VOTOS[i]*100
  GANADOR_PARTIDO[i]<-PARTIDOS[j]
}
MEX$GANADOR_POR17<-as.numeric(GANADOR_POR)
MEX$GANADOR_PARTIDO17<-GANADOR_PARTIDO



D = 125
MEX$EM_17_LISTA_NOMINAL
head(MEX)
A <- MEX[1:D,]$EM_17_MUNICIPIO
B <- (MEX[1:D,]$EM_17_TOTAL_VOTOS)/1000
C <- MEX[1:D,]$PRI_17_12
D <- MEX[1:D,]$GANADOR_PARTIDO17
df <- data.frame(A,B,C,D)
df$w <- cumsum(df$B)
df$wm <- df$w - df$B
df$wt <- with(df, wm + (w - wm)/2)
p <- ggplot(df, aes (ymin = 0))
p1 <- p + geom_rect(aes(xmin = wm, xmax = w, 
                        ymax = C, fill = D), color = "black")
p1 <- p1 + theme_bw() + labs(x = NULL, y = NULL) 
p1<- p1 + scale_fill_manual(values = c("chocolate3","dodgerblue2","yellow2","red1"))
p1 <-p1 + xlab("Miles de votos") + ylab("Cambio en el % obtenido por el alianza PRI()")
p1+ geom_text(aes(label = A, vjust = ifelse(B >= 0, 0, 1)))

ggsave(filename = "Figuras/Edomex 2017 Barplot", device = )


#p1 + theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 90)) + labs(x = NULL, y = NULL) 

#######Correlaciones Jalisco

IC_JAL <-read.csv(file = "Datos/Múltiples periodos/IC_electorales_jalisco.csv")
head(IC_JAL)

chart.Correlation(
  IC_JAL %>% 
    dplyr::select(JAL15_PRI_por,IC_Por_Ingreso_Gobierno,JAL15_Por_Part, )
)

