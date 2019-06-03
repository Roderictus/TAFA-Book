library(AER)#libro de econometria con R
library(PerformanceAnalytics)#para correlaciones del voto 
library(tidyverse)
library(readxl)
library(stargazer)

#Las bases de datos electorales ya se encuentran trabajadas
#La base de datos para la intercensal se codifica aquí
#bajar base de datos 
#WebEdoMex <-"https://www.inegi.org.mx/contenidos/programas/intercensal/2015/microdatos/eic2015_15_csv.zip" #Para el estado de México
#Bases de datos bajadas a mano
#download.file(url = WebEdoMex, destfile = "D:/Proyectos R/TAFA-Book/Datos/2018/")
######################################################################################
#####           Trabajo de bases de Intercensal sólo para el EDOMEX   ################
#####                             VIVIENDA                            ################
######################################################################################
ViviendaIC$JEFE_SEXO<-factor(ViviendaIC$JEFE_SEXO, labels = c("Hombre", "Mujer"))
ViviendaIC[ViviendaIC$JEFE_EDAD == "999", ]$JEFE_EDAD <- NA#clasificar correctamente los NAs de edad del jefe del hogar

IC_Municipio <-ViviendaIC %>%
  group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
  summarise(Viviendas                = sum(FACTOR),
            Jefe_Hombre              = sum(FACTOR[JEFE_SEXO        == "1"]),
            Jefe_Mujer               = sum(FACTOR[JEFE_SEXO        == "3"]),
            Ingreso_Otro_Pais        = sum(FACTOR[INGR_PEROTROPAIS == "1"]),
            Ingreso_del_Pais         = sum(FACTOR[INGR_PERDENTPAIS == "3"]),
            Ingreso_Gobierno         = sum(FACTOR[INGR_AYUGOB      == "5"]),
            No_Ingreso_Gobierno      = sum(FACTOR[INGR_AYUGOB      == "6"]),
            Jubilacion_Pension       = sum(FACTOR[INGR_JUBPEN      == "7"]),
            Poca_Variedad_Alimentos  = sum(FACTOR[ING_ALIM_ADU3    == "5"]),
            Radio                    = sum(FACTOR[RADIO            == "1"],na.rm = T),
            Radio_div                = sum(FACTOR[!is.na(RADIO)]),
            Televisor                = sum(FACTOR[TELEVISOR        == "3"],na.rm = T),
            Computadora              = sum(FACTOR[COMPUTADORA      == "7"],na.rm = T),
            Telefono_fijo            = sum(FACTOR[TELEFONO         == "1"],na.rm = T),
            Celular                  = sum(FACTOR[CELULAR          == "3"],na.rm = T),
            Internet                 = sum(FACTOR[INTERNET         == "5"],na.rm = T),
            Television_paga          = sum(FACTOR[SERV_TV_PAGA     == "7"],na.rm = T),
            Dueño                    = sum(FACTOR[TENENCIA         == "1"],na.rm = T),
            Renta                    = sum(FACTOR[TENENCIA         == "2"],na.rm = T)) %>%
  mutate(Por_Jefe_Hombre = (Jefe_Hombre/Viviendas) * 100,
         Por_Jefe_Mujer = (Jefe_Mujer/Viviendas) *100, 
         Por_Ingreso_otro_Pais= (Ingreso_Otro_Pais/Viviendas) * 100,
         Por_Ingreso_del_Pais = (Ingreso_del_Pais/Viviendas) * 100,
         Por_Ingreso_Gobierno = (Ingreso_Gobierno/Viviendas) * 100,
         Por_Jubilacion_Pension = (Jubilacion_Pension/Viviendas) * 100,
         Por_Poca_Variedad_Alimentos = (Poca_Variedad_Alimentos/Viviendas) * 100,
         Por_Radio = (Radio/Viviendas) * 100,
         Por_Televisor =  (Televisor/Viviendas) * 100,
         Por_Computadora = (Computadora/Viviendas) * 100,
         Por_Telefono_Fijo =(Telefono_fijo /Viviendas) * 100,
         Por_Celular = (Celular/Viviendas) * 100,
         Por_Internet = (Internet/Viviendas) * 100,
         Por_Television_paga = (Television_paga/Viviendas) * 100,
         Por_Dueño =(Dueño/Viviendas) * 100,
         Por_Renta = (Renta/Viviendas) * 100))

############################################################################################
##############         Unión de las bases para ensayo de EDOMEX       ######################
############################################################################################
#Base con datos electorales
IC_Municipio$NOM_MUN_JOIN <- IC_Municipio$NOM_MUN
#Código para encontrar municipios que hay que renombrar
#intersect(chartr('áéíóúñ','aeioun',unique(tolower(EDOMEXENSAYO$NOM_MUN_JOIN))),
#          chartr('áéíóúñ','aeioun',unique(tolower(IC_Municipio$NOM_MUN_JOIN))))#124 aparecen en ambos
#setdiff(chartr('áéíóúñ','aeioun',unique(tolower(IC_Municipio$NOM_MUN))),
#        intersect(chartr('áéíóúñ','aeioun',unique(tolower(EDOMEXENSAYO$NOM_MUN_JOIN))),
#                  chartr('áéíóúñ','aeioun',unique(tolower(IC_Municipio$NOM_MUN_JOIN)))))
#acambay de ruiz castaneda
#Seguimos los nombres de EDOMEXENSAYO
EDOMEXENSAYO$NOM_MUN_JOIN[1]
IC_Municipio$NOM_MUN_JOIN <- chartr('áéíóúñ','aeioun',tolower(IC_Municipio$NOM_MUN)) #minúsculas y quitar acentos
#cambio a "acambay" en la base intercensal
IC_Municipio$NOM_MUN_JOIN[1] <- "acambay"

##########################################################################
##############        Unir las bases                ######################
##########################################################################

EMENSAYO <- inner_join(IC_Municipio, EDOMEXENSAYO, by = "NOM_MUN_JOIN")
rm(EDOMEXENSAYO)
rm(IC_Municipio)
rm(ViviendaIC)
#guardar el archivo
#write.csv(x = EMENSAYO, file = "Datos/2018/ENSAYO/ElectICEdomex20190305.csv")

#Extensión territorial en km2 para el EDOMEX
AREASEDOMEX <- c(465.7 , 83.95 , 453.26, 182.65, 485.21, #5
                 46.53 , 222.27, 638.55, 189.48, 75.73 , #10
                 83.8  , 6.92  , 91.07 , 267.89, 162.06, #15
                 230.94, 36.41 , 101.19, 32.25 , 35.1  , #20
                 282.36, 14.86 , 49.32 , 26.32 , 219.22, #25
                 292.32, 12.62 , 20.7  , 53.91 , 6.82  , #30
                 44.69 , 192.03, 160.17, 50.77 , 118.02, #35
                 233.91, 140.67, 75.79 , 327.4 , 110.75, #40
                 101.35, 335.85, 116.47, 4.73  , 583.95, #45
                 119.7 , 272.56, 277.26, 63.66 , 140.11, #50
                 212.83, 204.95, 17.78 , 67.52 , 11.47 , #55
                 236.32, 156.63, 63.74 , 54.51 , 235.65, #60
                 83.7  , 134.72, 314.53, 137.47, 195.56, #65
                 157.43, 116.67, 45.64 , 3.19  , 36.36 , #70
                 127.49, 23.4  , 18.5  , 368.15, 67.22 , #75
                 27.38 , 129.23, 104.25, 128.8 , 564.04, #80
                 157.34, 669.13, 28.75 , 163.8 , 362.39, #85 
                 544.59, 190.34, 163.15, 37.77 , 207.54, #90
                 53.04 , 83.16 , 178.37, 42.98 , 187.82, #95
                 122.32, 150.66, 24.78,  432.61, 17.46 , #100
                 167.97, 172.81, 161.57, 77.17 , 791.49, #105
                 452.371,91.98 , 27.22 , 69.15 , 430.8 , #110
                 309.28, 306.56, 209.96, 419.35, 65.85 , #115
                 66.67 , 301.47, 308.62, 201.18, 223.95, #120
                 109.54, 46.53 , 703.00, 492.25, 8.47)   #125 
#A mano desde datos de wikipedia, chequé que estuvieran en el mismo orden los mismos nombres de municipios

EMENSAYO$AREAkm <- AREASEDOMEX
#Densidad del padrón electoral
EMENSAYO$DLNominal12 <-(EMENSAYO$EM_12_Lista_Nominal/EMENSAYO$AREAkm) #2012
EMENSAYO$DLNominal17 <-(EMENSAYO$EM_17_LISTA_NOMINAL/EMENSAYO$AREAkm) #2017
EMENSAYO$CrecLN1217  <- (EMENSAYO$EM_17_LISTA_NOMINAL/EMENSAYO$EM_12_Lista_Nominal) #crecimiento de la lista nominal
#EMENSAYO[order(EMENSAYO$DLNominal17),]$NOM_MUN_JOIN #Orden menor a mayor por densidad de Lista Nominal
EMENSAYO$NOM_MUN_JOIN <- as.character(EMENSAYO$NOM_MUN_JOIN)  #transformar columna del join en character

#########################################################################################
##############  Partido que controla el municipio en 2015   #############################  
#########################################################################################
#Participación electoral y partido ganador
PM2015 <- read_xlsx(path = "D:/Proyectos R/TAFA-Book/Datos/2015/Computo_MUNICIPAL_2015.xlsx", range = "B7:AG132")
PM2015 <- PM2015[,c(1,27,32)]
colnames(PM2015) <- c("NOM_MUN_JOIN","Part_Mun_2015", "PG_Mun_2015")#renombrar para el join 
PM2015$NOM_MUN_JOIN <- chartr('áéíóúñ','aeioun',tolower(PM2015$NOM_MUN_JOIN)) #para tener nombres homogeneos
left_join(EMENSAYO, PM2015)$Part_Mun_2015# 57 y 58
#renombrar para que funcione el join
PM2015$NOM_MUN_JOIN[58] <-"naucalpan de juarez"
PM2015$NOM_MUN_JOIN[60] <- "nezahualcoyotl"
EMENSAYO <- left_join(EMENSAYO, PM2015)
#En Jilotepec gana Nueva Alianza pero es codificado como valor perdido
#convertirlo a character
EMENSAYO$PG_Mun_2015_REC  <- as.character(EMENSAYO$PG_Mun_2015_REC)
table(EMENSAYO$PG_Mun_2015_REC)

EMENSAYO[is.na(EMENSAYO$PG_Mun_2015_REC),]$PG_Mun_2015_REC <- "Nueva Alianza"
#Recodificar 
# PRI o PRI Alianza
EMENSAYO[EMENSAYO$PG_Mun_2015_REC == "PRI-PVEM-NA",]$PG_Mun_2015_REC <- "PRI o PRI Alianza"
EMENSAYO[EMENSAYO$PG_Mun_2015_REC == "PRI",]$PG_Mun_2015_REC <- "PRI o PRI Alianza"
# PAN o PAN Alianza
EMENSAYO[EMENSAYO$PG_Mun_2015_REC == "PAN-PT",]$PG_Mun_2015_REC <- "PAN o PAN Alianza"
EMENSAYO[EMENSAYO$PG_Mun_2015_REC == "PAN",]$PG_Mun_2015_REC <- "PAN o PAN Alianza"
#Otros
EMENSAYO[EMENSAYO$PG_Mun_2015_REC == "ES",]$PG_Mun_2015_REC <- "Otros"
EMENSAYO[EMENSAYO$PG_Mun_2015_REC == "MC",]$PG_Mun_2015_REC <- "Otros"
EMENSAYO[EMENSAYO$PG_Mun_2015_REC == "MORENA",]$PG_Mun_2015_REC <- "Otros"
EMENSAYO[EMENSAYO$PG_Mun_2015_REC == "PT",]$PG_Mun_2015_REC <- "Otros"
EMENSAYO[EMENSAYO$PG_Mun_2015_REC == "Nueva Alianza",]$PG_Mun_2015_REC <- "Otros"
#cambiar orden de los factores para poner como base a PRI
EMENSAYO$PG_Mun_2015_REC <- as.factor(EMENSAYO$PG_Mun_2015_REC)
EMENSAYO$PG_Mun_2015_REC <-relevel(x = EMENSAYO$PG_Mun_2015_REC, ref = "PRI o PRI Alianza")

####Cambio en la participación electoral entre elecciones
#Para gráfica de cambio en la participación electoral
#2015-2017
EMENSAYO$Dif1712 <- EMENSAYO$EM_17_POR_PART - EMENSAYO$EM_12_Por_Part 
EMENSAYO$Dif1715 <- EMENSAYO$EM_17_POR_PART - (EMENSAYO$Part_Mun_2015 * 100)
#Incluir tabla con resultados electorales del 2015
#Explicación de las variables
#Normalizar variables
scale(x = EMENSAYO$Dif1712)
colnames(EMENSAYO)[119] <- "Densidad_de_Lista_Nominal"
colnames(EMENSAYO)[125] <- "Partido_Gobernante_2015"
colnames(EMENSAYO)[30] <- "Ingreso_Gobierno"
####################################################################################
###################     Resultados de la elección 2011    ##########################
####################################################################################
colnames(EMENSAYO)[30]<- "Por_Ingreso_Gobierno" # Se repetía este nombre
#Incorporar una base en excel
library(readxl)
EDOMEX2011 <- read_xls(path = "D:/Proyectos R/TAFA-Book/Datos/2011/Computo_FINAL_GOB_2011.xls", sheet = "CASILLA Y SECCIÓN", range = "A9:N17498")
colnames(EDOMEX2011)[14] <- "Lista_Nominal" #formato adecuado para el nombre 
EDOMEX2011 <- EDOMEX2011 %>% group_by(Municipio) %>% summarise(EM11_Total = sum(Total), EM11_Lista_Nominal= sum(Lista_Nominal), 
                                                  EM11_UPM = sum(UPM), EM11_UPT = sum(UPT), EM11_PAN = sum(PAN))
EDOMEX2011$EM11_Por_Part <- (EDOMEX2011$EM11_Total/EDOMEX2011$EM11_Lista_Nominal)*100
#Incorporar a la base principal
EDOMEX2011$NOM_MUN_JOIN <- chartr('áéíóúñ','aeioun',tolower(EDOMEX2011$Municipio)) #para tener nombres homogeneos
#"coacalco de berriozabal"     "ecatepec de morelos"         "xalatlaco"                   "naucalpan de juarez"        
# "tlalnepantla de baz"         "valle de chalco solidaridad"

EDOMEX2011$NOM_MUN_JOIN[27]  <- "coacalco de berriozabal"
EDOMEX2011$NOM_MUN_JOIN[34]  <- "ecatepec de morelos"
EDOMEX2011$NOM_MUN_JOIN[45]  <- "xalatlaco"
EDOMEX2011$NOM_MUN_JOIN[61]  <- "naucalpan de juarez"
EDOMEX2011$NOM_MUN_JOIN[107] <- "tlalnepantla de baz"
EDOMEX2011$NOM_MUN_JOIN[114] <- "valle de chalco solidaridad"

EMENSAYO <- left_join(EMENSAYO, EDOMEX2011, by = "NOM_MUN_JOIN")
#write.csv(x = EMENSAYO, file = "Datos/2018/ENSAYO/EMENSAYO20190403.csv")
EMENSAYO <- read.csv(file = "Datos/2018/ENSAYO/EMENSAYO20190403.csv")
######################################## Aquí termina el trabajo adicional a bases de datos  ##################

###################################################################
##########      Gráficas de correlación     #######################
###################################################################
EMENSAYO<-read.csv(file = "Datos/2018/ENSAYO/EMENSAYO20190403.csv") 




colnames(EMENSAYO)
#Seleccionar las variables cuya correlación nos interesa
#nombres de las variables
EMENSAYO %>% select()

EMENSAYO %>% select(Por_Ingreso_Gobierno, Por_Ingreso_otro_Pais, Por_Poca_Variedad_Alimentos,
       EM_12_Por_Part, EM_12_PAN_por, EM_12_PRI_por, EM_12_PRD_por, EM_12_PVEM_por, EM_12_PT_por, EM_12_MC_por, 
       EM_12_NVA_ALIANZA_por, EM_12_PRI_PVEM_por, EM_12_PRD_PT_MC_por, EM_12_PRD_PT_por, EM_12_PRD_MC_por, 
       EM_12_PT_MC_por, EM_17_POR_PART, EM_17_POR_PRI, EM_17_PRI_ALIANZA_POR, EM_17_POR_PAN, EM_17_POR_PRD, 
       EM_17_POR_PRI, EM_17_POR_MORENA) %>% chart.Correlation(histogram = TRUE)

EMENSAYO %>% ungroup() %>%
  select(EM_12_PRI_por, EM_12_PAN_por, EM_12_PRD_por, EM_12_Por_Part, DLNominal12, Por_Ingreso_Gobierno) %>%
  chart.Correlation(histogram = TRUE)


EMENSAYO %>% ungroup() %>%
  select(EM_12_PRI_por, EM_12_PAN_por, EM_12_PRD_por, EM_12_Por_Part, DLNominal12, Por_Ingreso_Gobierno, Part_Mun_2015) %>%
  chart.Correlation(histogram = TRUE)


colnames(EMENSAYO)

EMENSAYO %>% 
  group_by(PG_Mun_2015) %>% 
  summarise(Avg = mean(EM_17_POR_PART))



###################################################################
##########      Numeralia             #############################
###################################################################

#Descriptivos de variables
#Por_Ingreso_Gobierno
#Por_Poca_Variedad_Alimentos


###################################################################
##########      Modelo de regresión     #######################
###################################################################

colnames(EMENSAYO)
table(EMENSAYO$PG_Mun_2015)

Mod1 <- lm(EM_17_POR_PART ~ Por_Ingreso_Gobierno, data = EMENSAYO)
summary(Mod1)
anova(Mod1)
confint(Mod1, level = 0.95)
plot(Mod1)

Mod2 <- lm(EM_17_POR_PART ~ Por_Ingreso_Gobierno + DLNominal17, data = EMENSAYO)
summary(Mod2)

Mod3 <- lm(EM_17_POR_PART ~ Por_Ingreso_Gobierno + DLNominal17 + PG_Mun_2015_REC + Dif1712 + Dif1715, data = EMENSAYO)
summary(Mod3)

#sólo participaciones gubernamentales






contrasts(EMENSAYO$PG_Mun_2015)
table(EMENSAYO$PG_Mun_2015)



summary(Mod1)
anova(Mod1)
confint(Mod1, level = 0.95)
plot(Mod1)




#variable dependiente voto por el PRI en el 2017
colnames(EMENSAYO)

temp <- lm(EM_12_PRI_por ~ EM_12_Por_Part + Por_Ingreso_Gobierno + 
     DLNominal17 + EM_12_PRI_por + Part_Mun_2015, data = EMENSAYO)

stargazer(temp, type = "text")



######## meadias ponderadas por partido que controla el municipio



#####modelo de regresión

lm(log(subs) ~ log(price/citations), data = Journals) 


#múltiples gráficas en una sola imagen


#subset para 2017
#por densidad de lista nominal
#por densidad de viviendas
#por clasificación de urbano y rural
#correlación 2012 vs. 2017


#Elegir las columnas pertinenetes
######      Primero 2012

#colnames(Ganador2012)[max.col(Ganador2012, ties.method = "first")] %>% table() #No coincide con los resultados oficiales
#utilizar actas de computo municipales, resultados municipales 2012, 2017
rm(temp)
#2017
#EMENSAYO %>% ungroup() %>% select(contains(match = "_POR")) %>% select(contains(match = "_17")) #también funciona
#temp <- EMENSAYO[,c("EM_17_POR_PRI","EM_17_POR_Nueva.A", "EM_17_PRI_ALIANZA_POR", "EM_17_POR_PAN", "EM_17_POR_PRD", "EM_17_POR_PT",
#            "EM_17_POR_PVEM", "EM_17_POR_MORENA", "EM_17_PRI_ALIANZA_POR")] 


#colnames(temp)[max.col(temp, ties.method = "first")] %>% table()
#renombrar columnas
#revisar contra resultados oficiales
#Salvar archivo, el documento puede empezar por cargar la base
#modelo de regresión
#Correlaciones con densidad de lista nominal en las elecciones del 2012
#Correlaciones con densidad de lista nominal en elecciones 2017
#Ejemplo mínimo para correlaciones
#Ayuntamientos 2012

#http://www.ieem.org.mx/proceso_2012/re2012/seccionAyuntamientos%202012.xlsx #resultados por sección
#el archivo ya está bajado, leerlo
#http://www.ieem.org.mx/proceso_2012/re2012/ayuntamientos2012_TEEM.xlsx #resultados por municipio
#http://www.ieem.org.mx/proceso_2012/planillas/ayunta2013_2015.pdf #integración de ayuntamientos


colnames(EMENSAYO)

EMENSAYO %>% ungroup() %>%
  select(EM_17_POR_PRI, EM_17_POR_MORENA) %>%
  chart.Correlation(histogram = TRUE)

#Principales partidos de la elección 2012
#Principales partidos de la elección 2017

head(EMENSAYO)
class(as.character(EMENSAYO$NOM_MUN))

################################################################
#Partido que gana el municipio en el 2012
#Partido que gana el municipio en le 2017

summary(EMENSAYO$EM_12_Por_Part)
hist(EMENSAYO$EM_12_Por_Part, breaks = 35)

colnames(EMENSAYO)

lm(EM_17_POR_PART ~ DLNominal17 + EM_12_PRI_)

#económicas
EMENSAYO$Por_Ingreso_Gobierno
EMENSAYO$Por_Ingreso_otro_Pais
EMENSAYO$Por_Ingreso_del_Pais
EMENSAYO$Por_Poca_Variedad_Alimentos


#Por hacer
#Participacion 2012, 2017, proxy de ingreso, urbano o rural
#Partido que gobernaba
#den
#Regresión
#Variables relevantes


#elecciones competidas y no competidas
#los niveles y lo que se disputa en la elección

#impuestos
#correlaciones
#modelo de regresión



#ensayo
#mapas
