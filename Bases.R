library(tidyverse)
library(readxl)
library(stringr)
library(foreign)
library(data.table)
#Bases de datos
#Archivo para construir las bases de datos, se sustituye la inclusión de las bases en el cuerpo del documento con un
#archivo auxiliar para su construcción

#Bases electorales
#######################################################################################################
##########################    P2006 Secc    ###########################################################
#######################################################################################################

P2006Secc<-fread(input = "http://siceef.ine.mx/BD/Presidente2006Seccion.csv", 
                 sep = ",", encoding = "UTF-8")
P2006Secc$CVE_SECC <-str_c(str_pad(P2006Secc$ID_ESTADO, width =2, "left", "0"),         #Estado Sección
                           str_pad(P2006Secc$SECCION, width = 4, "left", "0"))
P2006Secc$CV_MUN   <- str_c(str_pad(P2006Secc$ID_ESTADO, width = 2, "left", "0"),       #Estado Municipio
                            str_pad(P2006Secc$ID_MUNICIPIO, width = 3, "left", "0"))
P2006Secc$CV_TODO  <- str_c(str_c(str_pad(P2006Secc$ID_ESTADO, width = 2, "left", "0"), #Estado, Municipio, Sección
                                  str_pad(P2006Secc$ID_MUNICIPIO, width = 3, "left", "0")), 
                            str_pad(P2006Secc$SECCION, width = 4, "left", "0"))
P2006Secc<-filter(P2006Secc, MUNICIPIO != "Voto en el Extranjero")
write.csv(x = P2006Secc, file = "Datos/Electorales/P2006Secc.csv")

####################      P2006 Mun      ##############################################################
P2006Mun<-P2006Secc %>% #2442 Municipios 
  group_by(CIRCUNSCRIPCION, ID_ESTADO, NOMBRE_ESTADO, ID_MUNICIPIO, MUNICIPIO) %>%
  summarise(TOTAL = sum(TOTAL_VOTOS, na.rm =TRUE),
            Lista_Nominal = sum (LISTA_NOMINAL, na.rm = TRUE),
            PAN = sum(PAN, na.rm = TRUE),
            APM = sum(APM, na.rm = TRUE),
            PBT = sum(PBT, na.rm = TRUE),
            NVA_ALIANZA   = sum(NVA_ALIANZA, na.rm = TRUE),
            ASDC          = sum(ASDC, na.rm = TRUE), 
            CANCELADOS    = sum(NUM_VOTOS_CAN_NREG, na.rm = TRUE),
            NULOS         = sum(NUM_VOTOS_NULOS, na.rm = TRUE),
            Num_Secciones = length(unique(na.omit(SECCION))),
            Num_Casillas  = length(unique(na.omit(CASILLA))),
            Municipio     = unique(MUNICIPIO), 
            ID_Municipio  = unique(ID_MUNICIPIO))
P2006Mun$CV_MUN <- str_c(str_pad(P2006Mun$ID_ESTADO, width = 2, "left", "0"),       #Estado Municipio
                            str_pad(P2006Mun$ID_MUNICIPIO, width = 3, "left", "0"))
P2006Mun$Por_Part   <- (P2006Mun$TOTAL / P2006Mun$Lista_Nominal) * 100 
P2006Mun$PAN_por    <- (P2006Mun$PAN/P2006Mun$TOTAL) * 100
P2006Mun$APM_por    <- (P2006Mun$APM/P2006Mun$TOTAL) * 100
P2006Mun$PBT_por    <- (P2006Mun$PBT/P2006Mun$TOTAL) * 100
P2006Mun$NVA_ALIANZA_por <- (P2006Mun$NVA_ALIANZA/P2006Mun$TOTAL) * 100
P2006Mun$ASDC_por   <- (P2006Mun$ASDC/P2006Mun$TOTAL) * 100
write.csv(x = P2006Mun, file = "Datos/Electorales/P2006Mun.csv")# N

##################################     P2006 Edo   ###################################################
P2006Edo<- P2006Mun %>% group_by(CIRCUNSCRIPCION, ID_ESTADO, NOMBRE_ESTADO) %>% 
  summarise(TOTAL = sum(TOTAL, na.rm = TRUE),
            Lista_Nominal = sum (Lista_Nominal, na.rm = TRUE),
            PAN           = sum(PAN, na.rm = TRUE),
            APM           = sum(APM, na.rm = TRUE),
            PBT           = sum(PBT, na.rm = TRUE),
            NVA_ALIANZA   = sum(NVA_ALIANZA, na.rm = TRUE),
            ASDC          = sum(ASDC, na.rm = TRUE),
            CANCELADOS    = sum(CANCELADOS, na.rm = TRUE),
            NULOS         = sum(NULOS, na.rm = TRUE),
            Num_Secciones = sum(Num_Secciones, na.rm = TRUE),
            Num_Casillas  = sum(Num_Casillas, na.rm = TRUE)
  )
P2006Edo$Por_Part <- (P2006Edo$TOTAL/P2006Edo$Lista_Nominal) * 100 #porcentajes. 
P2006Edo$PAN_por <- (P2006Edo$PAN/P2006Edo$TOTAL) * 100
P2006Edo$APM_por <- (P2006Edo$APM/P2006Edo$TOTAL) * 100
P2006Edo$PBT_por <- (P2006Edo$PBT/P2006Edo$TOTAL) * 100
P2006Edo$NVA_ALIANZA_por <- (P2006Edo$NVA_ALIANZA/P2006Edo$TOTAL) * 100
P2006Edo$ASDC_por <- (P2006Edo$ASDC/P2006Edo$TOTAL) * 100
write.csv(x = P2006Edo, file = "Datos/Electorales/P2006Edo.csv")

################################################################################################
#######################################       2012  SECC    ########################################
################################################################################################
P2012Secc<-fread(input = "http://siceef.ine.mx/BD/Presidente2012Seccion.csv", 
                 sep = ",", encoding = "Latin-1")
P2012Secc$CVE_SECC<-str_c(str_pad(P2012Secc$ID_ESTADO, width =2, "left", "0"),
                          str_pad(P2012Secc$SECCION, width = 4, "left", "0"))
P2012Secc$CV_MUN <- str_c(str_pad(P2012Secc$ID_ESTADO, width = 2, "left", "0"),
                          str_pad(P2012Secc$ID_MUNICIPIO, width = 3, "left", "0"))
P2012Secc$CV_TODO  <- str_c(str_c(str_pad(P2012Secc$ID_ESTADO, width = 2, "left", "0"), #Estado, Municipio, Sección
                                  str_pad(P2012Secc$ID_MUNICIPIO, width = 3, "left", "0")), 
                            str_pad(P2012Secc$SECCION, width = 4, "left", "0"))
write.csv(x = P2012Secc, file = "Datos/Electorales/P2012Secc.csv")# N
################################################################################################
#########################   2012 Municipal      ##################################################
################################################################################################
P2012Mun<-P2012Secc %>%   #2448
  group_by(NOMBRE_ESTADO, ID_ESTADO, MUNICIPIO, ID_MUNICIPIO, CVUN =as.factor(P2012Secc$CV_MUN)) %>%
  summarise(TOTAL = sum(TOTAL_VOTOS, na.rm =TRUE),
            Lista_Nominal = sum (LISTA_NOMINAL, na.rm = TRUE),
            PAN  = sum(PAN, na.rm = TRUE),
            PRI  = sum(PRI, na.rm = TRUE),
            PRD  = sum(PRD, na.rm = TRUE),
            PVEM = sum(PVEM, na.rm = TRUE),
            PT   = sum(PT, na.rm = TRUE),
            MC   = sum(MC, na.rm = TRUE),
            NVA_ALIANZA   = sum(NVA_ALIANZA, na.rm = TRUE),
            PRI_PVEM      = sum(PRI_PVEM, na.rm = TRUE),
            PRD_PT_MC     = sum(PRD_PT_MC, na.rm = TRUE),
            PRD_PT        = sum(PRD_PT, na.rm = TRUE),
            PRD_MC        = sum(PRD_MC, na.rm = TRUE),
            PT_MC         = sum(PT_MC, na.rm = TRUE),
            CANCELADOS    = sum(NUM_VOTOS_CAN_NREG, na.rm = TRUE),
            NULOS         = sum(NUM_VOTOS_NULOS, na.rm = TRUE),
            Num_Secciones = length(unique(na.omit(SECCION))),
            Num_Casillas  = length(unique(na.omit(CASILLAS))))
P2012Mun$Por_Part   <- (P2012Mun$TOTAL / P2012Mun$Lista_Nominal) * 100  
P2012Mun$PAN_por         <- (P2012Mun$PAN/P2012Mun$TOTAL)  * 100
P2012Mun$PRI_por         <- (P2012Mun$PRI/P2012Mun$TOTAL)  * 100
P2012Mun$PRD_por         <- (P2012Mun$PRD/P2012Mun$TOTAL)  * 100
P2012Mun$PVEM_por        <- (P2012Mun$PVEM/P2012Mun$TOTAL) * 100
P2012Mun$PT_por          <- (P2012Mun$PT/P2012Mun$TOTAL)   * 100
P2012Mun$MC_por          <- (P2012Mun$MC/P2012Mun$TOTAL)   * 100
P2012Mun$NVA_ALIANZA_por <- (P2012Mun$NVA_ALIANZA/P2012Mun$TOTAL)   * 100
P2012Mun$PRI_PVEM_por    <- (P2012Mun$PRI_PVEM/P2012Mun$TOTAL)   * 100
P2012Mun$PRD_PT_MC_por   <- (P2012Mun$PRD_PT_MC/P2012Mun$TOTAL)   * 100
P2012Mun$PRD_PT_por      <- (P2012Mun$PRD_PT/P2012Mun$TOTAL)   * 100
P2012Mun$PRD_MC_por      <- (P2012Mun$PRD_MC/P2012Mun$TOTAL)   * 100
P2012Mun$PT_MC_por       <- (P2012Mun$PT_MC/P2012Mun$TOTAL)   * 100
write.csv(x = P2012Mun, file = "Datos/Electorales/P2012Mun.csv")# N

###########################   Datos 2012 Estatal  #########################################
P2012Edo<- P2012Mun %>% group_by(NOMBRE_ESTADO, ID_ESTADO) %>% 
  summarise(TOTAL = sum(TOTAL, na.rm = TRUE),
            Lista_Nominal = sum (Lista_Nominal, na.rm = TRUE),
            PAN           = sum(PAN, na.rm = TRUE),
            PRI           = sum(PRI, na.rm = TRUE),
            PRD           = sum(PRD, na.rm = TRUE),
            PVEM          = sum(PVEM, na.rm = TRUE),
            PT            = sum(PT, na.rm = TRUE),
            MC            = sum(MC, na.rm = TRUE),
            NVA_ALIANZA   = sum(NVA_ALIANZA, na.rm = TRUE),
            PRI_PVEM      = sum(PRI_PVEM, na.rm = TRUE),
            PRD_PT_MC     = sum(PRD_PT_MC, na.rm = TRUE), 
            PRD_PT        = sum(PRD_PT, na.rm = TRUE),
            PRD_MC        = sum(PRD_MC, na.rm = TRUE),
            CANCELADOS    = sum(CANCELADOS, na.rm = TRUE),
            NULOS         = sum(NULOS, na.rm = TRUE),
            Num_Secciones = sum(Num_Secciones, na.rm = TRUE),
            Num_Casillas  = sum(Num_Casillas, na.rm = TRUE),
            Num_Municipios = length(unique(na.omit(MUNICIPIO))))
P2012Edo$Por_Part <- (P2012Edo$TOTAL/P2012Edo$Lista_Nominal) * 100 #porcentajes. 
P2012Edo$PAN_por  <- (P2012Edo$PAN/P2012Edo$TOTAL) * 100
P2012Edo$PRI_por  <- (P2012Edo$PRI/P2012Edo$TOTAL) * 100
P2012Edo$PRD_por  <- (P2012Edo$PRD/P2012Edo$TOTAL) * 100
P2012Edo$PVEM_por <- (P2012Edo$PVEM/P2012Edo$TOTAL) * 100
P2012Edo$PT_por   <- (P2012Edo$PT/P2012Edo$TOTAL) * 100
P2012Edo$MC_por   <- (P2012Edo$MC/P2012Edo$TOTAL) * 100
P2012Edo$NVA_ALIANZA_por  <- (P2012Edo$NVA_ALIANZA/P2012Edo$TOTAL) * 100
P2012Edo$PRI_PVEM_por     <- (P2012Edo$PRI_PVEM/P2012Edo$TOTAL) * 100
P2012Edo$PRD_PT_MC_por    <- (P2012Edo$PRD_PT_MC/P2012Edo$TOTAL) * 100
P2012Edo$PRD_PT_por       <- (P2012Edo$PRD_PT/P2012Edo$TOTAL) * 100
P2012Edo$PRD_MC_por       <- (P2012Edo$PRD_MC/P2012Edo$TOTAL) * 100
write.csv(x = P2012Edo, file = "Datos/Electorales/P2012Edo.csv")# N

#######################################################################################################
################### M2016 Jalisco     #################################################################
#######################################################################################################
#125 ayuntamientos
# 39 diputados al congreso del estado, 20 de mayoría relativa por cada uno de los distritos electorales

JAL2015 <- read_xlsx(path = "C:/Proyectos R/Datos-Electorales/2015 Guadalajara/ResultadosPorCasilla2015.xlsx", 
                     range = "A4:AD18734")
colnames(JAL2015) <- make.names(colnames(JAL2015))
JALSECC2015 <- JAL2015 %>%
  group_by(Municipio, Sección, Elección) %>%
  summarise(PAN = sum(PAN), 
            PRI = sum(PRI), 
            PRD = sum(PRD), 
            PT = sum(PT), 
            PVEM =sum(PVEM), 
            MC = sum(MC),
            N.A. = sum (NA.), 
            MORENA = sum(MORENA),
            JPK = sum(JOSE.PEDRO.KUMAMOTO.AGUILAR), 
            Boletas = sum(Boletas),
            VotosTotales = sum(VotosTotales), 
            VotosValidos = sum(VotosVálidos))
JALSECC2015$ANO <- 2015

JALSECC2015<- JALSECC2015 %>% 
  mutate(Por_Part = (VotosTotales/Boletas)*100,
         PAN_por    = (PAN/VotosValidos)*100,
         PRI_por    = (PRI/VotosValidos)*100,
         PRD_por    = (PRD/VotosValidos)*100,
         PT_por     = (PT/VotosValidos)*100,
         PVEM_por   = (PVEM/VotosValidos)*100,
         MC_por     = (MC/VotosValidos)*100,
         MORENA_por = (MORENA/VotosValidos)*100,
         JPK_por    = (JPK/VotosValidos)*100)
JALSECCMUN2015 <- JALSECC2015 %>% filter(Elección == "Municipes") 
JALSECCDIP2015 <- JALSECC2015 %>% filter(Elección == "Diputados MR")

###################################         Jalisco a Nivel Municipal       ################################
#JALSECCMUN2015 <-fread(file = "Datos/Electorales/Jalisco/JALSECCMUN2015.csv")
#hay 10 secciones electorales, en el municipio de Guadalajara con nas en Votos Validos
#Asignamos Votos Totales a los votos validos
JALSECCMUN2015[is.na(JALSECCMUN2015$VotosValidos),]$VotosValidos <-JALSECCMUN2015[is.na(JALSECCMUN2015$VotosValidos),]$VotosTotales

JALMUNMUN2015 <- JALSECCMUN2015 %>%      #municipes en municipios, 125 Municipios
  group_by(Municipio) %>%
  summarise(PAN = sum(PAN), 
            PRI = sum(PRI), 
            PRD = sum(PRD), 
            PT = sum(PT), 
            PVEM =sum(PVEM), 
            MC = sum(MC),
            N.A. = sum (N.A.), 
            MORENA = sum(MORENA),
            JPK = sum(JPK), 
            Boletas = sum(Boletas),
            VotosTotales = sum(VotosTotales), 
            VotosValidos = sum(VotosValidos))
JALMUNMUN2015<- JALMUNMUN2015 %>% 
  mutate(Por_Part  = (VotosTotales/Boletas)*100,
         PAN_por    = (PAN/VotosValidos)*100,
         PRI_por    = (PRI/VotosValidos)*100,
         PRD_por    = (PRD/VotosValidos)*100,
         PT_por     = (PT/VotosValidos)*100,
         PVEM_por   = (PVEM/VotosValidos)*100,
         MC_por     = (MC/VotosValidos)*100,
         NA_por     = (N.A./VotosValidos)*100, 
         MORENA_por = (MORENA/VotosValidos)*100,
         JPK_por    = (JPK/VotosValidos)*100, 
         Boletas_Por= (Boletas/sum(Boletas)*100),#porcentaje de la lista nominal que tiene cada municipio
         Validos_Por= (VotosValidos/ sum(VotosValidos)*100))#porcentaje de la votación efectiva de cada municipio 
head(JALMUNMUN2015)

write.csv(x = JALMUNMUN2015, file = "Datos/Electorales/Jalisco/JALMUNMUN2015.csv")
write.csv(x = JALSECCMUN2015, file = "Datos/Electorales/Jalisco/JALSECCMUN2015.csv")
write.csv(x = JALSECCDIP2015, file = "Datos/Electorales/Jalisco/JALSECCDIP2015.csv")

#####################################################################################################
##############      ENOE a nivel municipal      ####################################################
#####################################################################################################
# De sdem105.dbf a sdem416.dbf
ENOE <- read.dbf(file = "Datos/ENOE/Sociodemografico/sdemt317.dbf") #  3er trimestre 2017 
ENOE <- filter(ENOE, R_DEF == "00") #00 Resultado definitivo de la entrevista, entrevista completa
ENOE$EDA<-as.numeric(ENOE$EDA)
ENOE <- filter(ENOE, EDA >= 14)
ENOE <- filter(ENOE, EDA <=98)
ENOE <- filter(ENOE, C_RES == 1 | C_RES == 3) #Condiciónn de la residencia, 1 Residente Habitual, 3 Nuevo residente 
# Nuevas variables
ENOE<- mutate(ENOE, INF = ifelse(TUE2==5,1,0))
ENOE<- mutate(ENOE, INF_POB = INF * FAC)
ENOE<- mutate(ENOE, Estado = as.factor(ENT))
#nombres de los Estados
ENOE$Estado<- factor(ENOE$Estado, labels = c("AGS", "BC", "BCS","CAMP", "COAH", "COL", "CHIS", "CHIH", "DF", "DGO", "GTO","GRO", "HGO", "JAL", "MEX","MICH", "MOR", "NAY", "NL", "OAX","PUE", "QRO", "QR", "SLP", "SIN", "SON", "TAB", "TAMS", "TLAX", "VER", "YUC", "ZAC"))
ENOE$SEX<- factor(ENOE$SEX, labels = c("HOMBRE","MUJER")) #Sexo
ENOE$TUE2<- factor(ENOE$TUE2, labels = c("No Clasif","EmpSocCop","Neg. noConst", "Priv", "Pub", "Sect. Informal", "TDR", "AgSubs")) 
ENOE$RAMA_EST2<- factor(ENOE$RAMA_EST2, labels = c("No Clasif","Agricultura GSCP","Extractiva y Electricidad", "Manufactura", "Construcción", "Comercio", "Restaurantes y ServAloj", "Transportes y Com...", "Serv. Prof", "Serv. Sociales", "Sevr. Diversos", "Gobierno y Org. Int.")) 
ENOE$E_CON <- factor(ENOE$E_CON, labels = c ("Unión libre", "Separado(a)", "Divorciado(a)", "Viudo(a)", "Casado(a)", "Soltero(a)", "No sabe"))
ENOE$CLASE1 <- factor(ENOE$CLASE1, labels = c( "PEA", "No PEA" ))
ENOE$ING7C<- factor(ENOE$ING7C, labels =c("No Clas","Hasta 1 SM", "1-2 SM", "2-3 SM", "3-5 SM", "Más de 5 SM", "No recibe ingresos", "No especificado"))  #clasificación de la población ocupada por nivel de ingreso 
ENOE$MEDICA5C <- factor(ENOE$MEDICA5C, labels = c("No Clas", "Sin prestaciones", "Solo acceso a instituciones de salud", "Acceso a inst. de salud y otras prest.", "no tiene acceso, pero si otras prestaciones", "No especificado" ))
ENOE$AMBITO2 <- factor (ENOE$AMBITO2, labels = c("no clas", "Sin Establecimiento" , "Con establecimiento", "Pequeños establecimientos" , "Medianos Establecimientos", "Grandes Establecimientos", "Gobierno", "Otros") )
ENOE<- mutate(ENOE, Mujer = ifelse(SEX == "MUJER",1,0)) #Mujeres
ENOE<- mutate(ENOE, Mujer_Pob = FAC * Mujer) #Mujeres multiplicadas por el factor de poblacion
ENOE<- mutate(ENOE, Hombre = ifelse(SEX == "HOMBRE",1,0)) #Hombres
ENOE<- mutate(ENOE, Hombre_Pob = FAC * Hombre) #Hombres multiplicados por el factor de poblacion
ENOE<- mutate(ENOE, ING_Pob_HR = ING_X_HRS * FAC)
ENOE<- mutate(ENOE, OCUPADA = ifelse(CLASE2 ==1,1,0)) #Poblacion ocupada, para sacar ingresos promedio
ENOE<- mutate(ENOE, OCUPADA_Pob = OCUPADA * FAC) #Poblacion ocupada, para sacar ingresos promedio
ENOE<- mutate(ENOE, HRSOCUP_Pob = HRSOCUP * FAC)
ENOE<- mutate(ENOE, ANIOS_ESC_Pob = ANIOS_ESC * FAC)
ENOE<- mutate(ENOE, SUB_O_POB = SUB_O * FAC)
#Codificación de las variables de escolaridad
ENOE$CS_P13_1<- factor(ENOE$CS_P13_1, labels = c("Ninguna", "Preescolar", "Primaria", "Secundaria","Preparatoria o bach", "Normal", "Carrera técnica", "Profesional", "Maestría", "Doctorado", "No sabe"))
#ENOE$NS<-ENOE$NS * ENOE$FAC
write.csv(x = ENOE,file = "Datos/ENOE/Sociodemografico/Formateados CSV/Sdem317.csv")

#####################################################################################################
##############      Intercensal, Jalisco     ########################################################
#####################################################################################################
#la codificación de las etiquetas se obtiene del cuestionario 
#http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/especiales/intercensal/2015/doc/eic2015_cuestionario.pdf
##############    Vivienda    #############################################
ViviendaICJal     <- read.csv(file = "C:/Proyectos R/Datos intercensal/Datos Intercensal/TR_VIVIENDA14.CSV")#Vivienda intercensal, jalisco 
ViviendaICEdoMex     <- read.csv(file = "C:/Proyectos R/Datos intercensal/Datos Intercensal/TR_VIVIENDA15.CSV")#Vivienda intercensal, Edomex 
ViviendaIC <- bind_rows(ViviendaICJal, ViviendaICEdoMex) #Eventualmente para todos los estados programáticamente
rm(ViviendaICJal)
rm(ViviendaICEdoMex)
ViviendaIC$JEFE_SEXO<-factor(ViviendaIC$JEFE_SEXO, labels = c("Hombre", "Mujer"))
ViviendaIC[ViviendaIC$JEFE_EDAD == "999", ]$JEFE_EDAD <- NA#clasificar correctamente los NAs de edad del jefe del hogar

#jefatura de hombre y jefatura de mujer, % jefatura de hombre
head(ViviendaIC)
ViviendaIC %>% group_by(ENT, MUN, NOM_MUN)







temp <- ViviendaIC %>% 
  filter(JEFE_EDAD != "NA") %>% 
  group_by(MUN, NOM_MUN, JEFE_SEXO ) %>% 
  summarise(EDAD_JEFE_PROMEDIO = weighted.mean(JEFE_EDAD, FACTOR)) #para sacar las promedios ponderados, corte municipio y sexo 

temp <- spread(temp, JEFE_SEXO, EDAD_JEFE_PROMEDIO)[,c(2:4)]

ICMunViv <-  

ViviendaIC %>% filter(INGTRHOG != c("NA", "999999")) %>% group_by(MUN, NOM_MUN, JEFE_SEXO ) %>% summarise(INGRESO_VIVIENDA_PROMEDIO = weighted.mean(JEFE_EDAD, FACTOR), weighted.mean(Ingreso_Hogar, FACTOR)) #para sacar las promedios ponderados, corte municipio y sexo 





#1. Dos bases, personas y vivienda, sacar información a nivel municipal
#2. Unir datos de las dos bases
write.csv(x = ViviendaIC, file = "Datos/Intercensal/Vivienda_2015.csv")# vivienda intercensal 

#sacarlo para Jalisco, municipal, homologarlo con shapefiles. 
PersonaICJal      <- read.csv(file = "C:/Proyectos R/Datos intercensal/Datos Intercensal/TR_PERSONA14.CSV")#persona intercensal, jalisco
PersonaICEdoMex      <- read.csv(file = "C:/Proyectos R/Datos intercensal/Datos Intercensal/TR_PERSONA15.CSV")#persona intercensal, jalisco 
PersonaIC$SEXO <-factor(PersonaIC$SEXO, labels = c("Hombre", "Mujer"))
write.csv(x = PersonaIC, file = "Datos/Intercensal/Persona_2015.csv")# Persona intercensal 


##########################################################################
################      Bases de construcción cartográfica      ############
##########################################################################
Municipios <-readOGR("C:/Proyectos R/Analisis Electoral Puebla/Datos/Cartográficos/Municipios Nacional/areas_geoestadisticas_municipales.shp", "areas_geoestadisticas_municipales", encoding = "UTF8") #cartografía de Jalisco 14
MunMapJal15 <- Municipios[Municipios@data$CVE_ENT == "14",] #Subseteo de los datos geográficos del estado que nos interesa 
rm(Municipios)#WastenotWantNot RAM
MunMapJal<-tidy(x = MunMapJal15, region = "NOM_MUN")
MunMapJal$NOM_MUN <- MunMapJal$id #probando unirlos por nombre, mayor certeza de que está bien
#el paso siguiente se hace para tener CVE_MUN
#unión de cartografía con los datos electorales 
MunMapJal2015 <- left_join(MunMapJal, MunMapJal15@data, by = "NOM_MUN") #notar el @data, MunMapJal15 no es data.frame 
#añadir datos de resultados electorales Municipales
#revisar que empaten nombres de municipios
JALMUNMUN2015$NOM_MUN <- JALMUNMUN2015$Municipio
#Código para encontrar al que hay que renombrar
#intersect(chartr('áéíóúñ','aeioun',unique(tolower(JALMUNMUN2015$NOM_MUN))),
#          chartr('áéíóúñ','aeioun',unique(tolower(MunMapJal$NOM_MUN))))#124 aparecen en ambos
#setdiff(chartr('áéíóúñ','aeioun',unique(tolower(JALMUNMUN2015$NOM_MUN))),
#        intersect(chartr('áéíóúñ','aeioun',unique(tolower(JALMUNMUN2015$NOM_MUN))),
#                  chartr('áéíóúñ','aeioun',unique(tolower(MunMapJal$NOM_MUN)))))
#"manzanilla de la paz"
#setdiff(chartr('áéíóúñ','aeioun',unique(tolower(MunMapJal$NOM_MUN))),
#        intersect(chartr('áéíóúñ','aeioun',unique(tolower(JALMUNMUN2015$NOM_MUN))),
#                  chartr('áéíóúñ','aeioun',unique(tolower(MunMapJal$NOM_MUN)))))
#"la manzanilla de la paz"
#generar las variables de nombres unificados, en minúsculas sin acentos y un nombre que hay que asignar a mano
JALMUNMUN2015$NOM_MUN_JOIN <- chartr('áéíóúñ','aeioun',tolower(JALMUNMUN2015$NOM_MUN))
JALMUNMUN2015[JALMUNMUN2015$NOM_MUN_JOIN == "manzanilla de la paz",]$NOM_MUN_JOIN <- "la manzanilla de la paz"
MunMapJal$NOM_MUN_JOIN<- chartr('áéíóúñ','aeioun',tolower(MunMapJal$NOM_MUN))
##chequemos 
#intersect(unique(JALMUNMUN2015$NOM_MUN_JOIN),
#          unique(MunMapJal$NOM_MUN_JOIN))#125 de lujoso lujo 
##Ahora juntemos los datos electorales con los datos geográficos
MunMapJal2015<-left_join(x = MunMapJal, y = JALMUNMUN2015, by = "NOM_MUN_JOIN")#a la izquierda van el data set más grande, datos geográficos
#guardemos la base para después invocarla en el código principal 
write.csv(x = MunMapJal2015, file = "Datos/Electorales/Jalisco/MunMapJal2015.csv")
write.csv(x = JALMUNMUN2015, file = "Datos/Electorales/Jalisco/JALMUNMUN2015.csv")

#para mujeres por rangos de edad
############################################################################################
##########################    Base para Mapas, Intercensal 2015   #########################
############################################################################################
#unir información municipal de Jalisco (fuente intercensal) don datos electorales 
####    Pirámide Poblacional ####
PPJal <- PersonaIC %>% group_by(SEXO) %>% count(vars = EDAD, wt = FACTOR) 
#suma del tiempo dedicado a actividades sin pago 
colnames(PPJal) <- c('Sexo', "Edad", "Pob")
PPJal <- PPJal[ PPJal$Edad < 111,] #quitamos unos  NA's codificados como 999 
#elaboramos las categorias de edad 
PPJal<-mutate(PPJal, Edad_grupo = 
         ifelse(Edad <= 4, "0-04", 
                ifelse(Edad <= 9, "05-09",
                       ifelse(Edad <= 14, "10-14",
                              ifelse(Edad <= 19, "15-19",
                                     ifelse(Edad <= 24, "20-24",
                                            ifelse(Edad <= 29, "25-29",
                                                   ifelse(Edad <= 34, "30-34",
                                                          ifelse(Edad <= 39, "35-39",
                                                                 ifelse(Edad <= 44, "40-44",
                                                                        ifelse(Edad <= 49, "45-49",
                                                                               ifelse(Edad <= 54, "50-54",
                                                                                      ifelse(Edad <= 59, "55-59",
                                                                                             ifelse(Edad <=64, "60-64",
                                                                                                    ifelse(Edad <= 69, "65-69",
                                                                                                           ifelse(Edad <=74, "70-74",
                                                                                                                  ifelse(Edad <= 79, "75-79", 
                                                                                                                         ifelse(Edad <= 84, "80-84",
                                                                                                                                ifelse(Edad <= 89, "85-89",
                                                                                                                                       ifelse(Edad <= 94, "90-94","95+"))))))))))))))))))))
write.csv(PPJal, file = "Datos/Intercensal/Piramide_Jalisco_2015.csv") 
#piramide poblacional 
#####################
sum(PersonaIC$FACTOR) #población de Jalisco
PersonaIC %>% filter(SEXO == "Mujer") %>% select(FACTOR) %>% sum() #Mujeres de Jalisco
PersonaIC %>% filter(SEXO == "Hombre") %>% select(FACTOR) %>% sum() #Hombres de Jalisco
#######   Base de vivienda, variables a nivel municipal 
#Ingresos por jefatura de hogar
ggplot(ViviendaIC, aes(JEFE_EDAD, INGTRHOG, fill = JEFE_SEXO)) + geom_smooth(aes(weight = FACTOR)) 
ENOEB %>%
  filter(CS_P13_1 == c("Ninguna", "Primaria", "Secundaria", "Preparatoria o bach", "Carrera técnica", "Profesional"))
IngresoOcup <- ggplot(filter(.data = ENOEB, CS_P13_1 == c("Ninguna", "Primaria", "Secundaria", "Preparatoria o bach", "Carrera técnica", "Profesional")), 
                      aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, todas las escolaridades 
IngresoOcup <- IngresoOcup + geom_smooth(aes(weight = FAC))
IngresoOcup <- IngresoOcup + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual")  + ggtitle("Ingreso Mensual declarado por  Sexo(ENOE, II 2015) ")
IngresoOcup + facet_wrap(~Estado) #Por Estado 
IngresoOcup + facet_wrap(~CS_P13_1) + geom_jitter(alpha = 0.01) + coord_cartesian( xlim = c(0, 75), ylim = c(0, 25000))#

#tabla con resultados agregados por estado, participación electoral y principales partidos
##### Padrón  ######
PADRON2017    <- read_excel(path = "C:/Users/Franco/Desktop/DatosAbiertos-DERFE-pl_20170731.xlsx")
####################
##### EDOMEX MUN 2017 ##############
#EDOMEX2017 <- read_excel(path = "C:/Users/Franco/Desktop/Resultados_computo_de_Gobernador_2017_por_casilla.xlsx") #laptop
EDOMEX2017 <- read_excel(path = "Datos/Electorales/Edomex/Resultados_computo_de_Gobernador_2017_por_casilla.xlsx") #mounstruo
EDOMEXMUN2017 <- EDOMEX2017 %>% group_by(ID_ESTADO, ID_MUNICIPIO, MUNICIPIO) %>% 
  summarise(PRI = sum(PRI), 
            PAN           = sum(PAN), 
            PRD           = sum(PRD),
            MORENA        = sum(MORENA),
            LISTA_NOMINAL = sum(LISTA_NOMINAL),
            TOTAL_VOTOS   = sum(TOTAL_VOTOS),
            POR_PART = sum(TOTAL_VOTOS)/sum(LISTA_NOMINAL))
EDOMEXMUN2017$POR_PRI <- (EDOMEXMUN2017$PRI/EDOMEXMUN2017$TOTAL_VOTOS) * 100
write.csv(x = EDOMEXMUN2017, file = "Datos/Electorales/Edomex/EDOMEXMUN2017.csv")
###########################     Merge de bases    ########################
#JALMUNMUN2015 <- read.csv(file = "Electorales/Jalisco/JALMUNMUN2015.csv") #laptop
JALMUNMUN2015 <- read.csv(file = "Datos/Electorales/Jalisco/JALMUNMUN2015.csv") #mounstruo
#homologar variable de join
P2012JalMun <- P2012Mun %>% 
  filter(NOMBRE_ESTADO =="JALISCO")
JALMUNMUN2015$NOM_MUN_JOIN   <- chartr('áéíóúñ','aeioun',unique(tolower(JALMUNMUN2015$Municipio)))
P2012JalMun$NOM_MUN_JOIN <-chartr('áéíóúñ','aeioun',unique(tolower(P2012JalMun$MUNICIPIO)))
#####
intersect(chartr('áéíóúñ','aeioun',unique(tolower(JALMUNMUN2015$NOM_MUN_JOIN))),
          chartr('áéíóúñ','aeioun',unique(tolower(P2012JalMun$NOM_MUN_JOIN))))#124aparecen en ambos
#"manzanilla de la paz", "san pedro tlaquepaque
#generar las variables de nombres unificados, en minúsculas sin acentos y un nombre que hay que asignar a mano
JALMUNMUN2015[JALMUNMUN2015$NOM_MUN_JOIN == "manzanilla de la paz",]$NOM_MUN_JOIN <- "la manzanilla de la paz"
P2012JalMun[P2012JalMun$NOM_MUN_JOIN == "tlaquepaque",]$NOM_MUN_JOIN <- "san pedro tlaquepaque"
#Juntar utilizando NOM_MUN_JOIN
intersect(unique(JALMUNMUN2015$NOM_MUN_JOIN),
          unique(P2012JalMun$NOM_MUN_JOIN))#125 de lujoso lujo 
#nombres para distinguir 
colnames(JALMUNMUN2015) <- str_c("JAL15",  colnames(JALMUNMUN2015), sep = "_")
colnames(P2012JalMun)   <- str_c("PRES12", colnames(P2012JalMun), sep = "_" )
colnames(JALMUNMUN2015)[26] <- "NOM_MUN_JOIN"
colnames(P2012JalMun)[37]   <- "NOM_MUN_JOIN"
####    Guardar
JalEl<-inner_join(as.data.frame(P2012JalMun), as.data.frame(JALMUNMUN2015), by = "NOM_MUN_JOIN")
write.csv(x = JalEl, file = "Datos/Electorales/Jalisco/JalEl1215.csv")
########    EDOMEX    ##########################################
#EDOMEXMUN2017
EDOMEXMUN2017 <- read.csv(file = "Datos/Electorales/Edomex/EDOMEXMUN2017.csv")

#homologar variable de join
P2012EdomexMun <- P2012Mun %>% 
  filter(NOMBRE_ESTADO =="MEXICO")
EDOMEXMUN2017$NOM_MUN_JOIN   <- chartr('áéíóúñ','aeioun',unique(tolower(EDOMEXMUN2017$MUNICIPIO)))
P2012EdomexMun$NOM_MUN_JOIN  <- chartr('áéíóúñ','aeioun',unique(tolower(P2012EdomexMun$MUNICIPIO)))
#####
intersect(EDOMEXMUN2017$NOM_MUN_JOIN, P2012EdomexMun$NOM_MUN_JOIN) #121
setdiff(P2012EdomexMun$NOM_MUN_JOIN, EDOMEXMUN2017$NOM_MUN_JOIN) #aparecen en presidencial pero no en municipal
#"coacalco de berriozabal" "ecatepec de morelos"     "naucalpan de juarez"     "tlalnepantla de baz" 
setdiff(EDOMEXMUN2017$NOM_MUN_JOIN, P2012EdomexMun$NOM_MUN_JOIN) # para ver los nombres en base 2017
#"coacalco"     "ecatepec"     "naucalpan"    "tlalnepantla"
#asignar nombres a la 2017 para homologar con presidencial 2012 
EDOMEXMUN2017[EDOMEXMUN2017$NOM_MUN_JOIN == "coacalco",]$NOM_MUN_JOIN     <- "coacalco de berriozabal"
EDOMEXMUN2017[EDOMEXMUN2017$NOM_MUN_JOIN == "ecatepec",]$NOM_MUN_JOIN     <- "ecatepec de morelos"
EDOMEXMUN2017[EDOMEXMUN2017$NOM_MUN_JOIN == "naucalpan",]$NOM_MUN_JOIN    <- "naucalpan de juarez"
EDOMEXMUN2017[EDOMEXMUN2017$NOM_MUN_JOIN == "tlalnepantla",]$NOM_MUN_JOIN <- "tlalnepantla de baz"
intersect(EDOMEXMUN2017$NOM_MUN_JOIN, P2012EdomexMun$NOM_MUN_JOIN) #125
#nombres para distinguir 
colnames(EDOMEXMUN2017)    <- str_c("EM_17",  colnames(EDOMEXMUN2017), sep = "_")
colnames(P2012EdomexMun)   <- str_c("EM_12", colnames(P2012EdomexMun), sep = "_" )
colnames(EDOMEXMUN2017)[13] <- "NOM_MUN_JOIN"
colnames(P2012EdomexMun)[37]   <- "NOM_MUN_JOIN"
EdoMex1215<-inner_join(EDOMEXMUN2017, P2012EdomexMun, by = "NOM_MUN_JOIN")
write.csv(x = EdoMex1215, file = "Datos/Electorales/Edomex/EdoMex1215.csv")

############################################################################
########    Juntar el resto de las bases a nivel municipal  ################
############################################################################

#Intercensal Vivienda
ICV2015 <- read.csv(file = "Datos/Intercensal/Vivienda_2015.csv")

head(ICV2015)
table(ICV2015$NOM_ENT)


write.csv(x = ENOE,file = "Datos/ENOE/Sociodemografico/Formateados CSV/Sdem317.csv")
write.csv(x = PersonaIC, file = "Datos/Intercensal/Persona_2015.csv")# Persona intercensal 
write.csv(x = ViviendaIC, file = "Datos/Intercensal/Vivienda_2015.csv")# vivienda intercensal 




