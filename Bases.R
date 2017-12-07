library(tidyverse)
library(readxl)
library(stringr)
#Bases de datos
#Archivo para construir las bases de datos, se sustituye la inclusión de las bases en el cuerpo del documento con un
#archivo auxiliar para su construcción

#Bases electorales

##########################    P2006 Secc    ###########################################################

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

write.csv(x = P2006Secc, file = "Datos/Electorales/P2006Secc.csv")# N

####################      P2006 Mun      ##############################################################

P2006Mun<-P2006Secc %>% #2442 Municipios 
  group_by(CVUN =as.factor(P2006Secc$CV_MUN)) %>%
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
P2006Mun$CVE_ENT    <- str_sub(string = P2006Mun$CVUN, start = 1, end = 2)
P2006Mun$Por_Part   <- (P2006Mun$TOTAL / P2006Mun$Lista_Nominal) * 100 
P2006Mun$PAN_por    <- (P2006Mun$PAN/P2006Mun$TOTAL) * 100
P2006Mun$APM_por    <- (P2006Mun$APM/P2006Mun$TOTAL) * 100
P2006Mun$PBT_por    <- (P2006Mun$PBT/P2006Mun$TOTAL) * 100
P2006Mun$NVA_ALIANZA_por <- (P2006Mun$NVA_ALIANZA/P2006Mun$TOTAL) * 100
P2006Mun$ASDC_por   <- (P2006Mun$ASDC/P2006Mun$TOTAL) * 100

write.csv(x = P2006Mun, file = "Datos/Electorales/P2006Mun.csv")# N

##################################     P2006 Edo   ###################################################

P2006Edo<- P2006Mun %>% group_by(CVE_ENT = as.factor(CVE_ENT)) %>% 
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

######## P2012 Secc
######## P2012 Mun
######## P2012 Edo
######## G2016 Pue

################### M2016 Jalisco     #################################################################
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
JALSECC2015<- JALSECC2015 %>% mutate(Por_Part <- (VotosTotales/Boletas),
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

write.csv(x = JALSECCMUN2015, file = "Datos/Electorales/Jalisco/JALSECCMUN2015.csv")
write.csv(x = JALSECCDIP2015, file = "Datos/Electorales/Jalisco/JALSECCDIP2015.csv")

#Bases de construcción cartográfica

