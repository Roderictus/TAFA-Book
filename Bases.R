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

####################      P2006 Mun      #############################################################
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
            PRI  = sum(PRI, na.rm = TRUE),#este
            PRD  = sum(PRD, na.rm = TRUE),
            PVEM = sum(PVEM, na.rm = TRUE),
            PT   = sum(PT, na.rm = TRUE),
            MC   = sum(MC, na.rm = TRUE),
            NVA_ALIANZA   = sum(NVA_ALIANZA, na.rm = TRUE),
            PRI_PVEM      = sum(PRI_PVEM, na.rm = TRUE),#este
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
P2012Mun$PRI_por         <- (P2012Mun$PRI/P2012Mun$TOTAL)  * 100          #Alianza PRI
P2012Mun$PRD_por         <- (P2012Mun$PRD/P2012Mun$TOTAL)  * 100
P2012Mun$PVEM_por        <- (P2012Mun$PVEM/P2012Mun$TOTAL) * 100
P2012Mun$PT_por          <- (P2012Mun$PT/P2012Mun$TOTAL)   * 100
P2012Mun$MC_por          <- (P2012Mun$MC/P2012Mun$TOTAL)   * 100
P2012Mun$NVA_ALIANZA_por <- (P2012Mun$NVA_ALIANZA/P2012Mun$TOTAL)   * 100
P2012Mun$PRI_PVEM_por    <- (P2012Mun$PRI_PVEM/P2012Mun$TOTAL)   * 100    #Alianza PRI
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

P2012Edo <-read.csv(file = "Datos/Electorales/P2012Edo.csv")


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
ENOE <- filter(ENOE, EDA >= 18) #sólo para mayores de 18 años
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
ENOE <- read.csv("Datos/ENOE/Sociodemografico/Formateados CSV/Sdem317.csv")
ENOE <- ENOE %>% filter(Estado %in% c("JAL", "MEX"))#10998 y 14313 casos Jalisco y Mex

#Clase1 1:PEA y 2:PNEA
#Clase2 1: Población Ocupada, 2: Población desocupada, 3: Disponibles, 4:  No Disponibles
#POS_OCU 1: Trabajadores subordinados y remunerados, 2. Empleadores, 3. Trabajadores por cuenta propia

#hombres y mujeres por municipio
#escolaridad

#ordenar a nivel municipal
#####################################################################################################
##############    Base Intercensal, Nacional      ########################################################
#####################################################################################################
#la codificación de las etiquetas se obtiene del cuestionario 
#http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/especiales/intercensal/2015/doc/eic2015_cuestionario.pdf
##############    Vivienda    #############################################

ViviendaIC$JEFE_SEXO<-factor(ViviendaIC$JEFE_SEXO, labels = c("Hombre", "Mujer"))
ViviendaIC[ViviendaIC$JEFE_EDAD == "999", ]$JEFE_EDAD <- NA#clasificar correctamente los NAs de edad del jefe del hogar

head(ViviendaIC)
A<-data.frame()
for(i in 1:32){
  print(i)
  df <- read.csv(file = str_c("C:/Proyectos R/Datos intercensal/Datos Intercensal/TR_VIVIENDA",  str_pad(i, 2, "left", "0"), ".CSV"))
  IC_Municipio <- df %>%
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
           Por_Renta = (Renta/Viviendas) * 100)
        A<-bind_rows(A, IC_Municipio)
}

#########       Variables econónomicas    #######################################

B<-data.frame()
for(i in 1:32){
  print(i)
  df <- read.csv(file = str_c("C:/Proyectos R/Datos intercensal/Datos Intercensal/TR_VIVIENDA",  str_pad(i, 2, "left", "0"), ".CSV"))
  IC_Municipio <- df %>%
    group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
    filter(INGTRHOG  != "NA" & INGTRHOG != "999999") %>% #subset para cálculo de Ingreso
    summarise(Ingreso_promedio_mensual =  weighted.mean((INGTRHOG)/3, w = FACTOR),
              Integrantes_Promedio = weighted.mean(NUMPERS, w =FACTOR)) #sólo para las jefaturas que reportan ingresos  
  B<-bind_rows(B, IC_Municipio)
}
#hay algunos municipios para los que no se puede calcular el ingreso 

############    Juntar las dos intercensales    ##########################
IC_Nac <-left_join(A, B, by = c("ENT", "NOM_ENT", "MUN", "NOM_MUN"))
write.csv(x = IC_Nac, file = "Datos/Intercensal/ICNAC_20180119.csv")

############    Juntar con padrón electoral     ##########################
Padron_2017 <- read_xlsx(path = "Datos/Electorales/Padron Electoral/DatosAbiertos-DERFE-pl_20170731.xlsx")
#agregar a municipio 

############    Sacar densidad por municipio    ##########################

############    Unir con densidad por municipio ##########################





#Necesitamos, fundamentalmente, dos variables ingreso gobierno, como porcentaje de las viviendas dentro del municipio, e ingreso



rm(A)

write.csv(x = IC_Pais, file = "Datos/Intercensal/IC_Pais.csv")





ViviendaIC$JEFE_SEXO<-factor(ViviendaIC$JEFE_SEXO, labels = c("Hombre", "Mujer"))
ViviendaIC[ViviendaIC$JEFE_EDAD == "999", ]$JEFE_EDAD <- NA#clasificar correctamente los NAs de edad del jefe del hogar

#para ingresos
ICMUN <- ViviendaIC %>% 
  group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>% 
  filter(INGTRHOG  != "NA" & INGTRHOG != "999999") %>% #subset para cálculo de Ingreso
  summarise(Ingreso_promedio_mensual =  weighted.mean((INGTRHOG)/3, w = FACTOR),
            Integrantes_Promedio = weighted.mean(NUMPERS, w =FACTOR),
            Edad_Promedio_Jefatura = weighted.mean(JEFE_EDAD, w =FACTOR,na.rm = TRUE)) #sólo para las jefaturas que reportan ingresos  

ICMUN2 <- ViviendaIC %>%
  group_by(ENT, NOM_ENT, MUN, NOM_MUN) %>%
  summarise(Viviendas = sum(FACTOR),
            Jefe_Hombre = sum(FACTOR[JEFE_SEXO == "Hombre"]),
            Jefe_Mujer  = sum(FACTOR[JEFE_SEXO == "Mujer"]),
            Ingreso_Otro_Pais = sum(FACTOR[INGR_PEROTROPAIS == "1"]),
            Ingreso_del_Pais = sum(FACTOR[INGR_PERDENTPAIS == "3"]),
            Ingreso_Gobierno = sum(FACTOR[INGR_AYUGOB == "5"]),
            No_Ingreso_Gobierno = sum(FACTOR[INGR_AYUGOB == "6"]),
            Poca_Variedad_Alimentos = sum(FACTOR[ING_ALIM_ADU3 == "5"])) %>%
  mutate(Por_Jefe_Hombre = (Jefe_Hombre/Viviendas) * 100,
         Por_Jefe_Mujer = (Jefe_Mujer/Viviendas) *100, 
         Por_Ingreso_otro_Pais= (Ingreso_Otro_Pais/Viviendas) * 100,
         Por_Ingreso_del_Pais = (Ingreso_del_Pais/Viviendas) * 100,
         Por_Ingreso_Gobierno = (Ingreso_Gobierno/Viviendas) * 100,
         Por_Ingreso_Gobierno2 = (Ingreso_Gobierno/ (Ingreso_Gobierno + No_Ingreso_Gobierno)),
         Por_Poca_Variedad_Alimentos = (Poca_Variedad_Alimentos/Viviendas) * 100)
Intercensal2015 <- inner_join(ICMUN, ICMUN2, by = c("ENT", "NOM_ENT","MUN","NOM_MUN"))
colnames(Intercensal2015) <- str_c("IC", colnames(Intercensal2015), sep = "_")

sum(ICMUN2$)

write.csv(x = Intercensal2015, file = "Datos/Intercensal/IntercensalMunicipal.csv")
#Para intercensal 
#personas que reciben dinero de alguien que vive en otro país, 1 si, 2 No
#en otra vivienda del país, 3 Si, 4 No
#de programas de gobierno 5 Si, 6 No
#tuvo paca variedad en sus alimentos por falta de dinero 5 Si, 6 No.
#edad promedio del jefe de familia
#proporción de jefatura de hombres

#1. Dos bases, personas y vivienda, sacar información a nivel municipal
#2. Unir datos de las dos bases
write.csv(x = ViviendaIC, file = "Datos/Intercensal/Vivienda_2015.csv")# vivienda intercensal 

#sacarlo para Jalisco, municipal, homologarlo con shapefiles. 
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
PersonaIC <- read.csv(file = "Datos/Intercensal/Persona_2015.csv")
###################################################################
###################    Pirámide Poblacional #######################
###################################################################
#PPJal <- PersonaIC %>% group_by(SEXO) %>% count(vars = EDAD, wt = FACTOR)#para todo el estado
# lo mismo pero para un municipio
Municipio_piramide <- "San Pedro Tlaquepaque"
#PPJal <- PersonaIC %>% group_by(SEXO) %>% count(vars = EDAD, wt = FACTOR) #todo el estado
PPJal <- PersonaIC %>% filter(NOM_MUN == Municipio_piramide) %>% group_by(SEXO) %>% count(vars = EDAD, wt = FACTOR) #para usar con algún municipio
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
#write.csv(PPJal, file = "Datos/Intercensal/Piramide_Jalisco_2015.csv") 
#write.csv(PPJal, file = "Datos/Intercensal/Piramide_SPT_2015.csv") 

#piramide poblacional electoral
#incorporar listas nominales
#listas nominales a nivel sección electoral
Municipio_piramide <- "Zapopan"
#PPJal <- PersonaIC %>% group_by(SEXO) %>% count(vars = EDAD, wt = FACTOR) #todo el estado
PPJal <- PersonaIC %>% filter(NOM_MUN == Municipio_piramide) %>% group_by(SEXO) %>% count(vars = EDAD, wt = FACTOR) #para usar con algún municipio
#suma del tiempo dedicado a actividades sin pago 
colnames(PPJal) <- c('Sexo', "Edad", "Pob")
PPJal <- PPJal[ PPJal$Edad < 111,] #quitamos unos  NA's codificados como 999 
#elaboramos las categorias de edad 
PPJal<-mutate(PPJal, Edad_grupo = 
                ifelse(Edad <= 4, "0-04", 
                       ifelse(Edad <= 9, "05-09",
                              ifelse(Edad <= 14, "10-14",
                                     ifelse(Edad <= 17, "15-17",
                                            ifelse(Edad <= 22, "18-22",
                                                   ifelse(Edad <= 27, "23-27",
                                                          ifelse(Edad <= 32, "28-32",
                                                                 ifelse(Edad <= 37, "33-37",
                                                                        ifelse(Edad <= 42, "37-42",
                                                                               ifelse(Edad <= 47, "43-47",
                                                                                      ifelse(Edad <= 52, "48-52",
                                                                                             ifelse(Edad <= 57, "53-57",
                                                                                                    ifelse(Edad <=62, "58-62",
                                                                                                           ifelse(Edad <= 67, "63-67",
                                                                                                                  ifelse(Edad <=72, "68-72",
                                                                                                                         ifelse(Edad <= 77, "73-77", 
                                                                                                                                ifelse(Edad <= 82, "78-82",
                                                                                                                                       ifelse(Edad <= 87, "83-87",
                                                                                                                                              ifelse(Edad <= 92, "88-92","93+"))))))))))))))))))))
ggplot(data = (PPJal %>% filter(Edad >= 18))) +
  geom_bar(aes(Edad_grupo,Pob,group=Sexo,fill=Sexo), stat = "identity",subset(PPJal,PPJal$Sexo=="Mujer" & Edad >= 18), width = .8) +
  geom_bar(aes(Edad_grupo,-Pob,group=Sexo,fill=Sexo), stat = "identity",subset(PPJal,PPJal$Sexo=="Hombre"& Edad >= 18), width = .8) +
  #scale_y_continuous(breaks = seq(-400000, 400000, 50000), 
  #                   labels = paste0(as.character(c(seq(400, 0, -50), seq(50, 400, 50))))) +
  scale_y_continuous(breaks = seq(-40000, 40000, 5000), 
                     labels = paste0(as.character(c(seq(400, 0, -50), seq(50, 400, 50))))) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  #xlab(label = "Edad") + ylab("Población (miles)") + ggtitle("Población Jalisco, Encuesta Intercensal 2015") +
  xlab(label = "Edad") + ylab("Población (miles)") + ggtitle("Población Zapopan") +
  theme_bw()

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

########################################
##### EDOMEX SECCIÓN 2017 ##############
#EDOMEX2017 <- read_excel(path = "C:/Users/Franco/Desktop/Resultados_computo_de_Gobernador_2017_por_casilla.xlsx") #laptop
EDOMEX2017 <- read_excel(path = "Datos/Electorales/Edomex/Resultados_computo_de_Gobernador_2017_por_casilla.xlsx") #laptop y mounstruo?
colnames(EDOMEX2017) <- make.names(colnames(EDOMEX2017))
EDOMEXSECC2017 <- EDOMEX2017 %>% 
  group_by(ID_ESTADO, ID_MUNICIPIO, MUNICIPIO, SECCIÓN) %>% 
  summarise(PRI           = sum(PRI), 
            PAN           = sum(PAN), 
            PRD           = sum(PRD),
            PT              = sum(PT),
            PVEM            = sum(PVEM),
            MORENA          = sum(MORENA),
            ES              = sum(ES),
            PRI.VERDE.NA.ES = sum(PRI.VERDE.NA.ES),
            PRI.VERDE.NA = sum(PRI.VERDE.NA),
            PRI.VERDE.ES = sum(PRI.VERDE.ES),
            PRI.NA.ES = sum(PRI.NA.ES),
            PRI.VERDE = sum(PRI.VERDE),
            PRI.NA = sum(PRI.NA),
            PRI.ES = sum(PRI.ES),
            VERDE.NA.ES = sum(VERDE.NA.ES),
            VERDE.NA = sum(VERDE.NA),
            VERDE.ES = sum(VERDE.ES),
            NA.ES = sum(NA.ES),
            TERESA.CASTELL = sum(TERESA.CASTELL),
            Nueva.A       = sum(NA.),
            LISTA_NOMINAL = sum(LISTA_NOMINAL),
            TOTAL_VALIDOS = sum(NUM_VOTOS_VALIDOS),
            TOTAL_VOTOS   = sum(TOTAL_VOTOS),
            POR_PART      = sum(TOTAL_VOTOS)/sum(LISTA_NOMINAL) * 100)
EDOMEXSECC2017$POR_PRI     <- (EDOMEXSECC2017$PRI/EDOMEXSECC2017$TOTAL_VALIDOS) * 100 #Ojo, sobre total de votos validos
EDOMEXSECC2017$POR_PAN     <- (EDOMEXSECC2017$PAN/EDOMEXSECC2017$TOTAL_VALIDOS) * 100 
EDOMEXSECC2017$POR_PRD     <- (EDOMEXSECC2017$PRD/EDOMEXSECC2017$TOTAL_VALIDOS) * 100
EDOMEXSECC2017$POR_PT      <- (EDOMEXSECC2017$PT/EDOMEXSECC2017$TOTAL_VALIDOS)  * 100 
EDOMEXSECC2017$POR_PVEM    <- (EDOMEXSECC2017$PVEM/EDOMEXSECC2017$TOTAL_VALIDOS)    * 100 
EDOMEXSECC2017$POR_MORENA  <- (EDOMEXSECC2017$MORENA/EDOMEXSECC2017$TOTAL_VALIDOS)  * 100 
EDOMEXSECC2017$PRI_ALIANZA <- (EDOMEXSECC2017$PRI + EDOMEXSECC2017$PVEM +
                                 EDOMEXSECC2017$PRI.VERDE.NA + 
                                 EDOMEXSECC2017$PRI.VERDE.ES + EDOMEXSECC2017$PRI.NA.ES + 
                                 EDOMEXSECC2017$PRI.VERDE + EDOMEXSECC2017$PRI.NA + 
                                 EDOMEXSECC2017$PRI.ES + EDOMEXSECC2017$VERDE.NA.ES + 
                                 EDOMEXSECC2017$VERDE.NA + EDOMEXSECC2017$VERDE.ES + EDOMEXSECC2017$NA.ES +
                                 EDOMEXSECC2017$Nueva.A + EDOMEXSECC2017$ES)
EDOMEXSECC2017$PRI_ALIANZA_POR <- (EDOMEXSECC2017$PRI_ALIANZA/EDOMEXSECC2017$TOTAL_VALIDOS)*100
EDOMEXSECC2017[EDOMEXSECC2017$POR_PART >=100,]$POR_PART <-100
EDOMEXSECC2017$POR_Nueva.A <- (EDOMEXSECC2017$Nueva.A/EDOMEXSECC2017$TOTAL_VALIDOS) * 100 
write.csv(x = EDOMEXSECC2017, file = "Datos/Electorales/Edomex/EDOMEXSECC2017.csv")

#### EDOMEX MUN 2017 ##############
EDOMEXMUN2017 <- EDOMEX2017 %>% group_by(ID_ESTADO, ID_MUNICIPIO, MUNICIPIO) %>% 
  summarise(PRI           = sum(PRI), 
            PAN           = sum(PAN), 
            PRD           = sum(PRD),
            PT            = sum(PT),
            PVEM          = sum(PVEM),
            MORENA        = sum(MORENA),
            MORENA          = sum(MORENA),
            ES            =sum(ES),
            PRI.VERDE.NA.ES = sum(PRI.VERDE.NA.ES),
            PRI.VERDE.NA = sum(PRI.VERDE.NA),
            PRI.VERDE.ES = sum(PRI.VERDE.ES),
            PRI.NA.ES = sum(PRI.NA.ES),
            PRI.VERDE = sum(PRI.VERDE),
            PRI.NA = sum(PRI.NA),
            PRI.ES = sum(PRI.ES),
            VERDE.NA.ES = sum(VERDE.NA.ES),
            VERDE.NA = sum(VERDE.NA),
            VERDE.ES = sum(VERDE.ES),
            NA.ES = sum(NA.ES),
            TERESA.CASTELL = sum(TERESA.CASTELL),
            Nueva.A       = sum(NA.),
            LISTA_NOMINAL = sum(LISTA_NOMINAL),
            TOTAL_VALIDOS = sum(NUM_VOTOS_VALIDOS),
            TOTAL_VOTOS   = sum(TOTAL_VOTOS),
            POR_PART      = sum(TOTAL_VOTOS)/sum(LISTA_NOMINAL) * 100)

EDOMEXMUN2017$POR_PRI     <- (EDOMEXMUN2017$PRI/EDOMEXMUN2017$TOTAL_VALIDOS) * 100 #Ojo, sobre total de votos validos
EDOMEXMUN2017$POR_PAN     <- (EDOMEXMUN2017$PAN/EDOMEXMUN2017$TOTAL_VALIDOS) * 100 
EDOMEXMUN2017$POR_PRD     <- (EDOMEXMUN2017$PRD/EDOMEXMUN2017$TOTAL_VALIDOS) * 100
EDOMEXMUN2017$POR_PT      <- (EDOMEXMUN2017$PT/EDOMEXMUN2017$TOTAL_VALIDOS)  * 100 
EDOMEXMUN2017$POR_PVEM    <- (EDOMEXMUN2017$PVEM/EDOMEXMUN2017$TOTAL_VALIDOS)    * 100 
EDOMEXMUN2017$POR_MORENA  <- (EDOMEXMUN2017$MORENA/EDOMEXMUN2017$TOTAL_VALIDOS)  * 100 
EDOMEXMUN2017$POR_Nueva.A <- (EDOMEXMUN2017$Nueva.A/EDOMEXMUN2017$TOTAL_VALIDOS) * 100 
  EDOMEXMUN2017$PRI_ALIANZA <- (EDOMEXMUN2017$PRI + + EDOMEXMUN2017$PVEM + EDOMEXMUN2017$PRI.VERDE.NA +
                                EDOMEXMUN2017$PRI.VERDE.NA.ES +
                                 EDOMEXMUN2017$PRI.VERDE.ES + EDOMEXMUN2017$PRI.NA.ES + 
                                 EDOMEXMUN2017$PRI.VERDE + EDOMEXMUN2017$PRI.NA + 
                                 EDOMEXMUN2017$PRI.ES + EDOMEXMUN2017$VERDE.NA.ES + 
                                 EDOMEXMUN2017$VERDE.NA + EDOMEXMUN2017$VERDE.ES + EDOMEXMUN2017$NA.ES +
                                 EDOMEXMUN2017$Nueva.A + EDOMEXMUN2017$ES)
EDOMEXMUN2017$PRI_ALIANZA_POR <- (EDOMEXMUN2017$PRI_ALIANZA/EDOMEXMUN2017$TOTAL_VALIDOS)*100
write.csv(x = EDOMEXMUN2017, file = "Datos/Electorales/Edomex/EDOMEXMUN2017.csv")

#########################################################################################

library(raster)






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

write.csv(x = ENOE,file = "Datos/ENOE/Sociodemografico/Formateados CSV/Sdem317.csv")
write.csv(x = PersonaIC, file = "Datos/Intercensal/Persona_2015.csv")# Persona intercensal 
write.csv(x = ViviendaIC, file = "Datos/Intercensal/Vivienda_2015.csv")# vivienda intercensal 

##############################################
######    Lista Nominal   ####################

PE <-read_xlsx(path = "./Datos/Electorales/Padron Electoral/DatosAbiertos-DERFE-pl_20170731.xlsx")
#Lista nominal Municipal
head(PE)
PE %>% group_by(ESTADO, DISTRITO, MUNICIPIO) %>% 
  summarize(PADRON_HOMBRES )

PE %>% filter(ESTADO == "14", MUNICIPIO %in% c(120,99,102, 98,69,55)) %>% 
  ggplot(aes(x = LISTA, colour = LISTA)) +  geom_histogram()  + facet_wrap(~MUNICIPIO)
