#ENSAYO
library(tidyverse)
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
           Por_Renta = (Renta/Viviendas) * 100)
    )
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

##############        Unir las bases                ######################

EMENSAYO <- inner_join(IC_Municipio, EDOMEXENSAYO, by = "NOM_MUN_JOIN")
rm(EDOMEXENSAYO)
rm(IC_Municipio)
rm(ViviendaIC)
#guardar el archivo
write.csv(x = EMENSAYO, file = "Datos/2018/ENSAYO/ElectICEdomex20190305.csv")

head(EMENSAYO)



