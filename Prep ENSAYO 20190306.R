
library(AER)#libro de econometria con R
library(PerformanceAnalytics)#para correlaciones del voto 

#Regresiones y correlaciones para el ensayo


head(EMENSAYO)

EMENSAYO
PRI
pan::PRD
PT
PVEM
MORENA
ES

#Partido que gana el municipio en el 2012
#Partido que gana el municipio en le 2017
summary(EMENSAYO$EM_17_POR_PART)
hist(EMENSAYO$EM_17_POR_PART, breaks = 15)

EMENSAYO$EM_17_POR_PRI +
EMENSAYO$EM_17_POR_PAN +
EMENSAYO$EM_17_POR_PRD +
EMENSAYO$EM_17_POR_PT +
EMENSAYO$EM_17_POR_PVEM +
EMENSAYO$EM_17_POR_MORENA +
EMENSAYO$EM_17_POR_Nueva.A 

summary(EMENSAYO$EM_12_Por_Part)
hist(EMENSAYO$EM_12_Por_Part, breaks = 35)


#económicas
EMENSAYO$Por_Ingreso_Gobierno
EMENSAYO$Por_Ingreso_otro_Pais
EMENSAYO$Por_Ingreso_del_Pais
EMENSAYO$Por_Poca_Variedad_Alimentos


#Participacion 2012, 2017, proxy de ingreso, urbano o rural
#Partido que gobernaba
#den
#Regresión
#Variables relevantes
colnames(EMENSAYO)




#Gráficas para el ensayo de EDOMEX



#Por hacer
#Densidad del municipio Polación/territorio
#elecciones competidas y no competidas
#los niveles y lo que se disputa en la elección



#impuestos
#correlaciones
#modelo de regresi[on]
#ensayo
#mapas
