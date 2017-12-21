Por hacer:
  #1. poner municipios en ambos lados del join de municipios.
  #2. Checar que empaten los nombres de los Municipios
  3. Mapa de participación municipal en quintiles. 
  #4. Reemplazar lectura de bases en internet con bases locales 
  5. Añadir datos municipales, demográficos de ENOE, intercensal
  6. Homologar nombres
  7. Empatar por Estado-Nombre (En lugar de clave única)
  
  for ( i in 1:32) {
    A2 <- dplyr::filter(temp, temp$ID_ESTADO == i )
    B2 <- dplyr::filter(LINEGI, LINEGI$CVE_ENT== unique(LINEGI$CVE_ENT)[i])
    print(unique(LINEGI$CVE_ENT)[i])
    MunicipiosMapa<-rbind(MunicipiosMapa,left_join(B2,A2, by = "MunMin")) #INEGI a la izquierda
  }
  
  8. en funcion de mapas municipales (Para secciones electorales) añadir el nombre del Municipios
  9. ENOE a nivel municipal
  10. Variables a nivel municipal de la intercensal 2015 
  #11. Jalisco, unir municipios por nombre
  12. Cuando se unen secciones electorales agrupar por municipio para unir por clave 
  13. revisar existencia de geolocalización de casillas electorales para mapa muestra
  14. Datos censo 2010, encontrar si hay proyecciones para secciones electorales 
  #15. Unir municipios, no tenemos una clave única 
  15. Revisar como mantener las clases a la hora de hacer mapas
  16. Establecer temática de colores para mapas
  17. Programar función para etiquetas de los mapas 
  18. 