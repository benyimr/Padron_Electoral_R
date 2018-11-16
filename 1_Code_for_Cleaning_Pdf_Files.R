################################################################################################################################################################################################

#ULTIMA ACTUALIZACION: 15/11/2018
#AUTOR: BENJAMÍN MUÑOZ ROJAS





################################################################################################################################################################################################
#########################################################################                                              #########################################################################  
#########################################################################     PASO 0: GESTIONAR ESPACIO DE TRABAJO     #########################################################################
#########################################################################                                              #########################################################################
################################################################################################################################################################################################

#rm(list= ls())
#.rs.restartR()
#options(max.print = 20000)

#Paquetes utilizados
library(Hmisc)
library(pdftools)
library(readxl)
library(stringr)
library(tcR)
library(tidyverse)


################################################################################################################################################################################################
#########################################################################                                              #########################################################################  
#########################################################################         PASO 1: IMPORTAR ARCHIVO PDF         #########################################################################
#########################################################################                                              #########################################################################
################################################################################################################################################################################################


#Importar pdf (lectura) como cadena de caracteres
original <- pdftools::pdf_text(pdf = temp)


################################################################################################################################################################################################
#########################################################################                                              #########################################################################  
#########################################################################          PASO 2: SEPARAR POR LINEAS          #########################################################################
#########################################################################                                              #########################################################################
################################################################################################################################################################################################

#Separar archivo según líneas-filas del archivo. 
por_filas <- stringr::str_split(string = original, pattern = "\n")  #El output es una lista


################################################################################################################################################################################################
#########################################################################                                              #########################################################################  
#########################################################################     PASO 2A:EXPLORAR ESTRUCTURA DE DATOS     #########################################################################
#########################################################################                                              #########################################################################
################################################################################################################################################################################################

#head(por_filas[[1]],25)

#head(por_filas[[1]]) 
#head(por_filas[[2]]) #Las primeras 4 líneas se repiten siempre (eliminables)

#Explorar si todas las hojas terminan en "".
#check <- list()
#
#for(i in 1: length(por_filas)){
#  check[[i]] <- por_filas[[i]][length(por_filas[[i]])]
#}
#
#check1 <- unlist(check)
#table(check1)
#CADA PAGINA TERMINA EN UN "", TAMBIÉN DEBEN REMOVERSE (AHORRO ESPACIO)


################################################################################################################################################################################################
#########################################################################                                              #########################################################################  
#########################################################################       PASO 3: ELIMINAR FILAS REPETIDAS       #########################################################################
#########################################################################                                              #########################################################################
################################################################################################################################################################################################


#Al eliminar las 4 primeras líneas de encabezado y la última de cada página (corresponde a ""). Se reduce el tamaño.

#Generar lista vacía para almacenar información
por_filas2 <- list()

#Bucle para eliminar exceso de espacios vacíos, el encabrzado y la última fila de cada hoja del PDF.
for(i in 1:length(por_filas)){
  por_filas2[[i]] <- stringr::str_squish(por_filas[[i]][c(-1,-2,-3,-4,-length(por_filas[[i]]))])
}


################################################################################################################################################################################################
#########################################################################                                              #########################################################################  
#########################################################################  PASO 4:COMBINAR FILAS EN UN UNICO ARCHIVO   #########################################################################
#########################################################################                                              #########################################################################
################################################################################################################################################################################################

#Transformar en data.frame
datos <- data.frame(info = unlist(por_filas2))


################################################################################################################################################################################################
#########################################################################                                              #########################################################################  
#########################################################################   PASO 5: EXTRAER VARIABLES FIJAS POR PDF    #########################################################################
#########################################################################                                              #########################################################################
################################################################################################################################################################################################

#Extraer encabezado de una hoja y colapsar en una única cadena de caracteres
encabezado <- data.frame(info = stringr::str_squish(stringr::str_c(por_filas[[1]][1:4], collapse = "")))

#Generar variable de número de registros en PDF (Nº de personas)
encabezado$control_n <- as.numeric(stringr::str_replace_all(string = 
                                                            stringr::str_squish(string = 
                                                                                stringr::str_sub(string = encabezado$info, 
                                                                                                  start = (stringr::str_locate(string = encabezado$info, pattern = "REGISTROS:")[2]+1), 
                                                                                                  end = (stringr::str_locate(string = encabezado$info, pattern = "SERVICIO ELECTORAL")[1]-1))), 
                                                            pattern = "\\.", replacement = ""))

#Nº de filas excesivas (por limpiar)
encabezado$problema_n <- dim(datos)[1] - encabezado$control_n

#Variable de región, provincia y comuna
encabezado$region    <- stringr::str_squish(stringr::str_sub(string = encabezado$info, 
                                                             start = (stringr::str_locate(string = encabezado$info, pattern = "REGIÓN :")[2]+1), 
                                                             end = (stringr::str_locate(string = encabezado$info, pattern = "COMUNA:")[1]-1)))
encabezado$provincia <- stringr::str_squish(stringr::str_sub(string = encabezado$info, 
                                                             start = (stringr::str_locate(string = encabezado$info, pattern = "PROVINCIA :")[2]+1), 
                                                             end = (stringr::str_locate(string = encabezado$info, pattern = "NOMBRE")[1]-1)))
encabezado$comuna    <- stringr::str_squish(stringr::str_sub(string = encabezado$info, 
                                                             start = (stringr::str_locate(string = encabezado$info, pattern = "COMUNA:")[2]+1), 
                                                             end = (stringr::str_locate(string = encabezado$info, pattern = "PÁGINA")[1]-1)))

#Extraer las circunscripciones electorales de la comuna
circuns %>% dplyr::filter(comuna == encabezado$comuna) %>% dplyr::pull(circunscripcion) -> circuns_temp


################################################################################################################################################################################################
#########################################################################                                              #########################################################################  
#########################################################################     PASO 5A: EXPLORAR PROBLEMAS EN DATOS     #########################################################################
#########################################################################                                              #########################################################################
################################################################################################################################################################################################

#¿Cuántas palabras hay por fila? Lo normal serían 4 palabras por nombre, una por Rut,
#una por sexo, tres por domicilio electoral, una por circunscripción y dos por mesa,
#dando un TOTAL de 12. Pueden ser un poco más o menos, pero es importante chequear
#casos anómalos.

datos$folio       <- seq_len(nrow(datos)) #Identificador de filas/casos
datos$palabras    <- stringr::str_count(datos$info, pattern = "\\S+") #Conteo de palabras por fila
datos$caracteres  <- stringr::str_length(datos$info)                  #Conteo de caracteres por fila
datos$caracteres1 <- Hmisc::cut2(x = datos$caracteres, cuts = c(20,40,50,60,90,100,110))

#table(datos$palabras)    #Es un problema que hayan filas con tan pocas palabras
#table(datos$caracteres1)

#NO TODAS LAS FILAS CONTIENEN LA MISMA INFORMACIÓN. HAY ALGUNAS QUE SÓLO CONTIENEN LA CONTINUACIÓN DEL NOMBRE (SI ES MUY EXTENSO) O DEL DOMICILIO (SI ES MUY EXTENSO).
#PUEDEN SER VARIAS FILAS.


################################################################################################################################################################################################
#########################################################################                                              #########################################################################  
#########################################################################    PASO 6:VARIABLES INDICADORAS O FILTROS    #########################################################################
#########################################################################                                              #########################################################################
################################################################################################################################################################################################

#Extraer sexo (tres caracteres posteriores a la variable temporal de RUT): sólo hay dos patrones válidos (MUJ y VAR), lo otro es indicativo de problemas  con la fila
datos$sexo_temp    <- stringr::str_sub(string = datos$info, start = stringr::str_locate(string = datos$info, pattern = "\\d[-]")[,1]+4, 
                                       end = stringr::str_locate(string = datos$info, pattern = "\\d[-]")[,1]+6)

#Extraer circunscripción electoral
datos$circ_temp <- if_else(condition = ((stringr::word(datos$info, -1) == "V"|stringr::word(datos$info, -1) == "M") & ((stringr::word(datos$info, -3) %in% circuns_temp)==TRUE)), 
                           true = stringr::word(datos$info, -3), 
                           false = if_else(condition = ((stringr::word(datos$info, -1) == "V"|stringr::word(datos$info, -1) == "M") & ((stringr::word(datos$info, -4,-3) %in% circuns_temp)==TRUE)), 
                                           true = stringr::word(datos$info, -4,-3), 
                                           false = if_else(condition = ((stringr::str_sub(datos$info, start = -1, end = -1)=="V"|stringr::str_sub(datos$info, start = -1, end = -1)=="M") &
                                                                          ((stringr::str_length(stringr::word(datos$info, -1))<4)==TRUE) &
                                                                          ((stringr::word(datos$info, -2) %in% circuns_temp)==TRUE)), 
                                                           true = stringr::word(datos$info, -2), 
                                                           false = if_else(condition = ((stringr::str_sub(datos$info, start = -1, end = -1)=="V"|stringr::str_sub(datos$info, start = -1, end = -1)=="M") &
                                                                                          ((stringr::str_length(stringr::word(datos$info, -1))<4)==TRUE) &
                                                                                          ((stringr::word(datos$info, -3,-2) %in% circuns_temp)==TRUE)), 
                                                                           true = stringr::word(datos$info,-3, -2), 
                                                                           false = if_else(condition = ((is.na(as.numeric(stringr::word(datos$info,-1)))==FALSE) & 
                                                                                                          ((stringr::str_length(stringr::word(datos$info, -1))<4)==TRUE) & 
                                                                                                          ((stringr::word(datos$info, -2) %in% circuns_temp)==TRUE)), 
                                                                                           true = stringr::word(datos$info, -2), 
                                                                                           false = if_else(condition = ((is.na(as.numeric(stringr::word(datos$info,-1)))==FALSE) & 
                                                                                                                          ((stringr::str_length(stringr::word(datos$info, -1))<4)==TRUE) & 
                                                                                                                          ((stringr::word(datos$info, -3,-2) %in% circuns_temp)==TRUE)), 
                                                                                                           true = stringr::word(datos$info, -3,-2), 
                                                                                                           false = "NO_CIRCUNSCRIPCION"))))))


################################################################################################################################################################################################
#########################################################################                                              #########################################################################  
#########################################################################     PASO 7: AJUSTAR DIMENSIONES DE DATOS     #########################################################################
#########################################################################                                              #########################################################################
################################################################################################################################################################################################

#TIPOS DE FILAS:
#                FILAS SIN PROBLEMAS                                                =  0   sexo_filtro == 0  circ_filtro == 0             
#                FILAS QUE SOLO CONTIENEN UN NOMBRE ADICIONAL                       =  1   sexo_filtro == 1  circ_filtro == 1                      
#                FILAS QUE CONTIENEN EL DOMICILIO, CIRCUNSCRIPCIÓN Y MESA           =  2   sexo_filtro == 1  circ_filtro == 1                 
#                FILAS QUE CONTIENEN PARTE DEL DOMICILIO, CIRCUNSCRIPCIÓN Y/O MESA  =  3   sexo_filtro == 0  circ_filtro == 1  sexo_filtro == 1  circ_filtro == 0   

#Recodificar sexo como filtro
datos$sexo         <- car::recode(datos$sexo_temp,"'MUJ'='Mujer';'VAR'='Hombre';else='NO_SEXO'")

#Clasificar las filas según información disponible
datos$filtro_condicion <- if_else(condition = (datos$sexo != "NO_SEXO" & datos$circ_temp != "NO_CIRCUNSCRIPCION"), true = "A", 
                                  false = if_else(condition = (datos$sexo != "NO_SEXO" & datos$circ_temp == "NO_CIRCUNSCRIPCION"), true = "B", 
                                                  false = if_else(condition = (datos$sexo == "NO_SEXO" & datos$circ_temp != "NO_CIRCUNSCRIPCION"), true = "C", 
                                                                  false = "D")))

#A = filas con toda la información disponible
#B = filas solo con la información del comienzo
#C = filas solo con la informacion del final
#D = filas complementarias o intermedias  

#Separador: filas sin problemas (1) y con problemas (0)
datos$separador  <- if_else(condition = (datos$filtro_condicion == "A" | datos$filtro_condicion == "B"), true = 1 , false = 0 )

#Asignar folio a filas condición A y B, al resto (a las cuales se les debe asignar un folio que permita combinarlas) se les deja NA
datos$folio1    <- if_else(condition = (datos$filtro_condicion == "A" | datos$filtro_condicion == "B"), true = as.character(datos$folio), false = "NA" )

#Nueva variable para completar folio. Bucle para arrastrar el valor del folio según grupo
datos$folio2    <- rep(NA, dim(datos)[1])
datos$folio2[1] <- 1

for( i in 2 : dim(datos)[1] ){
  if(datos$filtro_condicion[i] == "C" | datos$filtro_condicion[i] == "D"){
    datos$folio2[i] <- datos$folio2[i-1]
  } else {
    datos$folio2[i] <- datos$folio[i]
  }
}

#Identificar filas problemáticas ("AD" y "CD")
datos$condicion_lag <- dplyr::lag(datos$filtro_condicion, 1)
datos$complemento <- paste0(datos$condicion_lag, datos$filtro_condicion)

#Separar en bases de datos: A son filas con folio, B son filas a combinar
datos %>% dplyr::filter(separador == 1) %>% dplyr::select(info, filtro_condicion, folio2, complemento) -> datos_A
datos %>% dplyr::filter(separador == 0) %>% dplyr::select(info, filtro_condicion, folio2, complemento) -> datos_B

#Se genera C dónde se combinan según folio todos los textos.
datos_B %>% dplyr::filter(complemento!="AD", complemento!="CD") %>% dplyr::group_by(folio2) %>% 
  dplyr::summarise(info2 = paste(info, collapse = "")) %>% dplyr::rename(folio2 = folio2, info = info2) %>% as.data.frame()-> datos_C

#Combinar en una base de datos con la dimensión correcta
datos2 <- dplyr::left_join(x = datos_A, y = datos_C, by = "folio2")

datos2$info <- if_else(condition = is.na(datos2$info.y)==TRUE , true = as.character(datos2$info.x), 
                        false = as.character(paste(datos2$info.x, datos2$info.y)))



################################################################################################################################################################################################
#########################################################################                                              #########################################################################  
#########################################################################    PASO 8:EXTRAER Y PREPROCESAR VARIABLES    #########################################################################
#########################################################################                                              #########################################################################
################################################################################################################################################################################################

#Extraer nombre completo como una única cadena de caracteres (todo el texto antes de un dígito, ya que la segunda columna es el RUT).
datos2$nombre_temp <- stringr::str_sub(string = datos2$info, start = 1 , end = stringr::str_locate(string = datos2$info, pattern = "\\d")[,1]-1)

#Extraer RUT (todos los caracteres desde el primer dígito por fila y hasta el número posterior al guión).
datos2$rut_temp    <- stringr::str_sub(string = datos2$info, start = stringr::str_locate(string = datos2$info, pattern = "\\d")[,1] , 
                                      end = stringr::str_locate(string = datos2$info, pattern = "\\d[-]")[,1]+2)  

#Usar la función `check_rut` para verificar si el conjunto de digitos forma un rut válido. Si no es válido es indicativo de problemas con la fila
source("2_Function_Check_RUT.R")
datos2$rut          <- check_rut(string1 = datos2$rut_temp)

#Extraer sexo (tres caracteres posteriores a la variable temporal de RUT): sólo hay dos patrones válidos (MUJ y VAR), lo otro es indicativo de problemas  con la fila
datos2$sexo_temp    <- stringr::str_sub(string = datos2$info, start = stringr::str_locate(string = datos2$info, pattern = "\\d[-]")[,1]+4, 
                                       end = stringr::str_locate(string = datos2$info, pattern = "\\d[-]")[,1]+6)
datos2$sexo         <- car::recode(datos2$sexo_temp,"'MUJ'='Mujer';'VAR'='Hombre';else='NO_SEXO'")


#Extraer mesa de votación
datos2$mesa_temp1 <- stringr::word(datos2$info,-1)
datos2$mesa_temp2 <- if_else(condition = (datos2$mesa_temp1 == "V" | datos2$mesa_temp1 == "M"), true = "A", 
                             false = if_else(condition = is.na(as.numeric(datos2$mesa_temp1)) == FALSE, true = "B" , false = "C"))
datos2$mesa <- if_else(condition = datos2$mesa_temp2 == "A", true = stringr::str_c(word(datos2$info, -2), word(datos2$info, -1), sep = " "), 
                       false = if_else(condition = datos2$mesa_temp2 == "B", true = datos2$mesa_temp1, 
                                       false = stringr::str_c(str_sub(datos2$mesa_temp1, start = 1, end = str_length(datos2$mesa_temp1)-1),
                                         str_sub(datos2$mesa_temp1, -1, -1), sep = " ")))

#Extraer circunscripción electoral
datos2$circunscripcion <- if_else(condition = ((datos2$mesa_temp2 == "A") & (word(string = datos2$info, start = -3 - str_count(circuns_temp, "\\S+") + 1 , end = -3 ) %in% circuns_temp == T)), 
                                  true = word(string = datos2$info, start = -3 - str_count(circuns_temp, "\\S+") + 1 , end = -3 ), 
                                  false = if_else(condition = ((datos2$mesa_temp2 == "B") & (word(string = datos2$info, start = -2 - str_count(circuns_temp, "\\S+") + 1 , end = -2 ) %in% circuns_temp == T)), 
                                                  true = word(string = datos2$info, start = -2 - str_count(circuns_temp, "\\S+") + 1 , end = -2 ), 
                                                  false = if_else(condition = ((datos2$mesa_temp2 == "C") & (word(string = datos2$info, start = -2 - str_count(circuns_temp, "\\S+") + 1 , end = -2 ) %in% circuns_temp == T)), 
                                                                  true = word(string = datos2$info, start = -2 - str_count(circuns_temp, "\\S+") + 1 , end = -2 ), 
                                                                  false = "Error en Circunscripción")))


#Extraer domicilio electoral
datos2$separador <- stringr::str_c(datos2$circunscripcion, datos2$mesa, sep = " ")
datos2$domicilio <- stringr::str_sub(string = datos2$info, start = stringr::str_locate(string = datos2$info, pattern = "\\d[-]")[,1]+8, 
                                     end = stringr::str_locate(string = datos2$info, pattern = datos2$separador)[,1]-1)


#Limpiar nombre
source("2_Function_Clean_Name.R")
datos2$nombre_temp1 <- stringr::str_squish(clean_name(name = datos2$nombre_temp))
datos2$n_palabras <- stringr::str_count(datos2$nombre_temp1, pattern = "\\S+")

datos2$apellido_01a <- if_else(condition = datos2$n_palabras == 1, true = "NA", false = stringr::word(datos2$nombre_temp1, 1))
datos2$apellido_02a <- if_else(condition = datos2$n_palabras == 1, true = "NA", 
                              false = if_else(condition = datos2$n_palabras == 2, true = "NA", 
                                              false = stringr::word(datos2$nombre_temp1, 2)))

datos2$nombre_01a <- if_else(condition = datos2$n_palabras == 1, true = "NA", 
                            false = if_else(condition = datos2$n_palabras == 2, true = stringr::word(datos2$nombre_temp1,2), 
                                            false = stringr::word(datos2$nombre_temp1, 3)))
datos2$nombre_02a <- if_else(condition = datos2$n_palabras == 1, true = "NA", 
                             false = if_else(condition = datos2$n_palabras == 2, true = "NA", 
                                             false = if_else(condition = datos2$n_palabras ==3, true = "NA", false = stringr::word(datos2$nombre_temp1,4))))

datos2$adicional_01 <- if_else(condition = datos2$nombre_temp1>4, true = stringr::word(datos2$nombre_temp1, 5), false = "NA")
datos2$adicional_02 <- if_else(condition = datos2$nombre_temp1>5, true = stringr::word(datos2$nombre_temp1, 6), false = "NA")


datos2$apellido_01 <- stringr::str_replace_all(string = datos2$apellido_01a, pattern = "\\_", replacement = " ")
datos2$apellido_02 <- stringr::str_replace_all(string = datos2$apellido_02a, pattern = "\\_", replacement = " ")
datos2$nombre_01   <- stringr::str_replace_all(string = datos2$nombre_01a, pattern = "\\_", replacement = " ")
datos2$nombre_02   <- stringr::str_replace_all(string = datos2$nombre_02a, pattern = "\\_", replacement = " ")


datos2$region    <- encabezado$region
datos2$provincia <- encabezado$provincia
datos2$comuna    <- encabezado$comuna

#Seleccionar variables relevantes
datos2 %>% dplyr::select(folio2, nombre_temp1, n_palabras, apellido_01, apellido_02, nombre_01, nombre_02, adicional_01, adicional_02,
                         rut_temp, sexo, domicilio, region, provincia, comuna, circunscripcion, mesa) -> datos3

#Guardar archivo resultante
write_csv(x = datos3, path = paste0("/Users/benjaminmunozrojas/pCloud Drive/5_Proyectos/0_Copias_Locales/1_PADRON_AUDITADO_CHILE_2018/2_RESULTADOS/",save))
