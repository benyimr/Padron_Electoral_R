################################################################################################################################################################################################

#ULTIMA ACTUALIZACION: 15/11/2018
#AUTOR: BENJAMÍN MUÑOZ ROJAS


################################################################################################################################################################################################
#########################################################################                                              #########################################################################  
#########################################################################     PASO 0: GESTIONAR ESPACIO DE TRABAJO     #########################################################################
#########################################################################                                              #########################################################################
################################################################################################################################################################################################

rm(list= ls())
.rs.restartR()

#Activar paquetes utlizados
library(svMisc)
library(stringr)


################################################################################################################################################################################################
#########################################################################                                              #########################################################################  
#########################################################################       PASO 1:  EXTRAER NOMBRES DE PDFS       #########################################################################
#########################################################################                                              #########################################################################
################################################################################################################################################################################################

#Generar un vector de las carpetas a revisar (una por región)
carpetas <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15")

#Generar lista vacía para almacenar nombres de archivos pdf.
lista_archivos <- list()

#Bucle para extraer los nombres de los archivos incluidos en cada carpeta
for( i in 1:15){
  lista_archivos[[i]] <- list.files(path = paste0("/Users/benjaminmunozrojas/pCloud Drive/5_Proyectos/0_Copias_Locales/1_PADRON_AUDITADO_CHILE_2018/1_ORIGINALES/REGION",carpetas[i]), pattern = ".pdf")
}


#Transformar en data.frame().
listado_archivos <- data.frame(archivo = unlist(lista_archivos))

#Generar variable de nombre de carpeta
listado_archivos$carpeta <- paste0("REGION",stringr::str_sub(string = listado_archivos$archivo, start = 2, end = 3))

#Importar datos de circunscripciones electrorales
circuns <- readxl::read_excel(path = "0_Circunscripciones_Electorales_2017.xlsx")


################################################################################################################################################################################################
#########################################################################                                              #########################################################################  
#########################################################################     PASO 2:  EJECUTAR CODIGO DE LIMPIEZA     #########################################################################
#########################################################################                                              #########################################################################
################################################################################################################################################################################################

#Bucle para ejecutar el código de limpieza de archivos 
for(i in 1 : 2){
  #Archivo temporal: cuál es el PDF que se está procesando
  temp <- paste0("/Users/benjaminmunozrojas/pCloud Drive/5_Proyectos/0_Copias_Locales/1_PADRON_AUDITADO_CHILE_2018/1_ORIGINALES/",listado_archivos[i,2],"/",listado_archivos[i,1])
  
  #Archivo temporal: cómo se llamará el CSV al guardarlo
  save <- paste0(word(listado_archivos[i,1], sep = "\\."),".csv")
  
  #Codigo de procesamiento de datos
  source("1_Code_for_Cleaning_Pdf_Files.R")

  }


#Generar path dónde se almacenaron archivos CSV
directorio <- "/Users/benjaminmunozrojas/pCloud Drive/5_Proyectos/0_Copias_Locales/1_PADRON_AUDITADO_CHILE_2018/2_RESULTADOS"
  
#Extraer listado de archivos CSV
listado_archivos_csv <- dir(path = directorio, pattern = "*.csv", full.names = T, recursive = F)

#Extraer iterativamente los datos (importar vía función read_csv())
datos <- lapply(X = listado_archivos_csv, FUN = read_csv)

#Generar un objeto vacío para almacenar el padrón electoral
padron <- data.frame()

#Bucle para combinar cada uno de los archivos CSV que conforman el padrón electoral
for(i in 1:length(datos)){
  padron <- bind_rows(padron,datos[[i]])
}

#Guardar el objeto de padrón electoral
write_rds(x = padron, path = "4_PADRON_ELECTORAL_CHILE_2018.rds")

