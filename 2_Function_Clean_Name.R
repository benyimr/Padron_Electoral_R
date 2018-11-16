
clean_name <- function(name){
  
  data_temp <- data.frame(nombre_temp_A = rep(NA, length(name)))
  
  data_temp$nombre_temp_A <- name
  data_temp$nombre_temp1 <- stringr::str_c(" ", data_temp$nombre_temp_A, " ")

  problematicos <- c(" D "," DA "," DE "," DEL "," DI "," DO "," DOS "," EL "," LA "," LAS "," LOS "," MC "," DE_LOS "," DE_LAS "," DE_LA ", " NIÑO DE_ZEPEDA",
                     " DEL_SAGRADO CORAZON DE_JESUS"," DEL_SAGRADO CORAZON", " SAN ", " SANTA ", " Y ", " DEL_NIÑO JESUS", " LADRON DE_GUEVARA", " DE_TODOS LOS_SANTOS",
                     " PONCE DE_LEON", " VAN ", " VAN_DER "," VON ", " VON_DER ", " DE_VON ", " DE_VAN ", " DE_VON_DER ", " DE_VAN_DER ", " DU SANTOS", " GARCIA HUIDOBRO ",
                     " GJURATOVIC MISE ", " LE ", " MAC ", " PALACIOS DE_CHUQUIURE ", " DELLA ", " PEREZ COSIO ", " SAINZ DE_LA_PEÑA ", " SANCHEZ DE_LOZADA ", " ABD EL_KADER ")
  
  soluciones    <- c(" D_"," DA_"," DE_"," DEL_"," DI_"," DO_"," DOS_"," EL_"," LA_"," LAS_"," LOS_"," MC_"," DE_LOS_"," DE_LAS_"," DE_LA_", " NIÑO_DE_ZEPEDA",
                     " DEL_SAGRADO_CORAZON_DE_JESUS"," DEL_SAGRADO_CORAZON", " SAN_", " SANTA_", "_Y_", " DEL_NIÑO_JESUS", " LADRON_DE_GUEVARA", " DE_TODOS_LOS_SANTOS",
                     " PONCE_DE_LEON", " VAN_", " VAN_DER_"," VON_", " VON_DER_", " DE_VON_", " DE_VAN_", " DE_VON_DER_", " DE_VAN_DER_", " DU_SANTOS", " GARCIA_HUIDOBRO ",
                     " GJURATOVIC_MISE ", " LE_", " MAC_", " PALACIOS_DE_CHUQUIURE ", " DELLA_", " PEREZ_COSIO ", " SAINZ_DE_LA_PEÑA ", " SANCHEZ_DE_LOZADA ", " ABD_EL_KADER ")
  
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp1, pattern = problematicos[1], replacement = soluciones[1])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[2], replacement = soluciones[2])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[3], replacement = soluciones[3])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[4], replacement = soluciones[4])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[5], replacement = soluciones[5])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[6], replacement = soluciones[6])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[7], replacement = soluciones[7])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[8], replacement = soluciones[8])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[9], replacement = soluciones[9])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[10], replacement = soluciones[10])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[11], replacement = soluciones[11])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[12], replacement = soluciones[12])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[13], replacement = soluciones[13])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[14], replacement = soluciones[14])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[15], replacement = soluciones[15])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[16], replacement = soluciones[16])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[17], replacement = soluciones[17])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[18], replacement = soluciones[18])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[19], replacement = soluciones[19])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[20], replacement = soluciones[20])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[21], replacement = soluciones[21])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[22], replacement = soluciones[22])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[23], replacement = soluciones[23])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[24], replacement = soluciones[24])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[25], replacement = soluciones[25])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[26], replacement = soluciones[26])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[27], replacement = soluciones[27])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[28], replacement = soluciones[28])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[29], replacement = soluciones[29])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[30], replacement = soluciones[30])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[31], replacement = soluciones[31])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[32], replacement = soluciones[32])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[33], replacement = soluciones[33])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[34], replacement = soluciones[34])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[35], replacement = soluciones[35])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[36], replacement = soluciones[36])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[37], replacement = soluciones[37])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[38], replacement = soluciones[38])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[39], replacement = soluciones[39])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[40], replacement = soluciones[40])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[41], replacement = soluciones[41])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[42], replacement = soluciones[42])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[43], replacement = soluciones[43])
  data_temp$nombre_temp2 <- stringr::str_replace_all(string = data_temp$nombre_temp2, pattern = problematicos[44], replacement = soluciones[44])

  return(data_temp$nombre_temp2)  
}
