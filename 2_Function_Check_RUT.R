#FUNCTION FOR CHECKING THE RUT (VERIFICATION DIGIT)

check_rut <- function(string1){
  datos_temp <- data.frame(rut1 = rep(NA,length(string1)))
  
  datos_temp$rut1  <- string1
  datos_temp$rut2  <- stringr::str_remove_all(string = stringr::str_sub(string1, start = 1, end = (str_locate(string1, pattern = "[-]"))[,1]-1), pattern = "\\.")
  datos_temp$rut2a <- if_else(is.na(datos_temp$rut2)==T, "000000000000", datos_temp$rut2)
  datos_temp$rut3  <- stringr::str_sub(string1, start = -1, end = -1)
  datos_temp$rut4 <- tcR::reverse.string(datos_temp$rut2a, .n = 1)
  
  datos_temp$n <- nchar(datos_temp$rut4)
  
  datos_temp$num_1a <- as.numeric(str_sub(string = datos_temp$rut4, start = 1, end = 1)) * 2
  datos_temp$num_2a <- as.numeric(str_sub(string = datos_temp$rut4, start = 2, end = 2)) * 3
  datos_temp$num_3a <- as.numeric(str_sub(string = datos_temp$rut4, start = 3, end = 3)) * 4
  datos_temp$num_4a <- as.numeric(str_sub(string = datos_temp$rut4, start = 4, end = 4)) * 5
  datos_temp$num_5a <- as.numeric(str_sub(string = datos_temp$rut4, start = 5, end = 5)) * 6
  datos_temp$num_6a <- as.numeric(str_sub(string = datos_temp$rut4, start = 6, end = 6)) * 7
  datos_temp$num_7a <- as.numeric(str_sub(string = datos_temp$rut4, start = 7, end = 7)) * 2
  datos_temp$num_8a <- as.numeric(str_sub(string = datos_temp$rut4, start = 8, end = 8)) * 3
  
  datos_temp$agregar <- if_else(datos_temp$n == 6,
                                
                                rowSums(datos_temp[7:12]),
                                
                                if_else(datos_temp$n == 7,
                                        
                                        rowSums(datos_temp[7:13]),
                                        
                                        if_else(datos_temp$n == 8,
                                                
                                                rowSums(datos_temp[7:14]),-999
                                        )))
  
  datos_temp$modular <- if_else(datos_temp$agregar == -999, -999,11- (datos_temp$agregar%%11))
  
  datos_temp$verificar <- if_else(datos_temp$modular > 0 & datos_temp$modular < 10, as.character(datos_temp$modular),
                                  if_else(datos_temp$modular == 10 , "K",
                                          if_else(datos_temp$modular == 11, "0", "NO_RUT")))
  datos_temp$verificar2 <- if_else(is.na(datos_temp$rut3)==T,"NO_RUT",
                                   if_else(datos_temp$rut3 == datos_temp$verificar, "OK", "NO_RUT"))
  
  
  return(datos_temp$verificar2)
}