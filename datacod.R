CienciasLetras <- function(){
  require(readr)
  CienciasLetras <- read_delim("https://raw.githubusercontent.com/JavierRojasC/JavierRCam/master/CienciasLetras.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE)[,1:6]
  return(CienciasLetras)
}

Diabetes <- function(){
  require(readr)
  Data_Diab <- read_delim("https://raw.githubusercontent.com/JavierRojasC/JavierRCam/master/Data%20Diab.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)
  return(Data_Diab)
}

DataACP <- function(){
  require(readr)
  datos_tabla_5_2 <- read_delim("~/Downloads/datos_tabla_5_2.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
  return(datos_tabla_5_2)
}

#cienciasletras <- CienciasLetras()
#diab <- Diabetes()