DATA.EC.JRojas <- function(){
  require(rvest)
  url <- "https://www.coronavirusecuador.com/datos-provinciales/"
  READCOVID = read_html(url)
tabla= READCOVID %>%
  rvest::html_nodes("table")
tabla1 <- html_table(tabla[1], fill = TRUE, header = T, dec = ",")
Table.scrap <- as.data.frame(tabla1)
#Table.scrap$Confirmados <- sub(",", "", Table.scrap$Confirmados, fixed = TRUE)
Table.scrap$CONFIRMADOS <- as.numeric(Table.scrap$CONFIRMADOS)
Table.scrap$FALLECIDOS <- as.numeric(Table.scrap$FALLECIDOS)

return(Table.scrap)}


Data.Multiv <- function(){
  url.mult <- 'https://raw.githubusercontent.com/JavierRojasC/JavierRCam/master/datos_mv.csv'
  Datos.Covid.911 <- read.csv2(url.mult)
  Datos.covid.911 <- Datos.Covid.911[1:24,]
  return(Datos.covid.911)
}

Data.Categ <- function(){
url.cat <- 'https://raw.githubusercontent.com/JavierRojasC/JavierRCam/master/categorCovid19.csv' 
Datos.Categ <- read.csv2(url.cat)
print('ADD')
  return(Datos.Categ)
}



Fall.etario <- function(){
  url.fall.et <- 'https://raw.githubusercontent.com/JavierRojasC/JavierRCam/master/Etario%20AC.csv'
  Base.fall.et <- read.csv2(url.fall.et)
  return(Base.fall.et)
  
}

Conf.etario <- function(){
  url.conf.et <- 'https://raw.githubusercontent.com/JavierRojasC/JavierRCam/master/ConfirmadosEdadxProv%20(1).csv'
  Base.conf.et <- read.csv2(url.conf.et)
  return(Base.conf.et)
  
}

#source('https://raw.githubusercontent.com/JavierRojasC/JavierRCam/master/Scrap.DATA.ec.JRojas.R')
#Datos.autoincrement <- DATA.EC.JRojas()
#Datos.cov.911 <- Data.Multiv()
#Datos.categoricos <- Data.Categ()



