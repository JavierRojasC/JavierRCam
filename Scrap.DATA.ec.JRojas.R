DATA.EC.JRojas <- function(){
  require(rvest)
url <- "https://news.google.com/covid19/map?hl=es-419&gl=US&ceid=US%3Aes-419&mid=%2Fm%2F02k1b"
READCOVID = read_html(url)
tabla= READCOVID %>%
  rvest::html_nodes("table")
tabla1 <- html_table(tabla[1], fill = TRUE, header = T, dec = ",")
Table.scrap <- as.data.frame(tabla1)
Table.scrap$Confirmados <- sub(",", "", Table.scrap$Confirmados, fixed = TRUE)
Table.scrap$Confirmados <- as.numeric(Table.scrap$Confirmados)
return(Table.scrap)}


Data.Multiv <- function(){
  url.mult <- 'https://raw.githubusercontent.com/JavierRojasC/JavierRCam/master/datos_mv.csv'
  Datos.Covid.911 <- read.csv2(url.mult)
  return(Datos.Covid.911)
}

#source('https://raw.githubusercontent.com/JavierRojasC/JavierRCam/master/Scrap.DATA.ec.JRojas.R')
#Datos.autoincrement <- DATA.EC.JRojas()
#Datos.cov.911 <- Data.Multiv()

