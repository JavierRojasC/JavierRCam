Detalle_vertical_pilotaje_para_ESPOL <- read.csv2("https://www.dropbox.com/s/adypuf1465d8znm/Detalle_vertical_pilotaje_para_ESPOL.csv?dl=1")

Base <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL$Codigo.Estudiante,
                   Pregunta=Detalle_vertical_pilotaje_para_ESPOL$Pregunta,
                   `C/I`=Detalle_vertical_pilotaje_para_ESPOL$C.I)

Base$Correcta[Base$C.I=='C'] <- 1
Base$Correcta[Base$C.I=='I'] <- 0
Base <- Base[,-3]

CaractIndividuo <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL$`Codigo.Estudiante`,
                              Forma=Detalle_vertical_pilotaje_para_ESPOL$Forma,
                              zona=Detalle_vertical_pilotaje_para_ESPOL$zona,
                              amie=Detalle_vertical_pilotaje_para_ESPOL$amie,
                              distrito=Detalle_vertical_pilotaje_para_ESPOL$distrito,
                              sostenimiento=Detalle_vertical_pilotaje_para_ESPOL$sostenimiento)





B <- pivot_wider(Base, names_from ='Pregunta', values_from = 'Correcta')
C <- merge(x=B, y=unique(CaractIndividuo), all.x = TRUE,by = 'Codigo.Estudiante')

C$distrito[is.na(C$distrito)] <- 'No registrado'

