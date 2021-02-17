
TodasFormas <- function(materia){
Detalle_vertical_pilotaje_para_ESPOL2_1 = Detalle_vertical_pilotaje_para_ESPOL%>%
  filter(Forma=='F001')%>%
  filter(Campo.de.conocimiento==materia)
Detalle_vertical_pilotaje_para_ESPOL2_2 = Detalle_vertical_pilotaje_para_ESPOL%>%
  filter(Forma=='F002')%>%
  filter(Campo.de.conocimiento==materia)
Detalle_vertical_pilotaje_para_ESPOL2_3 = Detalle_vertical_pilotaje_para_ESPOL%>%
  filter(Forma=='F003')%>%
  filter(Campo.de.conocimiento==materia)
Detalle_vertical_pilotaje_para_ESPOL2_4 = Detalle_vertical_pilotaje_para_ESPOL%>%
  filter(Forma=='F004')%>%
  filter(Campo.de.conocimiento==materia)
Detalle_vertical_pilotaje_para_ESPOL2_5 = Detalle_vertical_pilotaje_para_ESPOL%>%
  filter(Forma=='F005')%>%
  filter(Campo.de.conocimiento==materia)
Detalle_vertical_pilotaje_para_ESPOL2_6 = Detalle_vertical_pilotaje_para_ESPOL%>%
  filter(Forma=='F006')%>%
  filter(Campo.de.conocimiento==materia)
Detalle_vertical_pilotaje_para_ESPOL2_7 = Detalle_vertical_pilotaje_para_ESPOL%>%
  filter(Forma=='F007')%>%
  filter(Campo.de.conocimiento==materia)
Detalle_vertical_pilotaje_para_ESPOL2_8 = Detalle_vertical_pilotaje_para_ESPOL%>%
  filter(Forma=='F008')%>%
  filter(Campo.de.conocimiento==materia)
Detalle_vertical_pilotaje_para_ESPOL2_9 = Detalle_vertical_pilotaje_para_ESPOL%>%
  filter(Forma=='F009')%>%
  filter(Campo.de.conocimiento==materia)
Detalle_vertical_pilotaje_para_ESPOL2_10 = Detalle_vertical_pilotaje_para_ESPOL%>%
  filter(Forma=='F010')%>%
  filter(Campo.de.conocimiento==materia)
Detalle_vertical_pilotaje_para_ESPOL2_11 = Detalle_vertical_pilotaje_para_ESPOL%>%
  filter(Forma=='F011')%>%
  filter(Campo.de.conocimiento==materia)
Detalle_vertical_pilotaje_para_ESPOL2_12 = Detalle_vertical_pilotaje_para_ESPOL%>%
  filter(Forma=='F012')%>%
  filter(Campo.de.conocimiento==materia)


Base1 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_1$Codigo.Estudiante,
                   Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_1$Pregunta,
                   `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_1$C.I)

Base1$Correcta[Base1$C.I=='C'] <- 1
Base1$Correcta[Base1$C.I=='I'] <- 0
Base1 <- Base1[,-3]
CaractIndividuo1 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_1$`Codigo.Estudiante`,
                              Forma=Detalle_vertical_pilotaje_para_ESPOL2_1$Forma,
                              zona=Detalle_vertical_pilotaje_para_ESPOL2_1$zona,
                              amie=Detalle_vertical_pilotaje_para_ESPOL2_1$amie,
                              distrito=Detalle_vertical_pilotaje_para_ESPOL2_1$distrito,
                              sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_1$sostenimiento)




B1 <- pivot_wider(Base1, names_from ='Pregunta', values_from = 'Correcta')
C1 <- merge(x=B1, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')

C1$distrito[is.na(C1$distrito)] <- 'No registrado'



Forma_F1 <- C1


Forma_F1[,which(colMeans(is.na(Forma_F1))>0)] <- NULL



fit1 <- rasch(Forma_F1[,-c(1,ncol(Forma_F1),ncol(Forma_F1)-1,ncol(Forma_F1)-2,ncol(Forma_F1)-3,ncol(Forma_F1)-4)])

ICC.fit1 <- plot(fit1, legend = TRUE, cx = "bottomright", lwd = 3,
                 cex.main = 1.5, cex.lab = 1.3, cex = 1.1)
ICC.fit1 <- as.data.frame(ICC.fit1) 
ICC.fit1.GATH <- gather(ICC.fit1, key=z)
ICC.fit1.DF <- data.frame(rep(ICC.fit1$z,ncol(Forma_F1)-6),ICC.fit1.GATH)
colnames(ICC.fit1.DF) <- c('x', 'Pregunta','value')
ICC.fit1.DF <- data.frame(ICC.fit1.DF, rep('F001',nrow(ICC.fit1.DF)))
colnames(ICC.fit1.DF) <- c('x', 'Pregunta','value','Forma')


#_________________________________________________________






Base2 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_2$Codigo.Estudiante,
                    Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_2$Pregunta,
                    `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_2$C.I)

Base2$Correcta[Base2$C.I=='C'] <- 1
Base2$Correcta[Base2$C.I=='I'] <- 0
Base2 <- Base2[,-3]
CaractIndividuo2 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_2$`Codigo.Estudiante`,
                               Forma=Detalle_vertical_pilotaje_para_ESPOL2_2$Forma,
                               zona=Detalle_vertical_pilotaje_para_ESPOL2_2$zona,
                               amie=Detalle_vertical_pilotaje_para_ESPOL2_2$amie,
                               distrito=Detalle_vertical_pilotaje_para_ESPOL2_2$distrito,
                               sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_2$sostenimiento)




B2 <- pivot_wider(Base2, names_from ='Pregunta', values_from = 'Correcta')
C2 <- merge(x=B2, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')

C2$distrito[is.na(C2$distrito)] <- 'No registrado'



Forma_F2 <- C2


Forma_F2[,which(colMeans(is.na(Forma_F2))>0)] <- NULL



fit2 <- rasch(Forma_F2[,-c(1,ncol(Forma_F2),ncol(Forma_F2)-1,ncol(Forma_F2)-2,ncol(Forma_F2)-3,ncol(Forma_F2)-4)])

ICC.fit2 <- plot(fit2, legend = TRUE, cx = "bottomright", lwd = 3,
                 cex.main = 1.5, cex.lab = 1.3, cex = 1.1)
ICC.fit2 <- as.data.frame(ICC.fit2) 
ICC.fit2.GATH <- gather(ICC.fit2, key=z)
ICC.fit2.DF <- data.frame(rep(ICC.fit2$z,ncol(Forma_F2)-6),ICC.fit2.GATH)
colnames(ICC.fit2.DF) <- c('x', 'Pregunta','value')
ICC.fit2.DF <- data.frame(ICC.fit2.DF, rep('F002',nrow(ICC.fit2.DF)))
colnames(ICC.fit2.DF) <- c('x', 'Pregunta','value','Forma')


#_3________________________________________________________




Base3 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_3$Codigo.Estudiante,
                    Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_3$Pregunta,
                    `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_3$C.I)

Base3$Correcta[Base3$C.I=='C'] <- 1
Base3$Correcta[Base3$C.I=='I'] <- 0
Base3 <- Base3[,-3]
CaractIndividuo3 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_3$`Codigo.Estudiante`,
                               Forma=Detalle_vertical_pilotaje_para_ESPOL2_3$Forma,
                               zona=Detalle_vertical_pilotaje_para_ESPOL2_3$zona,
                               amie=Detalle_vertical_pilotaje_para_ESPOL2_3$amie,
                               distrito=Detalle_vertical_pilotaje_para_ESPOL2_3$distrito,
                               sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_3$sostenimiento)




B3 <- pivot_wider(Base3, names_from ='Pregunta', values_from = 'Correcta')
C3 <- merge(x=B3, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')

C3$distrito[is.na(C3$distrito)] <- 'No registrado'



Forma_F3 <- C3


Forma_F3[,which(colMeans(is.na(Forma_F3))>0)] <- NULL



fit3 <- rasch(Forma_F3[,-c(1,ncol(Forma_F3),ncol(Forma_F3)-1,ncol(Forma_F3)-2,ncol(Forma_F3)-3,ncol(Forma_F3)-4)])

ICC.fit3 <- plot(fit3, legend = TRUE, cx = "bottomright", lwd = 3,
                 cex.main = 1.5, cex.lab = 1.3, cex = 1.1)
ICC.fit3 <- as.data.frame(ICC.fit3) 
ICC.fit3.GATH <- gather(ICC.fit3, key=z)
ICC.fit3.DF <- data.frame(rep(ICC.fit3$z,ncol(Forma_F3)-6),ICC.fit3.GATH)
colnames(ICC.fit3.DF) <- c('x', 'Pregunta','value')
ICC.fit3.DF <- data.frame(ICC.fit3.DF, rep('F003',nrow(ICC.fit3.DF)))
colnames(ICC.fit3.DF) <- c('x', 'Pregunta','value','Forma')

#____4_____________________________________________________



Base4 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_4$Codigo.Estudiante,
                    Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_4$Pregunta,
                    `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_4$C.I)

Base4$Correcta[Base4$C.I=='C'] <- 1
Base4$Correcta[Base4$C.I=='I'] <- 0
Base4 <- Base4[,-3]
CaractIndividuo4 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_4$`Codigo.Estudiante`,
                               Forma=Detalle_vertical_pilotaje_para_ESPOL2_4$Forma,
                               zona=Detalle_vertical_pilotaje_para_ESPOL2_4$zona,
                               amie=Detalle_vertical_pilotaje_para_ESPOL2_4$amie,
                               distrito=Detalle_vertical_pilotaje_para_ESPOL2_4$distrito,
                               sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_4$sostenimiento)




B4 <- pivot_wider(Base4, names_from ='Pregunta', values_from = 'Correcta')
C4 <- merge(x=B4, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')

C4$distrito[is.na(C4$distrito)] <- 'No registrado'



Forma_F4 <- C4


Forma_F4[,which(colMeans(is.na(Forma_F4))>0)] <- NULL



fit4 <- rasch(Forma_F4[,-c(1,ncol(Forma_F4),ncol(Forma_F4)-1,ncol(Forma_F4)-2,ncol(Forma_F4)-3,ncol(Forma_F4)-4)])

ICC.fit4 <- plot(fit4, legend = TRUE, cx = "bottomright", lwd = 4,
                 cex.main = 1.5, cex.lab = 1.4, cex = 1.1)
ICC.fit4 <- as.data.frame(ICC.fit4) 
ICC.fit4.GATH <- gather(ICC.fit4, key=z)
ICC.fit4.DF <- data.frame(rep(ICC.fit4$z,ncol(Forma_F4)-6),ICC.fit4.GATH)
colnames(ICC.fit4.DF) <- c('x', 'Pregunta','value')
ICC.fit4.DF <- data.frame(ICC.fit4.DF, rep('F004',nrow(ICC.fit4.DF)))
colnames(ICC.fit4.DF) <- c('x', 'Pregunta','value','Forma')
#_____5____________________________________________________


Base5 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_5$Codigo.Estudiante,
                    Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_5$Pregunta,
                    `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_5$C.I)

Base5$Correcta[Base5$C.I=='C'] <- 1
Base5$Correcta[Base5$C.I=='I'] <- 0
Base5 <- Base5[,-3]
CaractIndividuo5 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_5$`Codigo.Estudiante`,
                               Forma=Detalle_vertical_pilotaje_para_ESPOL2_5$Forma,
                               zona=Detalle_vertical_pilotaje_para_ESPOL2_5$zona,
                               amie=Detalle_vertical_pilotaje_para_ESPOL2_5$amie,
                               distrito=Detalle_vertical_pilotaje_para_ESPOL2_5$distrito,
                               sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_5$sostenimiento)




B5 <- pivot_wider(Base5, names_from ='Pregunta', values_from = 'Correcta')
C5 <- merge(x=B5, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')

C5$distrito[is.na(C5$distrito)] <- 'No registrado'



Forma_F5 <- C5


Forma_F5[,which(colMeans(is.na(Forma_F5))>0)] <- NULL



fit5 <- rasch(Forma_F5[,-c(1,ncol(Forma_F5),ncol(Forma_F5)-1,ncol(Forma_F5)-2,ncol(Forma_F5)-3,ncol(Forma_F5)-4)])

ICC.fit5 <- plot(fit5, legend = TRUE, cx = "bottomright", lwd = 5,
                 cex.main = 1.5, cex.lab = 1.5, cex = 1.1)
ICC.fit5 <- as.data.frame(ICC.fit5) 
ICC.fit5.GATH <- gather(ICC.fit5, key=z)
ICC.fit5.DF <- data.frame(rep(ICC.fit5$z,ncol(Forma_F5)-6),ICC.fit5.GATH)
colnames(ICC.fit5.DF) <- c('x', 'Pregunta','value')
ICC.fit5.DF <- data.frame(ICC.fit5.DF, rep('F005',nrow(ICC.fit5.DF)))
colnames(ICC.fit5.DF) <- c('x', 'Pregunta','value','Forma')
#_____6____________________________________________________


Base6 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_6$Codigo.Estudiante,
                    Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_6$Pregunta,
                    `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_6$C.I)

Base6$Correcta[Base6$C.I=='C'] <- 1
Base6$Correcta[Base6$C.I=='I'] <- 0
Base6 <- Base6[,-3]
CaractIndividuo6 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_6$`Codigo.Estudiante`,
                               Forma=Detalle_vertical_pilotaje_para_ESPOL2_6$Forma,
                               zona=Detalle_vertical_pilotaje_para_ESPOL2_6$zona,
                               amie=Detalle_vertical_pilotaje_para_ESPOL2_6$amie,
                               distrito=Detalle_vertical_pilotaje_para_ESPOL2_6$distrito,
                               sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_6$sostenimiento)




B6 <- pivot_wider(Base6, names_from ='Pregunta', values_from = 'Correcta')
C6 <- merge(x=B6, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')

C6$distrito[is.na(C6$distrito)] <- 'No registrado'



Forma_F6 <- C6


Forma_F6[,which(colMeans(is.na(Forma_F6))>0)] <- NULL



fit6 <- rasch(Forma_F6[,-c(1,ncol(Forma_F6),ncol(Forma_F6)-1,ncol(Forma_F6)-2,ncol(Forma_F6)-3,ncol(Forma_F6)-4)])

ICC.fit6 <- plot(fit6, legend = TRUE, cx = "bottomright", lwd = 6,
                 cex.main = 1.6, cex.lab = 1.6, cex = 1.1)
ICC.fit6 <- as.data.frame(ICC.fit6) 
ICC.fit6.GATH <- gather(ICC.fit6, key=z)
ICC.fit6.DF <- data.frame(rep(ICC.fit6$z,ncol(Forma_F6)-6),ICC.fit6.GATH)
colnames(ICC.fit6.DF) <- c('x', 'Pregunta','value')
ICC.fit6.DF <- data.frame(ICC.fit6.DF, rep('F006',nrow(ICC.fit6.DF)))
colnames(ICC.fit6.DF) <- c('x', 'Pregunta','value','Forma')

#_____7____________________________________________________




Base7 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_7$Codigo.Estudiante,
                    Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_7$Pregunta,
                    `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_7$C.I)

Base7$Correcta[Base7$C.I=='C'] <- 1
Base7$Correcta[Base7$C.I=='I'] <- 0
Base7 <- Base7[,-3]
CaractIndividuo7 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_7$`Codigo.Estudiante`,
                               Forma=Detalle_vertical_pilotaje_para_ESPOL2_7$Forma,
                               zona=Detalle_vertical_pilotaje_para_ESPOL2_7$zona,
                               amie=Detalle_vertical_pilotaje_para_ESPOL2_7$amie,
                               distrito=Detalle_vertical_pilotaje_para_ESPOL2_7$distrito,
                               sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_7$sostenimiento)




B7 <- pivot_wider(Base7, names_from ='Pregunta', values_from = 'Correcta')
C7 <- merge(x=B7, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')

C7$distrito[is.na(C7$distrito)] <- 'No registrado'



Forma_F7 <- C7


Forma_F7[,which(colMeans(is.na(Forma_F7))>0)] <- NULL



fit7 <- rasch(Forma_F7[,-c(1,ncol(Forma_F7),ncol(Forma_F7)-1,ncol(Forma_F7)-2,ncol(Forma_F7)-3,ncol(Forma_F7)-4)])

ICC.fit7 <- plot(fit7, legend = TRUE, cx = "bottomright", lwd = 7,
                 cex.main = 1.7, cex.lab = 1.7, cex = 1.1)
ICC.fit7 <- as.data.frame(ICC.fit7) 
ICC.fit7.GATH <- gather(ICC.fit7, key=z)
ICC.fit7.DF <- data.frame(rep(ICC.fit7$z,ncol(Forma_F7)-6),ICC.fit7.GATH)
colnames(ICC.fit7.DF) <- c('x', 'Pregunta','value')
ICC.fit7.DF <- data.frame(ICC.fit7.DF, rep('F007',nrow(ICC.fit7.DF)))
colnames(ICC.fit7.DF) <- c('x', 'Pregunta','value','Forma')

#_____8____________________________________________________

Base8 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_8$Codigo.Estudiante,
                    Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_8$Pregunta,
                    `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_8$C.I)

Base8$Correcta[Base8$C.I=='C'] <- 1
Base8$Correcta[Base8$C.I=='I'] <- 0
Base8 <- Base8[,-3]
CaractIndividuo8 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_8$`Codigo.Estudiante`,
                               Forma=Detalle_vertical_pilotaje_para_ESPOL2_8$Forma,
                               zona=Detalle_vertical_pilotaje_para_ESPOL2_8$zona,
                               amie=Detalle_vertical_pilotaje_para_ESPOL2_8$amie,
                               distrito=Detalle_vertical_pilotaje_para_ESPOL2_8$distrito,
                               sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_8$sostenimiento)




B8 <- pivot_wider(Base8, names_from ='Pregunta', values_from = 'Correcta')
C8 <- merge(x=B8, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')

C8$distrito[is.na(C8$distrito)] <- 'No registrado'



Forma_F8 <- C8


Forma_F8[,which(colMeans(is.na(Forma_F8))>0)] <- NULL



fit8 <- rasch(Forma_F8[,-c(1,ncol(Forma_F8),ncol(Forma_F8)-1,ncol(Forma_F8)-2,ncol(Forma_F8)-3,ncol(Forma_F8)-4)])

ICC.fit8 <- plot(fit8, legend = TRUE, cx = "bottomright", lwd = 8,
                 cex.main = 1.8, cex.lab = 1.8, cex = 1.1)
ICC.fit8 <- as.data.frame(ICC.fit8) 
ICC.fit8.GATH <- gather(ICC.fit8, key=z)
ICC.fit8.DF <- data.frame(rep(ICC.fit8$z,ncol(Forma_F8)-6),ICC.fit8.GATH)
colnames(ICC.fit8.DF) <- c('x', 'Pregunta','value')
ICC.fit8.DF <- data.frame(ICC.fit8.DF, rep('F008',nrow(ICC.fit8.DF)))
colnames(ICC.fit8.DF) <- c('x', 'Pregunta','value','Forma')
#_____9____________________________________________________

Base9 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_9$Codigo.Estudiante,
                    Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_9$Pregunta,
                    `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_9$C.I)

Base9$Correcta[Base9$C.I=='C'] <- 1
Base9$Correcta[Base9$C.I=='I'] <- 0
Base9 <- Base9[,-3]
CaractIndividuo9 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_9$`Codigo.Estudiante`,
                               Forma=Detalle_vertical_pilotaje_para_ESPOL2_9$Forma,
                               zona=Detalle_vertical_pilotaje_para_ESPOL2_9$zona,
                               amie=Detalle_vertical_pilotaje_para_ESPOL2_9$amie,
                               distrito=Detalle_vertical_pilotaje_para_ESPOL2_9$distrito,
                               sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_9$sostenimiento)




B9 <- pivot_wider(Base9, names_from ='Pregunta', values_from = 'Correcta')
C9 <- merge(x=B9, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')

C9$distrito[is.na(C9$distrito)] <- 'No registrado'



Forma_F9 <- C9


Forma_F9[,which(colMeans(is.na(Forma_F9))>0)] <- NULL



fit9 <- rasch(Forma_F9[,-c(1,ncol(Forma_F9),ncol(Forma_F9)-1,ncol(Forma_F9)-2,ncol(Forma_F9)-3,ncol(Forma_F9)-4)])

ICC.fit9 <- plot(fit9, legend = TRUE, cx = "bottomright", lwd = 9,
                 cex.main = 1.9, cex.lab = 1.9, cex = 1.1)
ICC.fit9 <- as.data.frame(ICC.fit9) 
ICC.fit9.GATH <- gather(ICC.fit9, key=z)
ICC.fit9.DF <- data.frame(rep(ICC.fit9$z,ncol(Forma_F9)-6),ICC.fit9.GATH)
colnames(ICC.fit9.DF) <- c('x', 'Pregunta','value')
ICC.fit9.DF <- data.frame(ICC.fit9.DF, rep('F009',nrow(ICC.fit9.DF)))
colnames(ICC.fit9.DF) <- c('x', 'Pregunta','value','Forma')
#_______10__________________________________________________



Base10 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_10$Codigo.Estudiante,
                    Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_10$Pregunta,
                    `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_10$C.I)

Base10$Correcta[Base10$C.I=='C'] <- 1
Base10$Correcta[Base10$C.I=='I'] <- 0
Base10 <- Base10[,-3]
CaractIndividuo10 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_10$`Codigo.Estudiante`,
                               Forma=Detalle_vertical_pilotaje_para_ESPOL2_10$Forma,
                               zona=Detalle_vertical_pilotaje_para_ESPOL2_10$zona,
                               amie=Detalle_vertical_pilotaje_para_ESPOL2_10$amie,
                               distrito=Detalle_vertical_pilotaje_para_ESPOL2_10$distrito,
                               sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_10$sostenimiento)




B10 <- pivot_wider(Base10, names_from ='Pregunta', values_from = 'Correcta')
C10 <- merge(x=B10, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')

C10$distrito[is.na(C10$distrito)] <- 'No registrado'



Forma_F10 <- C10


Forma_F10[,which(colMeans(is.na(Forma_F10))>0)] <- NULL



fit10 <- rasch(Forma_F10[,-c(1,ncol(Forma_F10),ncol(Forma_F10)-1,ncol(Forma_F10)-2,ncol(Forma_F10)-3,ncol(Forma_F10)-4)])

ICC.fit10 <- plot(fit10, legend = TRUE, cx = "bottomright", lwd = 10,
                 cex.main = 1.10, cex.lab = 1.10, cex = 1.1)
ICC.fit10 <- as.data.frame(ICC.fit10) 
ICC.fit10.GATH <- gather(ICC.fit10, key=z)
ICC.fit10.DF <- data.frame(rep(ICC.fit10$z,ncol(Forma_F10)-6),ICC.fit10.GATH)
colnames(ICC.fit10.DF) <- c('x', 'Pregunta','value')
ICC.fit10.DF <- data.frame(ICC.fit10.DF, rep('F010',nrow(ICC.fit10.DF)))
colnames(ICC.fit10.DF) <- c('x', 'Pregunta','value','Forma')
#______11___________________________________________________

Base11 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_11$Codigo.Estudiante,
                    Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_11$Pregunta,
                    `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_11$C.I)

Base11$Correcta[Base11$C.I=='C'] <- 1
Base11$Correcta[Base11$C.I=='I'] <- 0
Base11 <- Base11[,-3]
CaractIndividuo11 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_11$`Codigo.Estudiante`,
                               Forma=Detalle_vertical_pilotaje_para_ESPOL2_11$Forma,
                               zona=Detalle_vertical_pilotaje_para_ESPOL2_11$zona,
                               amie=Detalle_vertical_pilotaje_para_ESPOL2_11$amie,
                               distrito=Detalle_vertical_pilotaje_para_ESPOL2_11$distrito,
                               sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_11$sostenimiento)




B11 <- pivot_wider(Base11, names_from ='Pregunta', values_from = 'Correcta')
C11 <- merge(x=B11, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')

C11$distrito[is.na(C11$distrito)] <- 'No registrado'



Forma_F11 <- C11


Forma_F11[,which(colMeans(is.na(Forma_F11))>0)] <- NULL



fit11 <- rasch(Forma_F11[,-c(1,ncol(Forma_F11),ncol(Forma_F11)-1,ncol(Forma_F11)-2,ncol(Forma_F11)-3,ncol(Forma_F11)-4)])

ICC.fit11 <- plot(fit11, legend = TRUE, cx = "bottomright", lwd = 11,
                 cex.main = 1.11, cex.lab = 1.11, cex = 1.1)
ICC.fit11 <- as.data.frame(ICC.fit11) 
ICC.fit11.GATH <- gather(ICC.fit11, key=z)
ICC.fit11.DF <- data.frame(rep(ICC.fit11$z,ncol(Forma_F11)-6),ICC.fit11.GATH)
colnames(ICC.fit11.DF) <- c('x', 'Pregunta','value')   
ICC.fit11.DF <- data.frame(ICC.fit11.DF, rep('F011',nrow(ICC.fit11.DF)))
colnames(ICC.fit11.DF) <- c('x', 'Pregunta','value','Forma')
#________12_________________________________________________

Base12 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_12$Codigo.Estudiante,
                    Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_12$Pregunta,
                    `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_12$C.I)

Base12$Correcta[Base12$C.I=='C'] <- 1
Base12$Correcta[Base12$C.I=='I'] <- 0
Base12 <- Base12[,-3]
CaractIndividuo12 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_12$`Codigo.Estudiante`,
                               Forma=Detalle_vertical_pilotaje_para_ESPOL2_12$Forma,
                               zona=Detalle_vertical_pilotaje_para_ESPOL2_12$zona,
                               amie=Detalle_vertical_pilotaje_para_ESPOL2_12$amie,
                               distrito=Detalle_vertical_pilotaje_para_ESPOL2_12$distrito,
                               sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_12$sostenimiento)




B12 <- pivot_wider(Base12, names_from ='Pregunta', values_from = 'Correcta')
C12 <- merge(x=B12, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')

C12$distrito[is.na(C12$distrito)] <- 'No registrado'



Forma_F12 <- C12


Forma_F12[,which(colMeans(is.na(Forma_F12))>0)] <- NULL



fit12 <- rasch(Forma_F12[,-c(1,ncol(Forma_F12),ncol(Forma_F12)-1,ncol(Forma_F12)-2,ncol(Forma_F12)-3,ncol(Forma_F12)-4)])

ICC.fit12 <- plot(fit12, legend = TRUE, cx = "bottomright", lwd = 12,
                 cex.main = 1.12, cex.lab = 1.12, cex = 1.1)
ICC.fit12 <- as.data.frame(ICC.fit12) 
ICC.fit12.GATH <- gather(ICC.fit12, key=z)
ICC.fit12.DF <- data.frame(rep(ICC.fit12$z,ncol(Forma_F12)-6),ICC.fit12.GATH)
colnames(ICC.fit12.DF) <- c('x', 'Pregunta','value')
ICC.fit12.DF <- data.frame(ICC.fit12.DF, rep('F012',nrow(ICC.fit12.DF)))
colnames(ICC.fit12.DF) <- c('x', 'Pregunta','value','Forma')
#_________________________________________________________




highchart()%>%
  hc_add_series(ICC.fit1.DF, type='line', hcaes(x=round(x,3), y=round(value,3), group=as.factor(Pregunta)))%>%
  hc_add_series(ICC.fit2.DF, type='line', hcaes(x=round(x,3), y=round(value,3), group=as.factor(Pregunta)))%>%
  hc_add_series(ICC.fit3.DF, type='line', hcaes(x=round(x,3), y=round(value,3), group=as.factor(Pregunta)))%>%
  hc_add_series(ICC.fit4.DF, type='line', hcaes(x=round(x,3), y=round(value,3), group=as.factor(Pregunta)))%>%
  hc_add_series(ICC.fit5.DF, type='line', hcaes(x=round(x,3), y=round(value,3), group=as.factor(Pregunta)))%>%
  hc_add_series(ICC.fit6.DF, type='line', hcaes(x=round(x,3), y=round(value,3), group=as.factor(Pregunta)))%>%
  hc_add_series(ICC.fit7.DF, type='line', hcaes(x=round(x,3), y=round(value,3), group=as.factor(Pregunta)))%>%
  hc_add_series(ICC.fit8.DF, type='line', hcaes(x=round(x,3), y=round(value,3), group=as.factor(Pregunta)))%>%
  hc_add_series(ICC.fit9.DF, type='line', hcaes(x=round(x,3), y=round(value,3), group=as.factor(Pregunta)))%>%
  hc_add_series(ICC.fit10.DF, type='line', hcaes(x=round(x,3), y=round(value,3), group=as.factor(Pregunta)))%>%
  hc_add_series(ICC.fit11.DF, type='line', hcaes(x=round(x,3), y=round(value,3), group=as.factor(Pregunta)))%>%
  hc_add_series(ICC.fit12.DF, type='line', hcaes(x=round(x,3), y=round(value,3), group=as.factor(Pregunta)))%>%
  
  hc_legend(
    align = "right",
    verticalAlign = "top",
    shadow= FALSE,
    layout = "vertical",
    x = 0,
    y = 100)%>%
  hc_title(text='Curva Característica del Ítem')%>%
  hc_subtitle(text= paste0('Forma: Todas las formas. Campo de conocimiento: ', materia,'.'))%>%
  hc_xAxis(title=list(text="Habilidad"))%>%
  hc_yAxis(title=list(text="Probabilidad"),
           plotLines = list(list(
             value = 0.5,
             color = '#1D4B5E',
             dashStyle= 'shortdash',
             width = 3,
             zIndex = 4,
             label = list(text = "Selección al azar",
                          style = list( color = '#1D4B5E', fontWeight = 'bold' )))))%>%
  hc_tooltip(pointFormat = "{point.Pregunta} <br> Prob: {point.y} <br>  Habilidad: {point.x} <br> Forma: {point.Forma}",
             headerFormat= '{point.Pregunta}')%>%
  hc_exporting(enabled = TRUE,
               filename = paste0('Curva Característica del Ítem - Forma: Todas las formas. Campo de conocimiento: ', materia,'.'))%>% 
  hc_credits(
    enabled = TRUE,
    text = "Fuente: Senescyt",
    href = "https://www.educacionsuperior.gob.ec/"
  )
}


#_______________________________________________________________


TodasFormas2 <- function(materia){
  Detalle_vertical_pilotaje_para_ESPOL2_1 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F001')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_2 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F002')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_3 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F003')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_4 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F004')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_5 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F005')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_6 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F006')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_7 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F007')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_8 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F008')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_9 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F009')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_10 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F010')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_11 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F011')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_12 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F012')%>%
    filter(Campo.de.conocimiento==materia)
  
  
  Base1 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_1$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_1$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_1$C.I)
  
  Base1$Correcta[Base1$C.I=='C'] <- 1
  Base1$Correcta[Base1$C.I=='I'] <- 0
  Base1 <- Base1[,-3]
  CaractIndividuo1 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_1$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_1$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_1$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_1$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_1$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_1$sostenimiento)
  
  
  
  
  B1 <- pivot_wider(Base1, names_from ='Pregunta', values_from = 'Correcta')
  C1 <- merge(x=B1, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C1$distrito[is.na(C1$distrito)] <- 'No registrado'
  
  
  
  Forma_F1 <- C1
  
  
  Forma_F1[,which(colMeans(is.na(Forma_F1))>0)] <- NULL
  
  
  
  fit1 <- rasch(Forma_F1[,-c(1,ncol(Forma_F1),ncol(Forma_F1)-1,ncol(Forma_F1)-2,ncol(Forma_F1)-3,ncol(Forma_F1)-4)])
  
  ICC.fit1 <- plot(fit1, type = "IIC", annot = FALSE, lwd = 3, cex.main = 1.5,
                   cex.lab = 1.3)
  ICC.fit1 <- as.data.frame(ICC.fit1) 
  ICC.fit1.GATH <- gather(ICC.fit1, key=z)
  ICC.fit1.DF <- data.frame(rep(ICC.fit1$z,ncol(Forma_F1)-6),ICC.fit1.GATH)
  colnames(ICC.fit1.DF) <- c('x', 'Pregunta','value')
  ICC.fit1.DF <- data.frame(ICC.fit1.DF, rep('F001',nrow(ICC.fit1.DF)))   
  colnames(ICC.fit1.DF) <- c('x', 'Pregunta','value','Forma')
  
  
  #_________________________________________________________
  
  
  
  
  
  
  Base2 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_2$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_2$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_2$C.I)
  
  Base2$Correcta[Base2$C.I=='C'] <- 1
  Base2$Correcta[Base2$C.I=='I'] <- 0
  Base2 <- Base2[,-3]
  CaractIndividuo2 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_2$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_2$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_2$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_2$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_2$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_2$sostenimiento)
  
  
  
  
  B2 <- pivot_wider(Base2, names_from ='Pregunta', values_from = 'Correcta')
  C2 <- merge(x=B2, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C2$distrito[is.na(C2$distrito)] <- 'No registrado'
  
  
  
  Forma_F2 <- C2
  
  
  Forma_F2[,which(colMeans(is.na(Forma_F2))>0)] <- NULL
  
  
  
  fit2 <- rasch(Forma_F2[,-c(1,ncol(Forma_F2),ncol(Forma_F2)-1,ncol(Forma_F2)-2,ncol(Forma_F2)-3,ncol(Forma_F2)-4)])
  
  ICC.fit2 <- plot(fit2,type = "IIC", annot = FALSE, lwd = 3, cex.main = 1.5,
                   cex.lab = 1.3)
  ICC.fit2 <- as.data.frame(ICC.fit2) 
  ICC.fit2.GATH <- gather(ICC.fit2, key=z)
  ICC.fit2.DF <- data.frame(rep(ICC.fit2$z,ncol(Forma_F2)-6),ICC.fit2.GATH)
  colnames(ICC.fit2.DF) <- c('x', 'Pregunta','value')
  ICC.fit2.DF <- data.frame(ICC.fit2.DF, rep('F002',nrow(ICC.fit2.DF)))   
  colnames(ICC.fit2.DF) <- c('x', 'Pregunta','value','Forma')
  
  
  #_3________________________________________________________
  
  
  
  
  Base3 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_3$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_3$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_3$C.I)
  
  Base3$Correcta[Base3$C.I=='C'] <- 1
  Base3$Correcta[Base3$C.I=='I'] <- 0
  Base3 <- Base3[,-3]
  CaractIndividuo3 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_3$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_3$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_3$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_3$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_3$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_3$sostenimiento)
  
  
  
  
  B3 <- pivot_wider(Base3, names_from ='Pregunta', values_from = 'Correcta')
  C3 <- merge(x=B3, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C3$distrito[is.na(C3$distrito)] <- 'No registrado'
  
  
  
  Forma_F3 <- C3
  
  
  Forma_F3[,which(colMeans(is.na(Forma_F3))>0)] <- NULL
  
  
  
  fit3 <- rasch(Forma_F3[,-c(1,ncol(Forma_F3),ncol(Forma_F3)-1,ncol(Forma_F3)-2,ncol(Forma_F3)-3,ncol(Forma_F3)-4)])
  
  ICC.fit3 <- plot(fit3, type = "IIC", annot = FALSE, lwd = 3, cex.main = 1.5,
                   cex.lab = 1.3)
  ICC.fit3 <- as.data.frame(ICC.fit3) 
  ICC.fit3.GATH <- gather(ICC.fit3, key=z)
  ICC.fit3.DF <- data.frame(rep(ICC.fit3$z,ncol(Forma_F3)-6),ICC.fit3.GATH)
  colnames(ICC.fit3.DF) <- c('x', 'Pregunta','value')
  ICC.fit3.DF <- data.frame(ICC.fit3.DF, rep('F003',nrow(ICC.fit3.DF)))   
  colnames(ICC.fit3.DF) <- c('x', 'Pregunta','value','Forma')
  
  #____4_____________________________________________________
  
  
  
  Base4 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_4$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_4$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_4$C.I)
  
  Base4$Correcta[Base4$C.I=='C'] <- 1
  Base4$Correcta[Base4$C.I=='I'] <- 0
  Base4 <- Base4[,-3]
  CaractIndividuo4 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_4$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_4$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_4$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_4$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_4$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_4$sostenimiento)
  
  
  
  
  B4 <- pivot_wider(Base4, names_from ='Pregunta', values_from = 'Correcta')
  C4 <- merge(x=B4, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C4$distrito[is.na(C4$distrito)] <- 'No registrado'
  
  
  
  Forma_F4 <- C4
  
  
  Forma_F4[,which(colMeans(is.na(Forma_F4))>0)] <- NULL
  
  
  
  fit4 <- rasch(Forma_F4[,-c(1,ncol(Forma_F4),ncol(Forma_F4)-1,ncol(Forma_F4)-2,ncol(Forma_F4)-3,ncol(Forma_F4)-4)])
  
  ICC.fit4 <- plot(fit4, type = "IIC", annot = FALSE, lwd = 3, cex.main = 1.5,
                   cex.lab = 1.3)
  ICC.fit4 <- as.data.frame(ICC.fit4) 
  ICC.fit4.GATH <- gather(ICC.fit4, key=z)
  ICC.fit4.DF <- data.frame(rep(ICC.fit4$z,ncol(Forma_F4)-6),ICC.fit4.GATH)
  colnames(ICC.fit4.DF) <- c('x', 'Pregunta','value')
  ICC.fit4.DF <- data.frame(ICC.fit4.DF, rep('F004',nrow(ICC.fit4.DF)))   
  colnames(ICC.fit4.DF) <- c('x', 'Pregunta','value','Forma')
  #_____5____________________________________________________
  
  
  Base5 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_5$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_5$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_5$C.I)
  
  Base5$Correcta[Base5$C.I=='C'] <- 1
  Base5$Correcta[Base5$C.I=='I'] <- 0
  Base5 <- Base5[,-3]
  CaractIndividuo5 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_5$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_5$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_5$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_5$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_5$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_5$sostenimiento)
  
  
  
  
  B5 <- pivot_wider(Base5, names_from ='Pregunta', values_from = 'Correcta')
  C5 <- merge(x=B5, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C5$distrito[is.na(C5$distrito)] <- 'No registrado'
  
  
  
  Forma_F5 <- C5
  
  
  Forma_F5[,which(colMeans(is.na(Forma_F5))>0)] <- NULL
  
  
  
  fit5 <- rasch(Forma_F5[,-c(1,ncol(Forma_F5),ncol(Forma_F5)-1,ncol(Forma_F5)-2,ncol(Forma_F5)-3,ncol(Forma_F5)-4)])
  
  ICC.fit5 <- plot(fit5, type = "IIC", annot = FALSE, lwd = 3, cex.main = 1.5,
                   cex.lab = 1.3)
  ICC.fit5 <- as.data.frame(ICC.fit5) 
  ICC.fit5.GATH <- gather(ICC.fit5, key=z)
  ICC.fit5.DF <- data.frame(rep(ICC.fit5$z,ncol(Forma_F5)-6),ICC.fit5.GATH)
  colnames(ICC.fit5.DF) <- c('x', 'Pregunta','value')
  ICC.fit5.DF <- data.frame(ICC.fit5.DF, rep('F005',nrow(ICC.fit5.DF)))   
  colnames(ICC.fit5.DF) <- c('x', 'Pregunta','value','Forma')
  #_____6____________________________________________________
  
  
  Base6 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_6$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_6$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_6$C.I)
  
  Base6$Correcta[Base6$C.I=='C'] <- 1
  Base6$Correcta[Base6$C.I=='I'] <- 0
  Base6 <- Base6[,-3]
  CaractIndividuo6 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_6$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_6$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_6$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_6$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_6$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_6$sostenimiento)
  
  
  
  
  B6 <- pivot_wider(Base6, names_from ='Pregunta', values_from = 'Correcta')
  C6 <- merge(x=B6, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C6$distrito[is.na(C6$distrito)] <- 'No registrado'
  
  
  
  Forma_F6 <- C6
  
  
  Forma_F6[,which(colMeans(is.na(Forma_F6))>0)] <- NULL
  
  
  
  fit6 <- rasch(Forma_F6[,-c(1,ncol(Forma_F6),ncol(Forma_F6)-1,ncol(Forma_F6)-2,ncol(Forma_F6)-3,ncol(Forma_F6)-4)])
  
  ICC.fit6 <- plot(fit6,type = "IIC", annot = FALSE, lwd = 3, cex.main = 1.5,
                   cex.lab = 1.3)
  ICC.fit6 <- as.data.frame(ICC.fit6) 
  ICC.fit6.GATH <- gather(ICC.fit6, key=z)
  ICC.fit6.DF <- data.frame(rep(ICC.fit6$z,ncol(Forma_F6)-6),ICC.fit6.GATH)
  colnames(ICC.fit6.DF) <- c('x', 'Pregunta','value')
  ICC.fit6.DF <- data.frame(ICC.fit6.DF, rep('F006',nrow(ICC.fit6.DF)))   
  colnames(ICC.fit6.DF) <- c('x', 'Pregunta','value','Forma')
  #_____7____________________________________________________
  
  
  
  
  Base7 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_7$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_7$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_7$C.I)
  
  Base7$Correcta[Base7$C.I=='C'] <- 1
  Base7$Correcta[Base7$C.I=='I'] <- 0
  Base7 <- Base7[,-3]
  CaractIndividuo7 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_7$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_7$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_7$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_7$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_7$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_7$sostenimiento)
  
  
  
  
  B7 <- pivot_wider(Base7, names_from ='Pregunta', values_from = 'Correcta')
  C7 <- merge(x=B7, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C7$distrito[is.na(C7$distrito)] <- 'No registrado'
  
  
  
  Forma_F7 <- C7
  
  
  Forma_F7[,which(colMeans(is.na(Forma_F7))>0)] <- NULL
  
  
  
  fit7 <- rasch(Forma_F7[,-c(1,ncol(Forma_F7),ncol(Forma_F7)-1,ncol(Forma_F7)-2,ncol(Forma_F7)-3,ncol(Forma_F7)-4)])
  
  ICC.fit7 <- plot(fit7,type = "IIC", annot = FALSE, lwd = 3, cex.main = 1.5,
                   cex.lab = 1.3)
  ICC.fit7 <- as.data.frame(ICC.fit7) 
  ICC.fit7.GATH <- gather(ICC.fit7, key=z)
  ICC.fit7.DF <- data.frame(rep(ICC.fit7$z,ncol(Forma_F7)-6),ICC.fit7.GATH)
  colnames(ICC.fit7.DF) <- c('x', 'Pregunta','value')
  ICC.fit7.DF <- data.frame(ICC.fit7.DF, rep('F007',nrow(ICC.fit7.DF)))   
  colnames(ICC.fit7.DF) <- c('x', 'Pregunta','value','Forma')
  #_____8____________________________________________________
  
  Base8 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_8$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_8$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_8$C.I)
  
  Base8$Correcta[Base8$C.I=='C'] <- 1
  Base8$Correcta[Base8$C.I=='I'] <- 0
  Base8 <- Base8[,-3]
  CaractIndividuo8 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_8$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_8$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_8$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_8$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_8$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_8$sostenimiento)
  
  
  
  
  B8 <- pivot_wider(Base8, names_from ='Pregunta', values_from = 'Correcta')
  C8 <- merge(x=B8, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C8$distrito[is.na(C8$distrito)] <- 'No registrado'
  
  
  
  Forma_F8 <- C8
  
  
  Forma_F8[,which(colMeans(is.na(Forma_F8))>0)] <- NULL
  
  
  
  fit8 <- rasch(Forma_F8[,-c(1,ncol(Forma_F8),ncol(Forma_F8)-1,ncol(Forma_F8)-2,ncol(Forma_F8)-3,ncol(Forma_F8)-4)])
  
  ICC.fit8 <- plot(fit8, type = "IIC", annot = FALSE, lwd = 3, cex.main = 1.5,
                   cex.lab = 1.3)
  ICC.fit8 <- as.data.frame(ICC.fit8) 
  ICC.fit8.GATH <- gather(ICC.fit8, key=z)
  ICC.fit8.DF <- data.frame(rep(ICC.fit8$z,ncol(Forma_F8)-6),ICC.fit8.GATH)
  colnames(ICC.fit8.DF) <- c('x', 'Pregunta','value')
  ICC.fit8.DF <- data.frame(ICC.fit8.DF, rep('F008',nrow(ICC.fit8.DF)))   
  colnames(ICC.fit8.DF) <- c('x', 'Pregunta','value','Forma')
  #_____9____________________________________________________
  
  Base9 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_9$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_9$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_9$C.I)
  
  Base9$Correcta[Base9$C.I=='C'] <- 1
  Base9$Correcta[Base9$C.I=='I'] <- 0
  Base9 <- Base9[,-3]
  CaractIndividuo9 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_9$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_9$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_9$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_9$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_9$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_9$sostenimiento)
  
  
  
  
  B9 <- pivot_wider(Base9, names_from ='Pregunta', values_from = 'Correcta')
  C9 <- merge(x=B9, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C9$distrito[is.na(C9$distrito)] <- 'No registrado'
  
  
  
  Forma_F9 <- C9
  
  
  Forma_F9[,which(colMeans(is.na(Forma_F9))>0)] <- NULL
  
  
  
  fit9 <- rasch(Forma_F9[,-c(1,ncol(Forma_F9),ncol(Forma_F9)-1,ncol(Forma_F9)-2,ncol(Forma_F9)-3,ncol(Forma_F9)-4)])
  
  ICC.fit9 <- plot(fit9, type = "IIC", annot = FALSE, lwd = 3, cex.main = 1.5,
                   cex.lab = 1.3)
  ICC.fit9 <- as.data.frame(ICC.fit9) 
  ICC.fit9.GATH <- gather(ICC.fit9, key=z)
  ICC.fit9.DF <- data.frame(rep(ICC.fit9$z,ncol(Forma_F9)-6),ICC.fit9.GATH)
  colnames(ICC.fit9.DF) <- c('x', 'Pregunta','value')
  ICC.fit9.DF <- data.frame(ICC.fit9.DF, rep('F009',nrow(ICC.fit9.DF)))   
  colnames(ICC.fit9.DF) <- c('x', 'Pregunta','value','Forma')
  #_______10__________________________________________________
  
  
  
  Base10 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_10$Codigo.Estudiante,
                       Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_10$Pregunta,
                       `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_10$C.I)
  
  Base10$Correcta[Base10$C.I=='C'] <- 1
  Base10$Correcta[Base10$C.I=='I'] <- 0
  Base10 <- Base10[,-3]
  CaractIndividuo10 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_10$`Codigo.Estudiante`,
                                  Forma=Detalle_vertical_pilotaje_para_ESPOL2_10$Forma,
                                  zona=Detalle_vertical_pilotaje_para_ESPOL2_10$zona,
                                  amie=Detalle_vertical_pilotaje_para_ESPOL2_10$amie,
                                  distrito=Detalle_vertical_pilotaje_para_ESPOL2_10$distrito,
                                  sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_10$sostenimiento)
  
  
  
  
  B10 <- pivot_wider(Base10, names_from ='Pregunta', values_from = 'Correcta')
  C10 <- merge(x=B10, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C10$distrito[is.na(C10$distrito)] <- 'No registrado'
  
  
  
  Forma_F10 <- C10
  
  
  Forma_F10[,which(colMeans(is.na(Forma_F10))>0)] <- NULL
  
  
  
  fit10 <- rasch(Forma_F10[,-c(1,ncol(Forma_F10),ncol(Forma_F10)-1,ncol(Forma_F10)-2,ncol(Forma_F10)-3,ncol(Forma_F10)-4)])
  
  ICC.fit10 <- plot(fit10, type = "IIC", annot = FALSE, lwd = 3, cex.main = 1.5,
                    cex.lab = 1.3)
  ICC.fit10 <- as.data.frame(ICC.fit10) 
  ICC.fit10.GATH <- gather(ICC.fit10, key=z)
  ICC.fit10.DF <- data.frame(rep(ICC.fit10$z,ncol(Forma_F10)-6),ICC.fit10.GATH)
  colnames(ICC.fit10.DF) <- c('x', 'Pregunta','value')
  ICC.fit10.DF <- data.frame(ICC.fit10.DF, rep('F010',nrow(ICC.fit10.DF)))   
  colnames(ICC.fit10.DF) <- c('x', 'Pregunta','value','Forma')
  #______11___________________________________________________
  
  Base11 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_11$Codigo.Estudiante,
                       Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_11$Pregunta,
                       `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_11$C.I)
  
  Base11$Correcta[Base11$C.I=='C'] <- 1
  Base11$Correcta[Base11$C.I=='I'] <- 0
  Base11 <- Base11[,-3]
  CaractIndividuo11 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_11$`Codigo.Estudiante`,
                                  Forma=Detalle_vertical_pilotaje_para_ESPOL2_11$Forma,
                                  zona=Detalle_vertical_pilotaje_para_ESPOL2_11$zona,
                                  amie=Detalle_vertical_pilotaje_para_ESPOL2_11$amie,
                                  distrito=Detalle_vertical_pilotaje_para_ESPOL2_11$distrito,
                                  sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_11$sostenimiento)
  
  
  
  
  B11 <- pivot_wider(Base11, names_from ='Pregunta', values_from = 'Correcta')
  C11 <- merge(x=B11, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C11$distrito[is.na(C11$distrito)] <- 'No registrado'
  
  
  
  Forma_F11 <- C11
  
  
  Forma_F11[,which(colMeans(is.na(Forma_F11))>0)] <- NULL
  
  
  
  fit11 <- rasch(Forma_F11[,-c(1,ncol(Forma_F11),ncol(Forma_F11)-1,ncol(Forma_F11)-2,ncol(Forma_F11)-3,ncol(Forma_F11)-4)])
  
  ICC.fit11 <- plot(fit11, type = "IIC", annot = FALSE, lwd = 3, cex.main = 1.5,
                    cex.lab = 1.3)
  ICC.fit11 <- as.data.frame(ICC.fit11) 
  ICC.fit11.GATH <- gather(ICC.fit11, key=z)
  ICC.fit11.DF <- data.frame(rep(ICC.fit11$z,ncol(Forma_F11)-6),ICC.fit11.GATH)
  colnames(ICC.fit11.DF) <- c('x', 'Pregunta','value')   
  ICC.fit11.DF <- data.frame(ICC.fit11.DF, rep('F011',nrow(ICC.fit11.DF)))   
  colnames(ICC.fit11.DF) <- c('x', 'Pregunta','value','Forma')
  #________12_________________________________________________
  
  Base12 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_12$Codigo.Estudiante,
                       Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_12$Pregunta,
                       `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_12$C.I)
  
  Base12$Correcta[Base12$C.I=='C'] <- 1
  Base12$Correcta[Base12$C.I=='I'] <- 0
  Base12 <- Base12[,-3]
  CaractIndividuo12 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_12$`Codigo.Estudiante`,
                                  Forma=Detalle_vertical_pilotaje_para_ESPOL2_12$Forma,
                                  zona=Detalle_vertical_pilotaje_para_ESPOL2_12$zona,
                                  amie=Detalle_vertical_pilotaje_para_ESPOL2_12$amie,
                                  distrito=Detalle_vertical_pilotaje_para_ESPOL2_12$distrito,
                                  sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_12$sostenimiento)
  
  
  
  
  B12 <- pivot_wider(Base12, names_from ='Pregunta', values_from = 'Correcta')
  C12 <- merge(x=B12, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C12$distrito[is.na(C12$distrito)] <- 'No registrado'
  
  
  
  Forma_F12 <- C12
  
  
  Forma_F12[,which(colMeans(is.na(Forma_F12))>0)] <- NULL
  
  
  
  fit12 <- rasch(Forma_F12[,-c(1,ncol(Forma_F12),ncol(Forma_F12)-1,ncol(Forma_F12)-2,ncol(Forma_F12)-3,ncol(Forma_F12)-4)])
  
  ICC.fit12 <- plot(fit12, type = "IIC", annot = FALSE, lwd = 3, cex.main = 1.5,
                    cex.lab = 1.3)
  ICC.fit12 <- as.data.frame(ICC.fit12) 
  ICC.fit12.GATH <- gather(ICC.fit12, key=z)
  ICC.fit12.DF <- data.frame(rep(ICC.fit12$z,ncol(Forma_F12)-6),ICC.fit12.GATH)
  colnames(ICC.fit12.DF) <- c('x', 'Pregunta','value')
  ICC.fit12.DF <- data.frame(ICC.fit12.DF, rep('F012',nrow(ICC.fit12.DF)))
  colnames(ICC.fit12.DF) <- c('x', 'Pregunta','value','Forma')
  
  #_________________________________________________________
  
  
  Bas <- rbind(ICC.fit1.DF,ICC.fit2.DF,ICC.fit3.DF,
               ICC.fit4.DF,
               ICC.fit5.DF,
               ICC.fit6.DF,
               ICC.fit7.DF,
               ICC.fit8.DF,
               ICC.fit9.DF,
               ICC.fit10.DF,
               ICC.fit11.DF,
               ICC.fit12.DF)
  Names <- c(
  rep('F001',nrow(ICC.fit1.DF)),
  rep('F002',nrow(ICC.fit2.DF)),
  rep('F003',nrow(ICC.fit3.DF)),
  rep('F004',nrow(ICC.fit4.DF)),
  rep('F005',nrow(ICC.fit5.DF)),
  rep('F006',nrow(ICC.fit6.DF)),
  rep('F007',nrow(ICC.fit7.DF)),
  rep('F008',nrow(ICC.fit8.DF)),
  rep('F009',nrow(ICC.fit9.DF)),
  rep('F010',nrow(ICC.fit10.DF)),
  rep('F011',nrow(ICC.fit11.DF)),
  rep('F012',nrow(ICC.fit12.DF)))
  
  BaseUnida <- data.frame(Bas,Names)

library(tidyr)
  highchart()%>%
    hc_add_series(ICC.fit1.DF, type='line', hcaes(x=x, y=round(value,5), group=as.factor(Pregunta)))%>%  
    hc_add_series(ICC.fit2.DF, type='line', hcaes(x=x, y=round(value,5), group=as.factor(Pregunta)))%>%  
    hc_add_series(ICC.fit3.DF, type='line', hcaes(x=x, y=round(value,5), group=as.factor(Pregunta)))%>%  
    hc_add_series(ICC.fit4.DF, type='line', hcaes(x=x, y=round(value,5), group=as.factor(Pregunta)))%>%  
    hc_add_series(ICC.fit5.DF, type='line', hcaes(x=x, y=round(value,5), group=as.factor(Pregunta)))%>%  
    hc_add_series(ICC.fit6.DF, type='line', hcaes(x=x, y=round(value,5), group=as.factor(Pregunta)))%>%  
    hc_add_series(ICC.fit7.DF, type='line', hcaes(x=x, y=round(value,5), group=as.factor(Pregunta)))%>%  
    hc_add_series(ICC.fit8.DF, type='line', hcaes(x=x, y=round(value,5), group=as.factor(Pregunta)))%>%  
    hc_add_series(ICC.fit9.DF, type='line', hcaes(x=x, y=round(value,5), group=as.factor(Pregunta)))%>%  
    hc_add_series(ICC.fit10.DF, type='line', hcaes(x=x, y=round(value,5), group=as.factor(Pregunta)))%>% 
    hc_add_series(ICC.fit11.DF, type='line', hcaes(x=x, y=round(value,5), group=as.factor(Pregunta)))%>% 
    hc_add_series(ICC.fit12.DF, type='line', hcaes(x=x, y=round(value,5), group=as.factor(Pregunta)))%>% 
    
 
    hc_legend(
      align = "right",
      verticalAlign = "top",
      shadow= FALSE,
      layout = "vertical",
      x = 0,
      y = 100)%>%
    hc_title(text='Curva de Información del Ítem')%>%
    hc_subtitle(text= paste0('Forma: Todas las formas. Campo de conocimiento: ', materia,'.'))%>%
    hc_xAxis(title=list(text="Habilidad"))%>%
    hc_yAxis(title=list(text="Información"),
             plotLines = list(list(
               value = 0.5,
               color = '#1D4B5E',
               dashStyle= 'shortdash',
               width = 3,
               zIndex = 4,
               label = list(text = "Selección al azar",
                            style = list( color = '#1D4B5E', fontWeight = 'bold' )))))%>%
    hc_tooltip(pointFormat = "{point.Pregunta} <br> Información: {point.y} <br>  Habilidad: {point.x} <br> Forma: {point.Forma}",
               headerFormat= '{point.Pregunta}')%>%
    hc_exporting(enabled = TRUE,
                 filename = paste0('Curva de Información del Ítem - Forma: Todas las formas. Campo de conocimiento: ', materia,'.'))%>% 
    hc_credits(
      enabled = TRUE,
      text = "Fuente: Senescyt",
      href = "https://www.educacionsuperior.gob.ec/"
    )
}

#______________________

TodasFormas3 <- function(materia){
  Detalle_vertical_pilotaje_para_ESPOL2_1 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F001')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_2 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F002')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_3 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F003')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_4 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F004')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_5 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F005')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_6 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F006')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_7 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F007')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_8 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F008')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_9 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F009')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_10 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F010')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_11 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F011')%>%
    filter(Campo.de.conocimiento==materia)
  Detalle_vertical_pilotaje_para_ESPOL2_12 = Detalle_vertical_pilotaje_para_ESPOL%>%
    filter(Forma=='F012')%>%
    filter(Campo.de.conocimiento==materia)
  
  
  Base1 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_1$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_1$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_1$C.I)
  
  Base1$Correcta[Base1$C.I=='C'] <- 1
  Base1$Correcta[Base1$C.I=='I'] <- 0
  Base1 <- Base1[,-3]
  CaractIndividuo1 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_1$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_1$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_1$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_1$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_1$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_1$sostenimiento)
  
  
  
  
  B1 <- pivot_wider(Base1, names_from ='Pregunta', values_from = 'Correcta')
  C1 <- merge(x=B1, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C1$distrito[is.na(C1$distrito)] <- 'No registrado'
  
  
  
  Forma_F1 <- C1
  
  
  Forma_F1[,which(colMeans(is.na(Forma_F1))>0)] <- NULL
  
  
  
  fit1 <- rasch(Forma_F1[,-c(1,ncol(Forma_F1),ncol(Forma_F1)-1,ncol(Forma_F1)-2,ncol(Forma_F1)-3,ncol(Forma_F1)-4)])
  
  COEF1 <- coef(fit1, prob = TRUE, order = TRUE)
  Coeff1 <- data.frame(rownames(COEF1),COEF1)
  colnames(Coeff1) <- c('Pregunta','Dificultad','Discriminación','P(x=1|z=0)')
  
  #_________________________________________________________
  
  
  
  
  
  
  Base2 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_2$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_2$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_2$C.I)
  
  Base2$Correcta[Base2$C.I=='C'] <- 1
  Base2$Correcta[Base2$C.I=='I'] <- 0
  Base2 <- Base2[,-3]
  CaractIndividuo2 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_2$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_2$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_2$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_2$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_2$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_2$sostenimiento)
  
  
  
  
  B2 <- pivot_wider(Base2, names_from ='Pregunta', values_from = 'Correcta')
  C2 <- merge(x=B2, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C2$distrito[is.na(C2$distrito)] <- 'No registrado'
  
  
  
  Forma_F2 <- C2
  
  
  Forma_F2[,which(colMeans(is.na(Forma_F2))>0)] <- NULL
  
  
  
  fit2 <- rasch(Forma_F2[,-c(1,ncol(Forma_F2),ncol(Forma_F2)-1,ncol(Forma_F2)-2,ncol(Forma_F2)-3,ncol(Forma_F2)-4)])
  
  
  COEF2 <- coef(fit2, prob = TRUE, order = TRUE)
  Coeff2 <- data.frame(rownames(COEF2),COEF2)
  colnames(Coeff2) <- c('Pregunta','Dificultad','Discriminación','P(x=1|z=0)')
  
  #_3________________________________________________________
  
  
  
  
  Base3 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_3$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_3$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_3$C.I)
  
  Base3$Correcta[Base3$C.I=='C'] <- 1
  Base3$Correcta[Base3$C.I=='I'] <- 0
  Base3 <- Base3[,-3]
  CaractIndividuo3 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_3$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_3$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_3$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_3$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_3$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_3$sostenimiento)
  
  
  
  
  B3 <- pivot_wider(Base3, names_from ='Pregunta', values_from = 'Correcta')
  C3 <- merge(x=B3, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C3$distrito[is.na(C3$distrito)] <- 'No registrado'
  
  
  
  Forma_F3 <- C3
  
  
  Forma_F3[,which(colMeans(is.na(Forma_F3))>0)] <- NULL
  
  
  
  fit3 <- rasch(Forma_F3[,-c(1,ncol(Forma_F3),ncol(Forma_F3)-1,ncol(Forma_F3)-2,ncol(Forma_F3)-3,ncol(Forma_F3)-4)])
  
  
  COEF3 <- coef(fit3, prob = TRUE, order = TRUE)
  Coeff3 <- data.frame(rownames(COEF3),COEF3)
  colnames(Coeff3) <- c('Pregunta','Dificultad','Discriminación','P(x=1|z=0)')
  
  #____4_____________________________________________________
  
  
  
  Base4 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_4$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_4$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_4$C.I)
  
  Base4$Correcta[Base4$C.I=='C'] <- 1
  Base4$Correcta[Base4$C.I=='I'] <- 0
  Base4 <- Base4[,-3]
  CaractIndividuo4 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_4$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_4$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_4$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_4$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_4$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_4$sostenimiento)
  
  
  
  
  B4 <- pivot_wider(Base4, names_from ='Pregunta', values_from = 'Correcta')
  C4 <- merge(x=B4, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C4$distrito[is.na(C4$distrito)] <- 'No registrado'
  
  
  
  Forma_F4 <- C4
  
  
  Forma_F4[,which(colMeans(is.na(Forma_F4))>0)] <- NULL
  
  
  
  fit4 <- rasch(Forma_F4[,-c(1,ncol(Forma_F4),ncol(Forma_F4)-1,ncol(Forma_F4)-2,ncol(Forma_F4)-3,ncol(Forma_F4)-4)])
  
  
  COEF4 <- coef(fit4, prob = TRUE, order = TRUE)
  Coeff4 <- data.frame(rownames(COEF4),COEF4)
  colnames(Coeff4) <- c('Pregunta','Dificultad','Discriminación','P(x=1|z=0)')
  #_____5____________________________________________________
  
  
  Base5 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_5$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_5$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_5$C.I)
  
  Base5$Correcta[Base5$C.I=='C'] <- 1
  Base5$Correcta[Base5$C.I=='I'] <- 0
  Base5 <- Base5[,-3]
  CaractIndividuo5 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_5$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_5$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_5$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_5$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_5$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_5$sostenimiento)
  
  
  
  
  B5 <- pivot_wider(Base5, names_from ='Pregunta', values_from = 'Correcta')
  C5 <- merge(x=B5, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C5$distrito[is.na(C5$distrito)] <- 'No registrado'
  
  
  
  Forma_F5 <- C5
  
  
  Forma_F5[,which(colMeans(is.na(Forma_F5))>0)] <- NULL
  
  
  
  fit5 <- rasch(Forma_F5[,-c(1,ncol(Forma_F5),ncol(Forma_F5)-1,ncol(Forma_F5)-2,ncol(Forma_F5)-3,ncol(Forma_F5)-4)])
  
  
  COEF5 <- coef(fit5, prob = TRUE, order = TRUE)
  Coeff5 <- data.frame(rownames(COEF5),COEF5)
  colnames(Coeff5) <- c('Pregunta','Dificultad','Discriminación','P(x=1|z=0)')
  #_____6____________________________________________________
  
  
  Base6 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_6$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_6$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_6$C.I)
  
  Base6$Correcta[Base6$C.I=='C'] <- 1
  Base6$Correcta[Base6$C.I=='I'] <- 0
  Base6 <- Base6[,-3]
  CaractIndividuo6 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_6$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_6$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_6$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_6$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_6$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_6$sostenimiento)
  
  
  
  
  B6 <- pivot_wider(Base6, names_from ='Pregunta', values_from = 'Correcta')
  C6 <- merge(x=B6, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C6$distrito[is.na(C6$distrito)] <- 'No registrado'
  
  
  
  Forma_F6 <- C6
  
  
  Forma_F6[,which(colMeans(is.na(Forma_F6))>0)] <- NULL
  
  
  
  fit6 <- rasch(Forma_F6[,-c(1,ncol(Forma_F6),ncol(Forma_F6)-1,ncol(Forma_F6)-2,ncol(Forma_F6)-3,ncol(Forma_F6)-4)])
  
  
  COEF6 <- coef(fit6, prob = TRUE, order = TRUE)
  Coeff6 <- data.frame(rownames(COEF6),COEF6)
  colnames(Coeff6) <- c('Pregunta','Dificultad','Discriminación','P(x=1|z=0)')
  #_____7____________________________________________________
  
  
  
  
  Base7 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_7$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_7$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_7$C.I)
  
  Base7$Correcta[Base7$C.I=='C'] <- 1
  Base7$Correcta[Base7$C.I=='I'] <- 0
  Base7 <- Base7[,-3]
  CaractIndividuo7 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_7$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_7$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_7$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_7$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_7$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_7$sostenimiento)
  
  
  
  
  B7 <- pivot_wider(Base7, names_from ='Pregunta', values_from = 'Correcta')
  C7 <- merge(x=B7, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C7$distrito[is.na(C7$distrito)] <- 'No registrado'
  
  
  
  Forma_F7 <- C7
  
  
  Forma_F7[,which(colMeans(is.na(Forma_F7))>0)] <- NULL
  
  
  
  fit7 <- rasch(Forma_F7[,-c(1,ncol(Forma_F7),ncol(Forma_F7)-1,ncol(Forma_F7)-2,ncol(Forma_F7)-3,ncol(Forma_F7)-4)])
  
  
  COEF7 <- coef(fit7, prob = TRUE, order = TRUE)
  Coeff7 <- data.frame(rownames(COEF7),COEF7)
  colnames(Coeff7) <- c('Pregunta','Dificultad','Discriminación','P(x=1|z=0)')
  #_____8____________________________________________________
  
  Base8 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_8$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_8$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_8$C.I)
  
  Base8$Correcta[Base8$C.I=='C'] <- 1
  Base8$Correcta[Base8$C.I=='I'] <- 0
  Base8 <- Base8[,-3]
  CaractIndividuo8 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_8$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_8$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_8$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_8$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_8$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_8$sostenimiento)
  
  
  
  
  B8 <- pivot_wider(Base8, names_from ='Pregunta', values_from = 'Correcta')
  C8 <- merge(x=B8, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C8$distrito[is.na(C8$distrito)] <- 'No registrado'
  
  
  
  Forma_F8 <- C8
  
  
  Forma_F8[,which(colMeans(is.na(Forma_F8))>0)] <- NULL
  
  
  
  fit8 <- rasch(Forma_F8[,-c(1,ncol(Forma_F8),ncol(Forma_F8)-1,ncol(Forma_F8)-2,ncol(Forma_F8)-3,ncol(Forma_F8)-4)])
  
  
  COEF8 <- coef(fit8, prob = TRUE, order = TRUE)
  Coeff8 <- data.frame(rownames(COEF8),COEF8)
  colnames(Coeff8) <- c('Pregunta','Dificultad','Discriminación','P(x=1|z=0)')
  #_____9____________________________________________________
  
  Base9 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_9$Codigo.Estudiante,
                      Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_9$Pregunta,
                      `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_9$C.I)
  
  Base9$Correcta[Base9$C.I=='C'] <- 1
  Base9$Correcta[Base9$C.I=='I'] <- 0
  Base9 <- Base9[,-3]
  CaractIndividuo9 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_9$`Codigo.Estudiante`,
                                 Forma=Detalle_vertical_pilotaje_para_ESPOL2_9$Forma,
                                 zona=Detalle_vertical_pilotaje_para_ESPOL2_9$zona,
                                 amie=Detalle_vertical_pilotaje_para_ESPOL2_9$amie,
                                 distrito=Detalle_vertical_pilotaje_para_ESPOL2_9$distrito,
                                 sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_9$sostenimiento)
  
  
  
  
  B9 <- pivot_wider(Base9, names_from ='Pregunta', values_from = 'Correcta')
  C9 <- merge(x=B9, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C9$distrito[is.na(C9$distrito)] <- 'No registrado'
  
  
  
  Forma_F9 <- C9
  
  
  Forma_F9[,which(colMeans(is.na(Forma_F9))>0)] <- NULL
  
  
  
  fit9 <- rasch(Forma_F9[,-c(1,ncol(Forma_F9),ncol(Forma_F9)-1,ncol(Forma_F9)-2,ncol(Forma_F9)-3,ncol(Forma_F9)-4)])
  
  
  COEF9 <- coef(fit9, prob = TRUE, order = TRUE)
  Coeff9 <- data.frame(rownames(COEF9),COEF9)
  colnames(Coeff9) <- c('Pregunta','Dificultad','Discriminación','P(x=1|z=0)')
  #_______10__________________________________________________
  
  
  
  Base10 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_10$Codigo.Estudiante,
                       Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_10$Pregunta,
                       `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_10$C.I)
  
  Base10$Correcta[Base10$C.I=='C'] <- 1
  Base10$Correcta[Base10$C.I=='I'] <- 0
  Base10 <- Base10[,-3]
  CaractIndividuo10 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_10$`Codigo.Estudiante`,
                                  Forma=Detalle_vertical_pilotaje_para_ESPOL2_10$Forma,
                                  zona=Detalle_vertical_pilotaje_para_ESPOL2_10$zona,
                                  amie=Detalle_vertical_pilotaje_para_ESPOL2_10$amie,
                                  distrito=Detalle_vertical_pilotaje_para_ESPOL2_10$distrito,
                                  sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_10$sostenimiento)
  
  
  
  
  B10 <- pivot_wider(Base10, names_from ='Pregunta', values_from = 'Correcta')
  C10 <- merge(x=B10, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C10$distrito[is.na(C10$distrito)] <- 'No registrado'
  
  
  
  Forma_F10 <- C10
  
  
  Forma_F10[,which(colMeans(is.na(Forma_F10))>0)] <- NULL
  
  
  
  fit10 <- rasch(Forma_F10[,-c(1,ncol(Forma_F10),ncol(Forma_F10)-1,ncol(Forma_F10)-2,ncol(Forma_F10)-3,ncol(Forma_F10)-4)])
  
  COEF10 <- coef(fit10, prob = TRUE, order = TRUE)
  Coeff10 <- data.frame(rownames(COEF10),COEF10)
  colnames(Coeff10) <- c('Pregunta','Dificultad','Discriminación','P(x=1|z=0)')
  #______11___________________________________________________
  
  Base11 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_11$Codigo.Estudiante,
                       Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_11$Pregunta,
                       `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_11$C.I)
  
  Base11$Correcta[Base11$C.I=='C'] <- 1
  Base11$Correcta[Base11$C.I=='I'] <- 0
  Base11 <- Base11[,-3]
  CaractIndividuo11 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_11$`Codigo.Estudiante`,
                                  Forma=Detalle_vertical_pilotaje_para_ESPOL2_11$Forma,
                                  zona=Detalle_vertical_pilotaje_para_ESPOL2_11$zona,
                                  amie=Detalle_vertical_pilotaje_para_ESPOL2_11$amie,
                                  distrito=Detalle_vertical_pilotaje_para_ESPOL2_11$distrito,
                                  sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_11$sostenimiento)
  
  
  
  
  B11 <- pivot_wider(Base11, names_from ='Pregunta', values_from = 'Correcta')
  C11 <- merge(x=B11, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C11$distrito[is.na(C11$distrito)] <- 'No registrado'
  
  
  
  Forma_F11 <- C11
  
  
  Forma_F11[,which(colMeans(is.na(Forma_F11))>0)] <- NULL
  
  
  
  fit11 <- rasch(Forma_F11[,-c(1,ncol(Forma_F11),ncol(Forma_F11)-1,ncol(Forma_F11)-2,ncol(Forma_F11)-3,ncol(Forma_F11)-4)])
  
  
  COEF11 <- coef(fit11, prob = TRUE, order = TRUE)
  Coeff11 <- data.frame(rownames(COEF11),COEF11)
  colnames(Coeff11) <- c('Pregunta','Dificultad','Discriminación','P(x=1|z=0)')
  #________12_________________________________________________
  
  Base12 <- data.frame(Codigo.Estudiante=Detalle_vertical_pilotaje_para_ESPOL2_12$Codigo.Estudiante,
                       Pregunta=Detalle_vertical_pilotaje_para_ESPOL2_12$Pregunta,
                       `C/I`=Detalle_vertical_pilotaje_para_ESPOL2_12$C.I)
  
  Base12$Correcta[Base12$C.I=='C'] <- 1
  Base12$Correcta[Base12$C.I=='I'] <- 0
  Base12 <- Base12[,-3]
  CaractIndividuo12 <- data.frame('Codigo Estudiante'=Detalle_vertical_pilotaje_para_ESPOL2_12$`Codigo.Estudiante`,
                                  Forma=Detalle_vertical_pilotaje_para_ESPOL2_12$Forma,
                                  zona=Detalle_vertical_pilotaje_para_ESPOL2_12$zona,
                                  amie=Detalle_vertical_pilotaje_para_ESPOL2_12$amie,
                                  distrito=Detalle_vertical_pilotaje_para_ESPOL2_12$distrito,
                                  sostenimiento=Detalle_vertical_pilotaje_para_ESPOL2_12$sostenimiento)
  
  
  
  
  B12 <- pivot_wider(Base12, names_from ='Pregunta', values_from = 'Correcta')
  C12 <- merge(x=B12, y=unique(CaractIndividuo1), all.x = TRUE,by = 'Codigo.Estudiante')
  
  C12$distrito[is.na(C12$distrito)] <- 'No registrado'
  
  
  
  Forma_F12 <- C12
  
  
  Forma_F12[,which(colMeans(is.na(Forma_F12))>0)] <- NULL
  
  
  
  fit12 <- rasch(Forma_F12[,-c(1,ncol(Forma_F12),ncol(Forma_F12)-1,ncol(Forma_F12)-2,ncol(Forma_F12)-3,ncol(Forma_F12)-4)])
  
  
  COEF12 <- coef(fit12, prob = TRUE, order = TRUE)
  Coeff12 <- data.frame(rownames(COEF12),COEF12)
  colnames(Coeff12) <- c('Pregunta','Dificultad','Discriminación','P(x=1|z=0)')
  
  #_________________________________________________________
  
  
  Bas <- rbind(Coeff1,Coeff2,Coeff3,
               Coeff4,
               Coeff5,
               Coeff6,
               Coeff7,
               Coeff8,
               Coeff9,
               Coeff10,
               Coeff11,
               Coeff12)
  Names <- c(
    rep('F001',nrow(Coeff1)),
    rep('F002',nrow(Coeff2)),
    rep('F003',nrow(Coeff3)),
    rep('F004',nrow(Coeff4)),
    rep('F005',nrow(Coeff5)),
    rep('F006',nrow(Coeff6)),
    rep('F007',nrow(Coeff7)),
    rep('F008',nrow(Coeff8)),
    rep('F009',nrow(Coeff9)),
    rep('F010',nrow(Coeff10)),
    rep('F011',nrow(Coeff11)),
    rep('F012',nrow(Coeff12)))
  
  BaseUnida <- data.frame(Bas,Names)
  
  library(tidyr)
  highchart()%>%
    hc_add_series(BaseUnida, type='line', hcaes(x=Pregunta, y=Dificultad), name='Dificultad')%>%  
    hc_add_series(BaseUnida, type='line', hcaes(x=Pregunta, y=Discriminación), name='Discriminación')%>%  
    
    
    hc_legend(
      align = "right",
      verticalAlign = "top",
      shadow= FALSE,
      layout = "vertical",
      x = 0,
      y = 100)%>%
    hc_title(text='Curva de Información del Ítem')%>%
    hc_subtitle(text= paste0('Forma: Todas las formas. Campo de conocimiento: ', materia,'.'))%>%
    hc_xAxis(title=list(text="Pregunta"),
             categories=BaseUnida$Pregunta)%>%
  #%>%
  #  hc_yAxis(title=list(text="Información"),
  #           plotLines = list(list(
  #             value = 0.5,
  #             color = '#1D4B5E',
  #             dashStyle= 'shortdash',
  #             width = 3,
  #             zIndex = 4,
  #             label = list(text = "Selección al azar",
  #                          style = list( color = '#1D4B5E', fontWeight = 'bold' )))))%>%
    hc_tooltip(pointFormat = "{point.name} <br>  {point.y} <br>   Forma: {point.Names}",
               headerFormat= '{point.Pregunta}')%>%
    hc_exporting(enabled = TRUE,
                 filename = paste0('Curva de Información del Ítem - Forma: Todas las formas. Campo de conocimiento: ', materia,'.'))%>% 
    hc_credits(
      enabled = TRUE,
      text = "Fuente: Senescyt",
      href = "https://www.educacionsuperior.gob.ec/"
    )
}


