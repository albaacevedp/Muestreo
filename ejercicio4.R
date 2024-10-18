#4. Selecciona una muestra bietápica m.a.s.-m.a.s. siendo las UPM los municipios y
#las USM las localidades. Selecciona n=300 UPM y mi=2 USM. ¿Qué dificultades
#tuvo?

#ordenar la base de datos Oaxaca 
mun_orden<- Oaxaca[order(Oaxaca$MUN),]
#Numero de municipios 
Nmun<-length(table(Oaxaca$MUN))  #570 municipios

#tabla frecuencias de la tabla de frecuencias de localidades por municipio
table(table(Oaxaca$MUN))
#Hay 57 municipios con una sola localidad por lo que separamos 
#la base de datos en dos partes los municipios con una sola localidad 
#y los munucipios con mas de una localidad para poder muestrear de forma 
#proporcional los municipios de una sola localidad y lo de mas

#separando los municipios de una y mas localidades

#vector de localidades por municipio
q<-table(mun_orden$MUN)
q
#vector de municipios de una sola localidad
i<-1
j<-0
mun1=c()
while (i <= length(q)) {
  if (q[i] == 1) {
    j<-j+1
    i_2<-i
    mun1<-c(mun1,i_2)
     }
  i<-i+1
}

#vector de municipios de mas de una localidad
i<-1
j<-0
munmas=c()
while (i <= length(q)) {
  if (q[i]>1) {
    j<-j+1
    i_3<-i
    munmas<-c(munmas,i_3)
  }
  i<-i+1
}

#data frame de los municipios de una localidad
i<-1
j<-0
dfmun1=c()
while (i <= length(mun1)) {
  a <-Oaxaca[Oaxaca$MUN == mun1[i], ]  
  dfmun1<-rbind(dfmun1,a)
  i<-i+1
}

#data frame de los municipios de mas de una localidad
i<-1
j<-0
dfmunmas=c()
while (i <= length(munmas)) {
  a <-Oaxaca[Oaxaca$MUN ==munmas[i], ]  
  dfmunmas<-rbind(dfmunmas,a)
  i<-i+1
}

#para este muestreo bietapico p una UPM de municipios de 300
#57 son municipios de una localidad son  0.1 de los 570 municipios totales
#nos pinden 300 municipios en muestra por lo que municipios de una localidad
#tomaremos 0.1 que son 30, hacer un mas sobre dfmun1

mm1 <- dfmun1[sample((1:57),30), ]

#incluyendo las probabilidades
Prob1<-rep(30/57*0.1,30)
mm1<- cbind(mm1,Prob1)

#513 son municipios de mas de una localidad, haremos un muestreo bietapico 
#son el 0.9 de los 570 municipios por lo que para muestra el 0.9 de 300 es 
#270 
Nb<-length(table(dfmunmas$MUN))
nb<-270 #UPM
mi<-2    #USM
indmm2<- mstage(dfmunmas,stage=list("",""),
             varnames=list("MUN","LOC"),
             size=list(nb,c(rep(mi,nb))),method=list("srswor","srswor"))

mimuestra <- getdata(dfmunmas,indmm2)
mm2 <- mimuestra[[2]]
Prob1<-mm2$Prob*0.9 #multiplique por 0.9 por que es la proporcion 
                    # de municipios con mas de una localidad
mm2<- mm2[   ,1:57]
mm2<-cbind(mm2,Prob1)


#muestra final
muestra4<- rbind (mm1,mm2)
length(muestra4$MUN)
#diseño de muestra

fpc1_4 <- rep(N,570)
fpc2_4 <- c(rep(1,30),rep(2,540))
d4<- svydesign(id=~MUN+LOC,probs=~Prob1,fpc=~fpc1_4+fpc2_4,data=muestra4)

# #Estima el promedio de la variable índice de relación H-M (ojo hay 3 NA’s en la
# base de datos).
prom_HM4<-svymean (~REL_H_M, design=d4)
prom_HM4
confint(prom_HM4)

# Estima el promedio de Grados promedio de escolaridad (GRAPROES).
#muestra1
prom_GR_4 <- svymean(~GRAPROES,design=d4)
prom_GR_4
confint(prom_GR_4)

# Estima el total de Población afrodescendiente.
prom_AF_4 <- svymean(~POB_AFRO,design=d4)
prom_AF_4
AFRO_TOT4<-prom_AF_4[1]*N
AFRO_TOT4
int4<-confint(prom_AF_4)
int4
int4_1<-int4[1]*N
int4_2<-int4[2]*N

# Estima la proporción de población afrodescendiente.
prop_AFRO_4<- svyratio(~POB_AFRO,~POBTOT,design=d4)
prop_AFRO_4
confint(prop_AFRO_4)


# Estima la proporción de Población de 3 años y más que habla lengua indígena
# con respecto a la Población de 3 años y más del estado.
P_3YM4<- muestra4$POBTOT - muestra4$P_0A2
muestra4.1<-cbind(muestra4,P_3YM4)
dis4<- svydesign(id=~MUN+LOC,probs=~Prob1,fpc=~fpc1_4+fpc2_4,data=muestra4.1)
prop_P_3YM4<- svyratio(~P3YM_HLI,~P_3YM4,design=dis4)
prop_P_3YM4
confint(prop_P_3YM4)

