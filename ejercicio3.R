#3. Selecciona una m.a.s. de conglomerados unietápica, siendo los conglomerados
#los municipios (570).

Nmun<-length (table(Oaxaca$MUN))
nmun<- 20
poblmun<- Oaxaca[order(Oaxaca$MUN),]
library(sampling)
d <- cluster(poblmun,clustername=c("MUN"),size=nmun,method="srswor",description=T)
table(d$MUN)
muestra3<- pobl[d$ID_unit,]
sum(table(muestra3$MUN))

W3<- 1/d$Prob 
fpc3 <- rep(Nmun,sum(table(muestra3$MUN)))
#diseño de muestra
dis_clus <- svydesign(id=~MUN,weights=W3,fpc=fpc3, data=muestra3)  

# #Estima el promedio de la variable índice de relación H-M (ojo hay 3 NA’s en la
# base de datos).
prom_HM3<-svymean (~REL_H_M, design=dis_clus)
prom_HM3
confint(prom_HM3)

# Estima el promedio de Grados promedio de escolaridad (GRAPROES).
#muestra1
prom_GR_3 <- svymean(~GRAPROES,design=dis_clus)
prom_GR_3
confint(prom_GR_3)

# Estima el total de Población afrodescendiente.
prom_AF_3 <- svymean(~POB_AFRO,design=dis_clus)
prom_AF_3
AFRO_TOT3<-prom_AF_3[1]*N
AFRO_TOT3
int3<-confint(prom_AF_3)
int3
int3_1<-int3[1]*N
int3_2<-int3[2]*N

# Estima la proporción de población afrodescendiente.
prop_AFRO_3<- svyratio(~POB_AFRO,~POBTOT,design=dis_clus)
prop_AFRO_3
confint(prop_AFRO_3)


# Estima la proporción de Población de 3 años y más que habla lengua indígena
# con respecto a la Población de 3 años y más del estado.
P_3YM3<- muestra3$POBTOT - muestra3$P_0A2
muestra3.1<-cbind(muestra3,P_3YM3)
dis_clus3<-svydesign(id=~MUN,weights=W3,fpc=fpc3, data=muestra3.1)   
prop_P_3YM3<- svyratio(~P3YM_HLI,~P_3YM3,design=dis_clus3)
prop_P_3YM3
confint(prop_P_3YM3)


    
  