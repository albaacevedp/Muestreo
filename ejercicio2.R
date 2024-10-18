#2. Selecciona una muestra sistemática de n= 600, con el marco de localidades
#ordenado por TAMLOC.
n<-600
k<-(N/n)
r<-sample (1:floor(k),1)
secuencia <- seq(r,N,k)
length(secuencia)
secuenciaf<-ceiling(secuencia)
muestra2<- pob[secuenciaf,]
table(muestra2$TAMLOC)

W2<- rep(n/N,n)
#diseño de muestra
dis_sis <- svydesign(~1, weights=~W2, data=muestra2)


# #Estima el promedio de la variable índice de relación H-M (ojo hay 3 NA’s en la
# base de datos).
prom_HM2<-svymean (~REL_H_M, design=dis_sis)
prom_HM2
confint(prom_HM2)


# Estima el promedio de Grados promedio de escolaridad (GRAPROES).
#muestra1
prom_GR_2 <- svymean(~GRAPROES,design=dis_sis)
prom_GR_2
confint(prom_GR_2)

# Estima el total de Población afrodescendiente.
prom_AF_2 <- svymean(~POB_AFRO,design=dis_sis)
prom_AF_2
AFRO_TOT2<-prom_AF_2[1]*N
AFRO_TOT2
int2<-confint(prom_AF_2)
int2
int2_1<-int2[1]*N
int2_2<-int2[2]*N

# Estima la proporción de población afrodescendiente.
prop_AFRO_2<- svyratio(~POB_AFRO,~POBTOT,design=dis_sis)
prop_AFRO_2
confint(prop_AFRO_2)

# Estima la proporción de Población de 3 años y más que habla lengua indígena
# con respecto a la Población de 3 años y más del estado.
P_3YM2<- muestra2$POBTOT - muestra2$P_0A2
muestra2.1<-cbind(muestra2,P_3YM2)
dis_sis2 <- svydesign(~1, weights=~W2, data=muestra2.1)
prop_P_3YM2<- svyratio(~P3YM_HLI,~P_3YM2,design=dis_sis2)
prop_P_3YM2
confint(prop_P_3YM2)


