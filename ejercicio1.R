
Oaxaca<- read.csv("Datosxloc.csv")
#1. Selecciona una muestra aleatoria estratificada de tamaño n=600, con
#distribución proporcional al número de localidades en el estrato, siendo los
#estratos los valores de la variable TAMLOC.
pob<-Oaxaca[order(Oaxaca$TAMLOC),]
poba<-pob[1:8356,]
N6<- length (poba$ENTIDAD)
n6 <- 590
Nh<- table(poba$TAMLOC)
sum(Nh)
nh <- round(n6*Nh/N6)
sum(nh)
N<-length (pob$ENTIDAD)
library(pps)
set.seed(12345)
indices <- stratsrs(poba$TAMLOC,nh)
muestraa<- poba[indices,]

#muestra de estrato 7,8,9 y 10
pobb<-pob[8357:8373,]
pobc<- pob[8374:8389,]
pobd<-pob [8390:8393,]
pobe <-pob [8394:8397,]
NNc<-c(17,16,4,4) #longitud de los estratos
muestrab <- pobb[sample(1:NNc[1],2),]
muestrac<- pobc[sample(1:NNc[2],2),]
muestrad <- pobd[sample(1:NNc[3],2),]
muestrae <- pobe[sample(1:NNc[4],2),]

#muestra 1 al fin

muestra1<-rbind(muestraa,muestrab,muestrac,
                muestrad,muestrae,pob[8398:8399,])
table(muestra1$TAMLOC)
#Calculado fpc y pesos
nh<-table(muestra1$TAMLOC)
nh
NNh<- table(pob$TAMLOC) 
fpc1<-rep(NNh,nh)
#vector de pesos por estrato
Wh<-1/N*nh_1
i<-1
W1<-c()
while (i <= 12 ){
  W1<-c(W1,rep(Wh[i], nh_1[i]))
  i<-i+1
}

#diseño de muestra
library(survey)
dis_est <- svydesign(~1,fpc=fpc1, strata=~TAMLOC, weights=W1, data=muestra1)
dis_est

# #Estima el promedio de la variable índice de relación H-M (ojo hay 3 NA’s en la
# base de datos).
# muestra1
prom_HM_1 <- svymean(~REL_H_M,design=dis_est)
prom_HM_1
confint(prom_HM_1)

# Estima el promedio de Grados promedio de escolaridad (GRAPROES).
#muestra1
prom_GR_1 <- svymean(~GRAPROES,design=dis_est, is.na=)
prom_GR_1
confint(prom_GR_1)

# Estima el total de Población afrodescendiente.
#muestra 1
Oaxaca$POB_AFRO
total_AF_1 <- svytotal(~POB_AFRO,design=dis_est)
total_AF_1
confint(total_AF_1)

prom_AF_1 <- svymean(~POB_AFRO,design=dis_est)
prom_AF_1
AFRO_TOT<-prom_AF_1[1]*N
AFRO_TOT
int1<-confint(prom_AF_1)
int1
int1_1<-int1[1]*N
int1_2<-int1[2]*N

# Estima la proporción de población afrodescendiente.
#muestra1 
prop_AFRO_1<- svyratio(~POB_AFRO,~POBTOT,design=dis_est)
prop_AFRO_1
confint(prop_AFRO_1)


# Estima la proporción de Población de 3 años y más que habla lengua indígena
# con respecto a la Población de 3 años y más del estado.

P_3YM<- muestra1$POBTOT - muestra1$P_0A2
muestra1.1<-cbind(muestra1,P_3YM)
dis_est1 <- svydesign(~1,fpc=fpc1, strata=~TAMLOC, weights=~W1, data=muestra1.1)
prop_P_3YM<- svyratio(~P3YM_HLI,~P_3YM,design=dis_est1)
prop_P_3YM
confint(prop_P_3YM)

