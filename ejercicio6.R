#  6. Selecciona una muestra p.p.t. con medida de tamaño POBTOT estratificando
#las localidades en grupos que tengan tamaño parecido.
dataaa <- Oaxaca[order(Oaxaca$TAMLOC),]	
xx<- dataaa$TAMLOC  # medida de tama?o
probxx<-xx/sum(xx)
sum(probxx)
probin6<-n*probxx
sum(probin6)
dataprobin<-cbind(dataaa,probin6)
sizesok(xx,n)    # vale 0, no habr? repeticiones
indices6 <- ppswr(xx,n)
table(indices6) # tabla de frecuencias (ver repetidos)
max(table(indices6)) # frecuencia mayor
muestra6 <- dataprobin[indices6,]  #esta es la muestra


#diseño de muestra
dppt6 <- svydesign(id=~1,probs=~probin6,data=muestra6) 

# #Estima el promedio de la variable índice de relación H-M (ojo hay 3 NA’s en la
# base de datos).
prom_HM6<-svymean (~REL_H_M, design=dppt6)
prom_HM6
confint(prom_HM6)

# Estima el promedio de Grados promedio de escolaridad (GRAPROES).
#muestra1
prom_GR_6 <- svymean(~GRAPROES,design=dppt6)
prom_GR_6
confint(prom_GR_6)

# Estima el total de Población afrodescendiente.
prom_AF_6 <- svymean(~POB_AFRO,design=dppt6)
prom_AF_6
AFRO_TOT6<-prom_AF_6[1]*N
AFRO_TOT6
int6<-confint(prom_AF_6)
int6
int6_1<-int6[1]*N
int6_2<-int6[2]*N

# Estima la proporción de población afrodescendiente.
prop_AFRO_6<- svyratio(~POB_AFRO,~POBTOT,design=dppt6)
prop_AFRO_6
confint(prop_AFRO_6)


# Estima la proporción de Población de 3 años y más que habla lengua indígena
# con respecto a la Población de 3 años y más del estado.
P_3YM6<- muestra6$POBTOT - muestra6$P_0A2
muestra6.1<-cbind(muestra6,P_3YM6)
dppt6.1 <- svydesign(id=~1,probs=~probin6,data=muestra6.1)   
prop_P_3YM6<- svyratio(~P3YM_HLI,~P_3YM6,design=dppt6.1)
prop_P_3YM6
confint(prop_P_3YM3)
