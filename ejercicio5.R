#  5. Selecciona una muestra con probabilidad proporcional al tamaño (p.p.t.) con
#reemplazo siendo la medida de tamaño la variable POBTOT (suponga que la
#conocemos para cada localidad de Oaxaca) con n=600. ¿Qué dificultades tuvo?

x <- Oaxaca$POBTOT [1:8276] # medida de tama?o
probext <- x/sum(x)*(nalt/n)	# probabilidades de extraccion, se multipico
                              # por nalt/n porque es la proporcion de la poblacion
                              #total que se muestreo de esta forma, lo realizamos
                              #de esta forma porque los datos mayores a 8276 en la 
                              # poblacion eran demasiado grandes y  hacian que estos datos 
                              #aparecieran muchas veces en la muestra
sum(probext)
nalt<-596
probinc1 <- n*probext	# probabilidades de inclusion hasta el 8276
sum(probinc1)
sizesok(x,nalt)    # vale 0, no habr? repeticiones
indices5 <- ppswr(x,nalt)
tf<-table(indices5) # tabla de frecuencias (ver repetidos)
table(tf)
max(table(indices5)) # frecuencia mayor
muestra5.1 <- Oaxaca[indices5,]  #esta es la muestra
#probabilidad de inclusion de muestras 5.2,.3,.4,.5
prob2<-1/LM
i<-1
probinc2<-c()
while (i <= 4 ){
  probinc2<-c(probinc2,rep(prob2[i], LM[i]))
  i<-i+1
}
sum(probinc2)
#vector de probabilidad de inclusion completo
probinc<-c(probinc1,probinc2)
sum(probinc)
Oaxaca<-cbind(Oaxaca,probinc)

pob5.2<-Oaxaca[8338:8399,]
pob5.3<- Oaxaca[8337:8295,]
pob5.4<-Oaxaca[8294:8282,]
pob5.5<-Oaxaca[8281:8277,]
LM<-c(62,43,13,5) #longitud de las muestras
muestra5.2 <- pob5.2[sample(1:LM[1],1),]
muestra5.3<- pob5.3[sample(1:LM[2],1),]
muestra5.4 <- pob5.4[sample(1:LM[3],1),]
muestra5.5 <- pob5.5[sample(1:LM[4],1),]


#muestra 5 al fin
muestra5<-rbind(muestra5.1,muestra5.2,muestra5.3,
                muestra5.4,muestra5.5)

#diseño de muestra
dd <- svydesign(id=~1,probs=~probinc,data=muestra5)
summary(dd)

#DEFF  en el diseño de muestra nos da el efecto del diseño V(ppt)/V(maswr)

# #Estima el promedio de la variable índice de relación H-M (ojo hay 3 NA’s en la
# base de datos).
prom_HM5<-svymean (~REL_H_M, design=dd)
prom_HM5
confint(prom_HM5)

# Estima el promedio de Grados promedio de escolaridad (GRAPROES).
#muestra1
prom_GR_5 <- svymean(~GRAPROES,design=dd)
prom_GR_5
confint(prom_GR_5)

# Estima el total de Población afrodescendiente.
prom_AF_5 <- svymean(~POB_AFRO,design=dd)
prom_AF_5
AFRO_TOT5<-prom_AF_5[1]*N
AFRO_TOT5
int5<-confint(prom_AF_5)
int5
int5_1<-int5[1]*N
int5_2<-int5[2]*N

# Estima la proporción de población afrodescendiente.
prop_AFRO_5<- svyratio(~POB_AFRO,~POBTOT,design=dd)
prop_AFRO_5
confint(prop_AFRO_5)


# Estima la proporción de Población de 3 años y más que habla lengua indígena
# con respecto a la Población de 3 años y más del estado.
P_3YM5<- muestra5$POBTOT - muestra5$P_0A2
muestra5a<-cbind(muestra5,P_3YM5)
d5<-svydesign(id=~1,probs=~probinc,data=muestra5a)
prop_P_3YM5<- svyratio(~P3YM_HLI,~P_3YM5,design=d5)
prop_P_3YM5
confint(prop_P_3YM5)



