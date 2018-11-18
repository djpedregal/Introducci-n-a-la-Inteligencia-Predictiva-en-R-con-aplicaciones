# Práctica 2.2
# Predice los tres últimos años del ipi para los que hay
# datos con la función "simple" y representa gráficamente
# las predicciones Ten en cuenta que un segmento de una
# serie temporal NO es una serie temporal, sino una matriz.
rm(list = ls())
library(forecast)
simple<- function(y, n= 12){
  ym<- meanf(y, n)
  yn<- naive(y, n)
  ysn<- snaive(y, n)
  return(cbind(ym$mean, yn$mean, ysn$mean))
}
ipi<- ts(scan("ipi.dat"), start = 1975, frequency = 12)
tsipi<- ts(ipi[1:(length(ipi)-36)], start = 1975, frequency = 12)
pipi<- simple(tsipi, 36)
ts.plot(ipi, pipi, col= c("black","green","blue","red"))

# Calcula las medidas de predicción para las tres predicciones
# usando la función "accuracy"
accuracy(pipi[,1], window(ipi, 2016-2))
accuracy(pipi[,2], window(ipi, 2016-2))
accuracy(pipi[,3], window(ipi, 2016-2))

# Un método de predicción es bueno si predice SISTEMÁTICAMENTE
# mejor que los demás. Para ello es importante hacer un contraste
# exahustivo mediante un experimento en el que se cambie
# el origen de predicción incrementándolo en un mes en cada iteración.
# Calcula los MAPE de 1 a 12 meses hacia adelante de la variable
# ipi comenzando en enero de 1996 hasta el final de la muestra.
# Necesitarás dos bucles anidados, uno para el origen de
# predicción y otro para el horizonte de predicción.
# Tienes que hacer esto para los tres modelos.
MAPEmean<- matrix(NA, 249, 12)
MAPEnaive<- MAPEmean
MAPEsnaive<- MAPEmean
for (i in 252:500){
  tsipi<- ts(ipi[1:i], start = 1975, frequency = 12)
  pipi<- simple(tsipi, 12)
  tsipif<- ts(ipi[1:(i+12)], start = 1975, frequency = 12)
  ts.plot(tsipif, pipi, col= c("black","green","blue","red"))
  Sys.sleep(0.1)
  for (h in (1:12)){
    pmean<- accuracy(pipi[1:h,1], ipi[(i+1):(i+h)])
    pnaive<- accuracy(pipi[1:h,2], ipi[(i+1):(i+h)])
    psnaive<- accuracy(pipi[1:h,3], ipi[(i+1):(i+h)])
    MAPEmean[i-251, h]<- pmean[5]
    MAPEnaive[i-251, h]<- pnaive[5]
    MAPEsnaive[i-251, h]<- psnaive[5]
  }
}

# Representa gráficamente los errores de predicción
# un periodo hacia adelante
plot(MAPEmean[,1], type="l")
lines(MAPEnaive[,1], col= "red")
lines(MAPEsnaive[,1], col= "blue")

# Calcula la media de todos los errores
plot(colMeans(MAPEmean), type ="l", ylim= c(0, 17))
lines(colMeans(MAPEnaive), col= "red")
lines(colMeans(MAPEsnaive), col = "blue")
