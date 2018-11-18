# Práctica 2.1
# En esta práctica vamos a usar el package "forecast".
# Cárgalo.
library(forecast)

# Carga en memoria el Indice de Producción Industrial
# mensual del archivo "ipi.dat" con la función "scan"
ipi<- scan("ipi.dat")

# Genera un objeto serie temporal a partir de esta
# variable, teniendo en cuenta que los datos empiezan
# en enero de 1975 y se trata de datos mensuales.
# Usa la función "ts".
ipi<- ts(scan("ipi.dat"), start = 1975, frequency = 12)

# Representa gráficamente los datos
plot(ipi)
title("IPI mensual 1975-2017")

# Repite todo lo anterior para el pib trimestral,
# teniendo en cuenta que el archivo es "pib.dat",y los
# datos comienzan en el primer trimestre de 1995
pib<- ts(scan("pib.dat"), start = 1995, frequency = 4)
plot(pib)
title("PIB trimestral 1995-2017")

# Repite todo lo anterior para el índice de Bolsa de Madrid,
# teniendo en cuenta que el archivo es "bolsa.dat",y los
# datos son diarios, comenzando el 1/1/1974
bolsa<- ts(scan("bolsa.dat"), frequency= 1)
plot(bolsa)
title("Bolsa de Madrid 1974-2017")

# Crea una función que realice las siguientes predicciones
# simples:
#   1) Media (función "meanf")
#   2) Naive (función "naive")
#   3) Naive estacional (función "snaive")
# El argumento de salida de la función debe ser una matriz
# con las tres predicciones en columnas y los argumentos 
# de entrada deben ser la serie temporal y el número de 
# predicciones
simple<- function(y, n= 12){
  ym<- meanf(y, n)
  yn<- naive(y, n)
  ysn<- snaive(y, n)
  return(cbind(ym$mean, yn$mean, ysn$mean))
}

# Predice el PIB 5 años hacia adelante con esta función
# y representa los resultados en una sola gráfica
# mediante la función "ts.plot"
ppib<- simple(pib, 20)
ts.plot(pib, ppib, col= c("black","red","blue","green"))

# Predice otras series de la misma forma


