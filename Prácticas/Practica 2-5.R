# Práctica 2.5
# Análisis manual de pasajeros de avión
# Cargamos y vemos los datos
rm(list = ls())
library(forecast)
s= 12       # Periodo estacional
npred= 4*s  # Número de predicciones
orig= 348   # Origen de predicción

# Utilizamos los 29 años iniciales para hacer el modelo
# Genera la serie temporal "pasajeros" que sea la anterior
# hasta el origen de predicción

# Análisis exploratorio con "tsdisplay", "seasonplot",
# "monthplot"

# Estimación con función "bats".

# Diagnóstico: autocorrelación de residuos ("Box.test"),
# Gaussianidad ("jarque.bera.test", "shapiro.test"), 
# heterocedasticidad ("var.test")

# Carga en memoria la función de predicción simple
simple<- function(y, n= 12){
  ym<- meanf(y, n)
  yn<- naive(y, n)
  ysn<- snaive(y, n)
  return(cbind(ym$mean, yn$mean, ysn$mean))
}

# Predicción un año hacia adelante de los dos modelos
# ETS y de las predicciones simples
x<- ts(scan("Pasajeros.dat"), start = 1969, frequency = s)
MAPEmean<- matrix(NA, length(x)-npred-orig+1, npred)
MAPEnaive<- MAPEmean
MAPEsnaive<- MAPEmean
MAPE1<- MAPEmean
MAPE2<- MAPEmean
for (i in orig:(length(x)-npred)){
  pasajeros<- ts(x[1:i], start = 1969, frequency = s)
  # Predicción simple
  pmodelo0<- simple(pasajeros, npred)
  # EStimando modelo ETS
  modelo1<- bats(pasajeros, seasonal.periods = s)
  # Calculando predicciones de modelos ETS
  pmodelo1<- forecast(modelo1, npred)
  # Gráfico para comprobar que no hay desajustes
  ppasajeros<- ts(x[1:(i+npred)], start= 1969, frequency = s)
  ts.plot(ppasajeros, cbind(pmodelo0, pmodelo1), col= c("black","green","blue","red", "blue"))
  title(paste("Iteración: ", i, " de ", length(x)-npred))
  Sys.sleep(0.1)
  # Calculando precisión para todos los modelos
  for (h in (1:npred)){
    pmean<- accuracy(pmodelo0[1:h,1], x[(i+1):(i+h)])
    pnaive<- accuracy(pmodelo0[1:h,2], x[(i+1):(i+h)])
    psnaive<- accuracy(pmodelo0[1:h,3], x[(i+1):(i+h)])
    p1<- accuracy(pmodelo1$mean[1:h], x[(i+1):(i+h)])
    MAPEmean[i-orig+1, h]<- pmean[5]
    MAPEnaive[i-orig+1, h]<- pnaive[5]
    MAPEsnaive[i-orig+1, h]<- psnaive[5]
    MAPE1[i-orig+1, h]<- p1[5]
  }
}

# Representa gráficamente los errores de predicción
# un periodo hacia adelante

# Calcula la media de todos los errores


