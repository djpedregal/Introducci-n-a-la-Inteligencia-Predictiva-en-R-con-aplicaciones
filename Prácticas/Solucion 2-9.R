# Práctica 2.9
# Análisis manual del pib español
# Cargamos y vemos los datos
rm(list = ls())
library(forecast)
s= 4        # Periodo estacional
npred= 4*s  # Número de predicciones
orig= 12*s  # Origen de predicción
pib<- ts(scan("pib.dat"), start = 1995, frequency = 4)
plot(pib)

# Utilizamos los 12 años iniciales para hacer el modelo
# Genera la serie temporal "pib" que sea la anterior
# hasta el origen de predicción
pib<- ts(pib[1:orig], start = 1995, frequency = 4)

# Análisis exploratorio con "tsdisplay", "seasonplot",
# "monthplot"
tsdisplay(pib)
seasonplot(pib)
monthplot(pib)

# Transformación varianza
lambda<- BoxCox.lambda(pib)
lambda
tpib<- BoxCox(pib, lambda)
tsdisplay(tpib)

# Análisis de diferencias e identificación
tsdisplay(diff(tpib))
tsdisplay(diff(tpib, s))
tsdisplay(diff(diff(tpib, s)))

# Estimación con función "Arima". Usa "summary" también.
modelo1<- Arima(tpib, order= c(0,1,1), seasonal= c(0,1,1))
summary(modelo1)

# Diagnóstico: autocorrelación de residuos ("Box.test"),
# Gaussianidad ("jarque.bera.test", "shapiro.test"), 
# heterocedasticidad ("var.test")
e1= modelo1$residuals
e1= e1[-(s+2):-1]
tsdisplay(e1)
Box.test(e1, 2*s, "Ljung-Box", length(modelo1$coef))
jarque.bera.test(e1)
shapiro.test(e1)
var.test(e1[1:(length(e1)/3)], e1[(length(e1)/3*2):length(e1)])

# Análisis automático del PIB español
# ("auto.arima") y diagnóstico
modelo2<- auto.arima(tpib)
e2= modelo2$residuals
e2= e2[-(s+2):-1]
tsdisplay(e2)
Box.test(e2, 2*s, "Ljung-Box", length(modelo2$coef))
jarque.bera.test(e2)
shapiro.test(e2)
var.test(e2[1:(length(e2)/3)], e2[(length(e2)/3*2):length(e2)])

# Carga en memoria la función de predicción simple
simple<- function(y, n= 12){
  ym<- meanf(y, n)
  yn<- naive(y, n)
  ysn<- snaive(y, n)
  return(cbind(ym$mean, yn$mean, ysn$mean))
}

# Predicción un año hacia adelante de los dos modelos
# ARIMA y de las predicciones simples
x<- ts(scan("pib.dat"), start = 1995, frequency = s)
MAPEmean<- matrix(NA, length(x)-npred-orig+1, npred)
MAPEnaive<- MAPEmean
MAPEsnaive<- MAPEmean
MAPE1<- MAPEmean
MAPE2<- MAPEmean
for (i in orig:(length(x)-npred)){
  pib<- ts(x[1:i], start = 1995, frequency = s)
  # Box-Cox
  lambda<- BoxCox.lambda(pib)
  tpib<- BoxCox(pib, lambda)
  # Predicción simple
  pmodelo0<- simple(pib, npred)
  # ARIMA manual
  modelo1<- Arima(tpib, order= c(0,1,1), seasonal= c(0,1,1))
  # ARIMA automático
  modelo2<- auto.arima(tpib, 1, 1)
  # Calculando predicciones de modelos ARIMA
  pmodelo1<- forecast(modelo1, npred)
  pmodelo2<- forecast(modelo2, npred)
  # Deshaciendo transformación Box-Cox
  pmodelo1<- InvBoxCox(pmodelo1$mean, lambda)
  pmodelo2<- InvBoxCox(pmodelo2$mean, lambda)
  # Gráfico para comprobar que no hay desajustes
  ppib<- ts(x[1:(i+npred)], start= 1995, frequency = s)
  ts.plot(ppib, cbind(pmodelo0, pmodelo1, pmodelo2), col= c("black","green","blue","red", "green", "blue"))
  title(paste("Iteración: ", i, " de ", length(x)-npred))
  Sys.sleep(0.1)
  # Calculando precisión para todos los modelos
  for (h in (1:npred)){
    pmean<- accuracy(pmodelo0[1:h,1], x[(i+1):(i+h)])
    pnaive<- accuracy(pmodelo0[1:h,2], x[(i+1):(i+h)])
    psnaive<- accuracy(pmodelo0[1:h,3], x[(i+1):(i+h)])
    p1<- accuracy(pmodelo1[1:h], x[(i+1):(i+h)])
    p2<- accuracy(pmodelo2[1:h], x[(i+1):(i+h)])
    MAPEmean[i-orig+1, h]<- pmean[5]
    MAPEnaive[i-orig+1, h]<- pnaive[5]
    MAPEsnaive[i-orig+1, h]<- psnaive[5]
    MAPE1[i-orig+1, h]<- p1[5]
    MAPE2[i-orig+1, h]<- p2[5]
  }
}

# Representa gráficamente los errores de predicción
# un periodo hacia adelante
step= 1
plot(MAPEsnaive[,step], type="l", col= "black")
lines(MAPE1[,step], col= "red", type= "b")
lines(MAPE2[,step], col= "blue", type = "b")

# Calcula la media de todos los errores
plot(colMeans(MAPEsnaive, na.rm= TRUE), col = "blue", type= "l", ylim= c(0, 11))
lines(colMeans(MAPE1, na.rm= TRUE), col= "red", type= "b")
lines(colMeans(MAPE2, na.rm= TRUE), col= "blue", type = "b")
