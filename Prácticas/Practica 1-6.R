# Práctica 1.6
# Considera la función "simula" de la práctica 1.5
simula<- function(beta= c(10, -2, 3), n= 300){
  x<- rnorm(n)
  e<- rnorm(n)
  x1<- sin(x)
  x2<- cos(x)
  unos<- rep(1, n)
  X<- cbind(unos, x1, x2)
  y<- X %*% beta + e
  betah<- solve(t(X) %*% X) %*% t(X) %*% y
  yh<- X %*% betah
  eh<- y-yh
  return(list(y= y, X= X, yh= yh, betah= betah))  
}
# Haz una simulación con la función "simula"

# Estima la regresión utilizando la función "lm"

# Muestra los resultados

# Calcula intervalos de confianza para los parámetros

# Estima las predicciones para valores de x1= c(1,1,1,1,1)
# y x2= c(-1,-0.5,0,0.5,1)

