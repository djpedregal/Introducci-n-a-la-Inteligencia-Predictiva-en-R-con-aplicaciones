# Práctica 1.5
# Considera la siguiente simulación
x<- rnorm(300)
e<- rnorm(300)
x1<- sin(x)
x2<- cos(x)
beta<- c(10, -2, 3)
unos<- rep(1, 300)
X<- cbind(unos, x1, x2)
y<- X %*% beta + e

# Estima la regresión por mínimos cuadrados, llama a los
# parámetros "betah" y compáralos con los valores simulados
#             -1
# betah= (X'X)   X'y
betah<- solve(t(X) %*% X) %*% t(X) %*% y
cbind(betah, beta)

# Calcula los valores ajustados de la varible endógena
# y llámala "yh". Haz un gráfico con "y" e "yh".
# yh= X betah
yh<- X %*% betah
plot(y, type= "l")
lines(yh, type= "l", col= "red")

# Calcula los errores estimados y llámalos "eh".
# eh= y-yh
eh<- y-yh
var(eh)

# Muestra "eh" junto con "e" en un gráfico.
plot(e, type="l")
lines(eh, type= "l", col= "red")

# Escribe una función que simule regresiones como las
# anteriores ("simula"). Los argumentos de entrada 
# serán el vector "beta" simulado y el tamaño de la muestra.
# Los argumentos de salida serán los valores de la endógena,
# las exógenas en una matriz ("X"), los valores ajustados 
# "yh" y los parámetros estimados "betah".
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

# Prueba la función "simula" con algún ejemplo
a<- simula(c(5,-2,2))
