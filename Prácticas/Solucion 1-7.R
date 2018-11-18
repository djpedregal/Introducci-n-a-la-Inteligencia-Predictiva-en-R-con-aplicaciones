# Práctica 1.7.
#####################
# PRIMER CASO:
# Relación polinomial entre edad y salarios
# 
# Basado en G. James, D. Witten, T. Hastie and R. Tibshirani (2014). 
# “An Introduction to Statistical Learning with Applications in R"
# (http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Seventh%20Printing.pdf) 
#####################
rm(list= ls())
library(ISLR)
attach(Wage)

# Mira la ayuda que hay sobre la base de datos Wage
?Wage
names(Wage)

# Representa gráficamente salario frente a edad
plot(age, wage)

# Ajusta un polinomio de orden 4 con la función "lm" 
# (linear model). Utiliza la función "poly" para 
# generar los polinomios.
modelo=lm(wage~poly(age,4),data=Wage)
summary(modelo)

# Representa gráficamente la nube de puntos y los valores
# ajustados
plot(age, wage)
ind<- order(age)
lines(age[ind], modelo$fitted.values[ind], col= "red", lwd= 5)

# Añade al gráfico el ajuste de un polinomio de orden 20
modelo10=lm(wage~poly(age,20),data=Wage)
summary(modelo10)
lines(age[ind], modelo10$fitted.values[ind], col= "blue", lwd= 5)



#####################
# SEGUNDO CASO (AVANZADO):
# Función de producción americana tipo Cobb-Douglas
# Con rendimientos constantes a escala
# 
# Basado en C.W. Cobb and P.H. Douglas (1928). 
# “A Theory of Production”,  American Economic 
# Review Vol. 18 (Supplement) pp. 139-165.
#####################
# 
#       b  (1-b)
# Q= a K  L     ,     con 0 < b < 1
#
# donde Q es la producción, K es la cantidad del factor
# capital utilizada y L es la cantidad de factor trabajo
# Esta función se puede escribir como
#
#       b
# q= a k  ,  siendo q=K/L y k= K/L
#
# Para considerar el ajuste hay que añadir un término de
# error, es decir, la función estimable será
#
#       b
# q= a k  e,  siendo "e" el término de error con media 1
#
# Se trata de una función no lineal, ¿cómo se puede
# estimar por mínimos cuadrados?
#
# Se puede estimar de forma lineal si tomamos logaritmos
# naturales, es decir, 
# 
# log(q)= log(a) + b log(k) + log(e)
# 
# o bien,
# 
# q* = a* + b k* + e*
rm(list= ls())
load("Produccion.dat")
attach(datos)
summary(datos)

# Represéntalas gráficamente en un mismo plot con
# distintos colores
plot(Year, K, type="l")
lines(Year, Q, col= "red")
lines(Year, L, col= "blue")

# Calcula las variables con asterisco
q_= log(Q/L)
k_= log(K/L)

# Representa gráficamente las variables con
# asterisco
plot(Year, k_, type= "l")
lines(Year, q_, col= "red")

# Estima la regresión
modelo<- lm(q_~k_)
summary(modelo)

# Representa la nube de puntos y la recta de regresión
plot(k_, q_)
abline(modelo, col= "red")

# Representa los valores reales y ajustados
plot(q_)
lines(modelo$fitted.values, col= "red")

# Representa la nube de puntos y la función no lineal
# estimada
KL= K/L
QL= Q/L
plot(KL, QL)
valores<- seq(min(KL), max(KL), 0.02)
ajuste<- exp(modelo$coefficients[1])*valores^modelo$coefficients[2]
lines(valores, ajuste, col="red", lwd= 3)



