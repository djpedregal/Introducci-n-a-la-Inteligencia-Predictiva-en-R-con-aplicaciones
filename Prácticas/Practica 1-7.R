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

# Representa gráficamente salario frente a edad

# Ajusta un polinomio de orden 4 con la función "lm" 
# (linear model). Utiliza la función "poly" para 
# generar los polinomios.

# Representa gráficamente la nube de puntos y los valores
# ajustados

# Añade al gráfico el ajuste de un polinomio de orden 20




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

# Calcula las variables con asterisco

# Representa gráficamente las variables con
# asterisco

# Estima la regresión

# Representa la nube de puntos y la recta de regresión

# Representa los valores reales y ajustados

# Representa la nube de puntos y la función no lineal
# estimada

