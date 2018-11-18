# Práctica 1.8
#######################
# PRIMER CASO
#######################
# Vamos a utilizar la data.frame "Wage" que se
# encuentra en el package "ISLR". Carga el package.
# También vamos a utilizar el package "leaps" para buscar
# las mejores regresiones
rm(list=ls())
library(ISLR)
library(leaps)
# Mira las variables que hay en la data.frame
names(Wage)
# Dibuja todas las variables mediante un bucle
nombres<- names(Wage)
for (i in 1:dim(Wage)[2]){
  plot(Wage[[i]], ylab = nombres[[i]])
  readline()
}
# Todas las observaciones son de la misma región. Esa
# variable va a producir colinealidad exacta con la constante,
# por lo que habrá que eliminarla de la base de datos
Wage<-subset(Wage, select= -c(region))

# Busca la mejor regresión seleccionando un subconjunto
# de todas las variables. Utiliza solo las 2200 primeras 
# observaciones y reserva las últimas 800 observaciones para 
# hacer predicción
# Define vector "train" de las observaciones que utilizamos
# para entrenar los modelos
train<- seq(2200)
# Utiliza "regsubsets" para ajustar los modelos con los datos
# del training. Realiza un gráfico que muestre el mejor BIC
# frente al número de variables.
modelo1= regsubsets(wage~., data= Wage[train,], nvmax= 17)
resumen1= summary(modelo1)
resumen1
names(resumen1)
plot(resumen1$bic, xlab= "Número de variables", ylab= "BIC")
# plot(modelo1, scale="bic")

# Recupera los coeficientes del mejor modelo (con tres variables)
beta<- coef(modelo1, 3)
# Calcula la suma de los errores al cuadrado del training
X<- model.matrix(wage~., data= Wage[train,])
wagefor<- X[, names(beta)] %*% beta
e_train<- (wagefor-Wage$wage[train])
SCR<- (t(e_train)%*%e_train)/dim(e_train)[1]
SCR
# Calcula SCR para todos los mejores modelos estimados
# con número creciente de regresores. Utiliza el código
# anterior y envuélvelo en un bucle. Llama a SCR "SCR_train"

# Repite lo mismo, pero ahora con predicciones
# fuera de la muestra que has utilizado para estimar.
# Llámalo "SCR_test".

# Representa en un solo gráfico el resultado para el
# training y el test
plot(SCR_test-SCR_test[1]+SCR_train[1], type= "l", col= "red")
lines(SCR_train, col= "blue")
legend(6, SCR_train[1], legend = c("Test", "Train"), col= c("red", "blue"), lty = c(1,1))

# Escribe una función "seleccionar" que estime el mejor 
# modelo y realice las predicciones. PERO AHORA la función
# debe elegir aleatoriamente la muestra que utiliza
# para estimar el modelo. Además los argumentos de
# entrada serán el modelo y la base de datos, y 
# las observaciones que se incluirán en el training
# set como en la llamada a "regsubsets"

# Usar 10-fold cross-validation. Dividimos la muestra
# en 10 partes y usamos 9 de ellas para ajustar
# las regresiones y la décima para calcular SCR.
# Repetimos para las 10 muestras y hacemos la media
# de los resultados.
muestras<- sample(rep(1:10, 300))
SCR_test= matrix(NA, 10, 17)
for (i in 1:10){
  train<- sample(muestras!= i)
  SCR<- seleccionar(wage~., Wage, train)
  SCR_test[i,]= SCR
}
plot(colMeans(SCR_test))


#######################
# SEGUNDO CASO
#######################
# Basado en G. James, D. Witten, T. Hastie and R. Tibshirani (2014). 
# “An Introduction to Statistical Learning with Applications in R"
# (http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Seventh%20Printing.pdf) 
#######################
# Vamos a utilizar la data.frame "Boston" que se
# encuentra en el package "MASS". Carga el package.

# Mira las variables que hay en la data.frame

# Familiarízate con la base de datos
# Mira la ayuda para esta base de datos

# Dibuja todas las variables mediante un bucle

# Examina las mejores regresiones con una muestra
# de training de 400 observaciones

# Busca la mejor regresión seleccionando un subconjunto
# de todas las variables con el package "leaps"

# Adapta la función "seleccionar" para este caso

# Prueba la función elegiendo una muestra de training de 400
# observaciones y una muestra de test de 106.

# Usar 10-fold cross-validation
