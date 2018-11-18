# Práctica 1.9
#######################
# Vamos a utilizar la data.frame "Wage" que se
# encuentra en el package "ISLR". Carga el package.
# También usaremos el package "glmnet" para hacer la
# regresión LASSO y el package "dummies" para construir
# variables tipo artificial
rm(list=ls())
library(ISLR)
library(glmnet)
library(dummies)

# Hacemos un poco de limpieza de datos necesaria
Wage<-subset(Wage, select= -c(region, logwage))
WageD = dummy.data.frame(Wage)

# Define n como el número de columnas de WageD
# Define train como la muestra de entrenamiento 2800 
# observaciones
n = dim(WageD)[2]
train<- seq(2800)

# Define las variables independientes como X (todas 
# menos la última, que es el salario). Tiene que ser
# una matriz plana (usa as.matrix).
# Define la variable y como el salario (última columna 
# de WageD)
X = as.matrix(WageD[, 1:(n - 1)])
y = as.matrix(WageD[, n])

# Ajusta regresión LASSO usando glmnet y muestra los
# resultados gráficamente
fit <- glmnet(x = X[train,], y = y[train,])
plot(fit,xvar="lambda")

# Calcula el valor óptimo de lambda por validación 
# cruzada (cv.glmnet)
fit.cv <- cv.glmnet(x = X[train,], y = y[train], grouped=FALSE)

# Extrae los parámetros del modelo y muéstralos
beta <- coef(fit.cv)
print(beta)
print(paste("Lambda:", fit.cv$lambda.1se))

# Predice los l amuestra test con el modelo obtenido y
# muestra gráficamente los valores predichos y reales
yhat.fit.cv <- predict(fit.cv, X[-train, ])
plot(y[-train], type="o")
lines(yhat.fit.cv, col="red")
legend("topright",c("Wages", "ajuste LASSO"),col=c("black","red"),lty=1)



