# Práctica 1.9
#######################
# Vamos a utilizar la data.frame "Wage" que se
# encuentra en el package "ISLR". 
# Borra toda la memoria. Carga el package ISLR.
# También usaremos el package "glmnet" para hacer la
# regresión LASSO y el package "dummies" para construir
# variables tipo artificial

# Hacemos un poco de limpieza de datos necesaria
Wage<-subset(Wage, select= -c(region, logwage))
WageD = dummy.data.frame(Wage)

# Define n como el número de columnas de WageD
# Define train como la muestra de entrenamiento 2800 
# observaciones

# Define las variables independientes como X (todas 
# menos la última, que es el salario). Tiene que ser
# una matriz plana (usa as.matrix).
# Define la variable y como el salario (última columna 
# de WageD)

# Ajusta regresión LASSO usando glmnet y muestra los
# resultados gráficamente

# Calcula el valor óptimo de lambda por validación 
# cruzada (cv.glmnet)

# Extrae los parámetros del modelo y muéstralos

# Predice los l amuestra test con el modelo obtenido y
# muestra gráficamente los valores predichos y reales



