# Práctica #1.1
# Comprueba la carpeta de trabajo
getwd()

# Mira los archivos que hay en el directorio por defecto
dir()

# Mira las variables que tienes en memoria
ls()

# Borra todas las variables de memoria
rm(list= ls())

# Crea dos matrices aleatorias de dimensión 4 x 2 y 
# asígnalas a las variables a y b (tienes que usar
# "runif" y "matrix")
a<- matrix(runif(8), 4, 2)
b<- matrix(runif(8), 4, 2)

# Calcula la matriz transpuesta de b
t(b)

# Calcula a+b, a-b, a*b'
a+b
a-b
a%*%t(b)

# Genera un ruido aleatorio de longitud 3000, 
# procedente de una distribución uniforme.
a<- runif(3000)

# Calcula la media y la varianza
print(paste("Media: ", mean(a)))
print(paste("Varianza: ", var(a)))

# Modifícala para que la media sea
# exactamente 0 y la varianza exactamente 1. Comprobar 
# la forma de la distribución con un histograma.
a<- (a-mean(a))/sd(a)
print(paste("Media: ", mean(a)))
print(paste("Varianza: ", var(a)))
hist(a)

# Repite el ejercicio anterior para una distribución N(0, 1)
a<- rnorm(3000)
print(paste("Media: ", mean(a)))
print(paste("Varianza: ", var(a)))
a<- (a-mean(a))/sd(a)
print(paste("Media: ", mean(a)))
print(paste("Varianza: ", var(a)))
hist(a)

# Borra la ventana de gráficos
dev.off()

