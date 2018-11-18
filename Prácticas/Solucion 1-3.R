# Práctica #1.3
# Copia aquí la última función de la Práctica #2
# que generaba números aleatorios. 
# Añade un segundo argumento de entrada que indique
# el tipo de distribución que quiera el usuario.
# Este argumento de entrada se llamará dist y será
# una cadena de caracteres que tomará dos valores, 
# "unif" para distribuciones uniformes o "norm" para
# una distribución Gaussiana.
generar<- function(n= 3000, dist= "unif"){
  if (dist== "unif"){
    a<- runif(n)
  } else {
    a<- rnorm(n)
  }
  print(paste("Media: ", mean(a)))
  print(paste("Varianza: ", var(a)))
  hist(a)
  return(a)
}

# Pruébala generando vectores aleatorios de dimensión
# 100.000 de las dos distribuciones
a<- generar(100000, "unif")
a<- generar(100000, "pp")

# Re-escribe la función "generar" eliminando todas 
# las salidas por pantalla
generar<- function(n= 3000, dist= "unif"){
  if (dist== "unif"){
    a<- runif(n)
  } else {
    a<- rnorm(n)
  }
  return(a)
}

##############################
# Experimento de Montecarlo:
##############################
# Calcula 10.000 medias y varianzas de muestras 
# aleatorias de longitud 100.000 de una distribución
# uniforme. Almacena las medias en el vector "media"
# y las varianzas en el vector "varianza"
N<- 10000
media<- rep(0, N)
varianza<- media
for (i in 1:N){
  y<- generar(100000, "unif")
  media[i]<- mean(y)
  varianza[i]<- var(y)
}

# TEOREMA CENTRAL DEL LÍMITE: Muestra que la
# distribución empírica de la media muestral es normal.
# Muestra además la distribución de la varianza muestral.
hist(media)
hist(varianza)

########################
## EJERCICIO AVANZADO ##
########################
# Escribe una función que calcule el factorial de un número
fact<- function(n){
  resultado= 1
  for (i in 2:n)
    resultado= resultado*i
  return(resultado)
}

# Utiliza la función para calcular el factorial 
# de los números que quieras.
fact(5)

# ¿Serías capaz de escribir la misma función llamándose a sí
# misma, teniendo en cuenta que n!= n*(n-1)!
fact<- function(n){
  if (n== 2)
    resultado<- 2
  else 
    resultado<- n*fact(n-1)
  return(resultado)
}

