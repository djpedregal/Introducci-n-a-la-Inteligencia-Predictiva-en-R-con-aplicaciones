# Práctica #1.3
# Copia aquí la última función de la Práctica #1.2
# que generaba números aleatorios. 
# Añade un segundo argumento de entrada que indique
# el tipo de distribución que quiera el usuario.
# Este argumento de entrada se llamará dist y será
# una cadena de caracteres que tomará dos valores, 
# "unif" para distribuciones uniformes o "norm" para
# una distribución Gaussiana.
generar<- function(n= 3000){
  a<- rnorm(n)
  print(paste("Media: ", mean(a)))
  print(paste("Varianza: ", var(a)))
  hist(a)
  return(a)
}

# Pruébala generando vectores aleatorios de dimensión
# 100.000 de las dos distribuciones

# Re-escribe la función "generar" eliminando todas 
# las salidas por pantalla

##############################
# Experimento de Montecarlo:
##############################
# Calcula N= 10.000 medias y varianzas de muestras 
# aleatorias de longitud 100.000 de una distribución
# uniforme. Almacena las medias en el vector "media"
# y las varianzas en el vector "varianza"

# TEOREMA CENTRAL DEL LÍMITE: Muestra que la
# distribución empírica de la media muestral es normal.
# Muestra además la distribución de la varianza muestral.

########################
## EJERCICIO AVANZADO ##
########################
# Escribe una función que calcule el factorial de un número


# Utiliza la función para calcular el factorial 
# de los números que quieras.

# ¿Serías capaz de escribir la misma función llamándose a sí
# misma, teniendo en cuenta que n!= n*(n-1)!

