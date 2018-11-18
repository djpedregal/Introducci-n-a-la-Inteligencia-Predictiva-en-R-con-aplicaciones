# Práctica #1.2
# Escribe una función que se llame "mostrar" para leer 
# una variable de datos del disco (con "scan") y hacer 
# un gráfico (con "plot"). El argumento de entrada debe
# ser el nombre del archivo que contiene los datos. 
# La función debe devolver el vector de datos. 
mostrar<- function(archivo){
  y<- scan(archivo)
  plot(y, type= "l")
  return(y)
}

# Comprueba que la función es correcta con el archivo airpas.dat.
z<- mostrar("airpas.dat")

# Escribe una función que se llame "generar" para 
# repetir la generación de números aleatorios de la 
# Práctica #1, eliminando la necesidad de que tenga
# que tener media cero y varianza 1 exactas. 
# La función debe tener como argumento de entrada la 
# dimensión del vector y debe devolver como argumento 
# de salida el vector aleatorio.
generar<- function(n= 3000){
  a<- rnorm(n)
  print(paste("Media: ", mean(a)))
  print(paste("Varianza: ", var(a)))
  hist(a)
  return(a)
}

# Prueba la función para generar vectores aleatorios de
# dimensión 10, 100, 1000 y 10000
a<- generar(10)
a<- generar(100)
a<- generar(1000)
a<- generar(10000)

