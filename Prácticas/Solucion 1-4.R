# Práctica #1.4
#######################
# REGRESIÓN SIMPLE
#######################
# Genera una variable aleatoria "x" de 300 observaciones
# procedente de una distribución normal,
x<- rnorm(300)

# Genera un vector de ruido aleatorio "e" de dimensión
# 300 de una distribución N(0, 1)
e<- rnorm(300)

# Genera un vector de parámetros "beta" que sea (10 -2)
beta<- c(10, -2)

# Genera un vector de unos "unos", de dimensión 300
unos<- rep(1, 300)

# Genera una matrix "X" que sea la concatenación horizontal
# del vector de unos y x.
X<- cbind(unos, x)

# Genera una variable "y" que responda al modelo de regresión
# lineal, con todos los elementos anteriores.
y<- X %*% beta + e

# Dibuja una nube de puntos que muestre la relación lineal
# entre las variables
plot(x, y)

#######################
# REGRESIÓN MÚLTIPLE
#######################
# Genera una variable aleatoria "x" de 300 observaciones
# procedente de una distribución normal,
x<- rnorm(300)

# Genera un vector "e" de ruidos aleatorios de dimensión
# 300 de una distribución N(0, 1)
e<- rnorm(300)

# Genera una variable "x1", que sea el seno de x y otra
# variabe "x2", que sea el coseno de x
x1<- sin(x)
x2<- cos(x)

# Genera un vector de parámetros "beta" que sea (10 -2 3)
beta<- c(10, -2, 3)

# Genera un vector de unos "unos", de dimensión 300
unos<- rep(1, 300)

# Genera una matrix "X" que sea la concatenación horizontal
# del vector de unos, "x1" y "x2".
X<- cbind(unos, x1, x2)

# Genera una variable "y" que responda al modelo de regresión
# lineal, con todos los elementos anteriores.
y<- X %*% beta + e

# Dibuja una nube de puntos entre "x1" e "y", y otro con
# "x2" e "y". ¿Dirías que la relación es lineal?
plot(x1, y)
plot(x2, y)

# ¿Cómo conseguirías gráficos que muestre la relación lineal?
plot(x1, y-x2*3)
plot(x2, y+x1*2)


