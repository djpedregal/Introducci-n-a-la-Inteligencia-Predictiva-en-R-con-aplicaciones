# Práctica 2.3
# Genera un vector de ruido blanco de 3000 observaciones
e<- rnorm(3000)
tsdisplay(e)

# Calcula el test de Ljung-Box para ese ruido y
# 26 retardos (usa la función "Box.test")
Box.test(e, 26, "Ljung-Box")

# Calcula el test de Ljung-Box para ese ruido
# para retardos de 1 a 38
for (lag in 1:38){
  box<- Box.test(e, lag, "Ljung-Box")
  print(box)
}

# Carga en memoria la serie ipi y analiza la 
# autocorrelación con las ACF y PACF y el test de
# Ljung-Box
ipi<- ts(scan("ipi.dat"), start = 1975, frequency = 12)
tsdisplay(ipi)
Box.test(ipi, 26, "Ljung-Box")

# Analiza el pib español
pib<- ts(scan("pib.dat"), start = 1995, frequency = 4)
tsdisplay(pib)
Box.test(pib, 26, "Ljung-Box")

# Analiza la bolsa española
bolsa<- ts(scan("bolsa.dat"), frequency = 1)
tsdisplay(bolsa)
Box.test(bolsa, 26, "Ljung-Box")

# Supongamos que la serie bolsa es un paseo
# aleatorio, examina la serie d(t)=bolsa(t)-bolsa(t-1)
d<- bolsa[2:length(bolsa)]-bolsa[1:(length(bolsa)-1)]
tsdisplay(d)
Box.test(d, 26, "Ljung-Box")

