# Práctica 2.7
# Simula un proceso AR(1) con parámetro 0.8 con
# la función "arima.sim".
# Prueba con 30, 300 y 3000 observaciones
tsdisplay(arima.sim(list(ar= 0.8), 30))
tsdisplay(arima.sim(list(ar= 0.8), 300))
tsdisplay(arima.sim(list(ar= 0.8), 3000))

# Simula un proceso AR(2) con parámetro 1.4 y -0.8
tsdisplay(arima.sim(list(ar= c(1.4, -0.8)), 3000))

# Simula un proceso AR(2) con parámetro -1.4 y -0.8
tsdisplay(arima.sim(list(ar= c(-1.4, -0.8)), 3000))

# Simula un proceso MA(2) con parámetro 1.4 y -0.8
tsdisplay(arima.sim(list(ma= c(1.4, -0.8)), 3000))

# Simula un proceso MA(2) con parámetro -1.4 y -0.8
tsdisplay(arima.sim(list(ma= c(-1.4, -0.8)), 3000))

# Simula un proceso ARMA(1, 1) con parámetros
# AR(1)= 0.8 y MA(1)= 0.8
tsdisplay(arima.sim(list(ar= c(0.8), ma= 0.8), 3000))

