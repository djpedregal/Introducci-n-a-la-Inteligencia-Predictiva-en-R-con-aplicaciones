# Práctica 2.4
# Carga los datos de ipi y represéntalos
ipi<- ts(scan("ipi.dat"), start = 1975, frequency = 12)
tsdisplay(ipi)

# Calcula el lambda de la trasformación Box-Cox
# con la función "BoxCox.lambda" y la serie 
# transformada con el lambda óptimo 
# con la función "BoxCox"
# Representa la serie transformada.
lambda<- BoxCox.lambda(ipi)
tipi<- BoxCox(ipi, lambda)
tsdisplay(tipi)

# Muestra la serie transformada con:
#   1) Una diferencia regular
#   2) Una diferencia estacional
#   3) Una diferencia regular y una estacional
tsdisplay(diff(tipi))
tsdisplay(diff(tipi, 12))
tsdisplay(diff(diff(tipi), 12))

# Busca la transformación de varianza y
# media adecuadas para los pasajeros de avión
pasajeros<- ts(scan("pasajeros.dat"), start = 1969, frequency = 12)
tsdisplay(pasajeros)
tpasajeros<- BoxCox(pasajeros, BoxCox.lambda(pasajeros))
tsdisplay(tpasajeros)
tsdisplay(diff(tpasajeros))
tsdisplay(diff(tpasajeros, 12))
tsdisplay(diff(diff(tpasajeros), 12))

# Repite el análisis para el pib
pib<- ts(scan("pib.dat"), start = 1995, frequency = 4)
tsdisplay(pib)
tpib<- BoxCox(pib, BoxCox.lambda(pib))
tsdisplay(tpib)
tsdisplay(diff(tpib))
tsdisplay(diff(tpib, 4))
tsdisplay(diff(diff(tpib), 4))
tsdisplay(diff(diff(tpib, diff= 2), 4))


