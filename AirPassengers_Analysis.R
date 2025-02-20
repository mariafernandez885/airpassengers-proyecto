install.packages("tseries")


data("AirPassengers")

print(class(AirPassengers))   
print(summary(AirPassengers)) 
print(start(AirPassengers))   
print(end(AirPassengers))   
print(frequency(AirPassengers)) 

# Grafico
plot(AirPassengers, main="Número de pasajeros aéreos (1949-1960)", col="blue", lwd=2, ylab="Pasajeros", xlab="Año")

# estadístics descriptivos
media <- mean(AirPassengers)
desviacion <- sd(AirPassengers)

cat("Media:", media, "\n")
cat("Desviación estándar:", desviacion, "\n")

# Descomposición de la serie temporal
descomposicion <- decompose(AirPassengers)
plot(descomposicion)

# librería para adf.test
library(tseries)

# Grafica autocorrelación
acf(AirPassengers, main="Autocorrelación de la serie AirPassengers")
pacf(AirPassengers, main="Autocorrelación parcial de la serie AirPassengers")

# Prueba de Dickey-Fuller
adf_resultado <- adf.test(AirPassengers)
print(adf_resultado)


# Diferenciación
AirPassengers_diff <- diff(AirPassengers)
plot(AirPassengers_diff, main="Serie diferenciada", col="red", lwd=2)

# Repetimos Dickey-Fuller
adf.test(AirPassengers_diff)

# Boxplot para detectar valores atípicos
boxplot(AirPassengers, main="Detección de valores atípicos")

# Destacar valores atípicos en la serie
outliers <- boxplot.stats(AirPassengers)$out
outliers


# Marcar valores atípicos en el gráfico original
plot(AirPassengers, main="Número de pasajeros con valores atípicos", col="blue", lwd=2)
points(time(AirPassengers)[AirPassengers %in% outliers], outliers, col="red", pch=19)



