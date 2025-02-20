# Cargar el dataset
data("nottem")

# Inspeccionar la estructura del dataset
print(class(nottem))  # Verificar que es una serie temporal (ts)
print(summary(nottem)) # Estadísticas descriptivas

# Graficar la serie temporal
plot(nottem, main = "Temperaturas Mensuales en Nottingham (1920-1939)",
     xlab = "Año", ylab = "Temperatura", col = "blue", type = "o")
# Descomponer la serie temporal
nottem_decomposed <- decompose(nottem)

# Graficar los componentes
plot(nottem_decomposed, col = "blue")
# Cargar paquete necesario para la prueba de Dickey-Fuller
install.packages("tseries")
library(tseries)

# Graficar ACF y PACF
par(mfrow = c(1,2))  # Para mostrar ambos gráficos juntos
acf(nottem, main = "Autocorrelación (ACF)")
pacf(nottem, main = "Autocorrelación Parcial (PACF)")
par(mfrow = c(1,1))  # Restaurar configuración de gráficos

# Prueba de Dickey-Fuller
adf_test <- adf.test(nottem)
print(adf_test)

# Aplicar diferenciación
nottem_diff <- diff(nottem)

#  serie diferenciada
plot(nottem_diff, main = "Serie Diferenciada de Nottem",
     xlab = "Año", ylab = "Diferencia en Temperatura", col = "red", type = "o")

# Verificar estacionariedad nuevamente
adf_test_diff <- adf.test(nottem_diff)
print(adf_test_diff)


# Boxplot  atípicos
boxplot(nottem, main = "Valores Atípicos en la Serie Nottem",
        ylab = "Temperatura", col = "lightblue", horizontal = TRUE)

# Destacar  atípicos 
outliers <- boxplot.stats(nottem)$out  # Extraer valores atípicos

#  marcar  valores atípicos
plot(nottem, main = "Serie Nottem con Valores Atípicos",
     xlab = "Año", ylab = "Temperatura", col = "blue", type = "o")
points(time(nottem)[nottem %in% outliers], outliers, col = "red", pch = 19)








