#  Carga del dataset
# ------------------------
data("AirPassengers")  # Cargar dataset en R

# Ver estructura del dataset
print(class(AirPassengers))   # Verificar si es una serie temporal (ts)
print(summary(AirPassengers)) # Resumen estadístico
print(start(AirPassengers))   # Inicio de la serie
eprint(end(AirPassengers))     # Fin de la serie
print(frequency(AirPassengers)) # Frecuencia: 12 (mensual)

# Exploración inicial
# ------------------------
library(ggplot2)
library(ggfortify)

autoplot(AirPassengers) + 
  ggtitle("Número de pasajeros de aerolíneas (1949-1960)") +
  xlab("Año") +
  ylab("Pasajeros")

# Media y desviación estándar
print(mean(AirPassengers))  # Media
print(sd(AirPassengers))    # Desviación estándar

# Análisis de tendencia y estacionalidad
# ------------------------------------------
decomp <- decompose(AirPassengers, type = "multiplicative")
autoplot(decomp)

# Análisis de estacionariedad
# --------------------------------
library(tseries)

# Gráficos ACF y PACF
acf(AirPassengers)
pacf(AirPassengers)

# Prueba de Dickey-Fuller
adf.test(AirPassengers)

# Si no es estacionaria, diferenciamos la serie
AP_diff <- diff(AirPassengers)
autoplot(AP_diff)
adf.test(AP_diff)  # Revisamos de nuevo

#  Detección de valores atípicos
# ---------------------------------
boxplot(AirPassengers, main="Detección de valores atípicos")

# Destacar valores atípicos en la serie
autoplot(AirPassengers) +
  geom_point(data = data.frame(time = time(AirPassengers),
                               value = AirPassengers),
             aes(x = time, y = value),
             color = "red", size = 2)

# Interpretación de resultados
# --------------------------------
print("La serie muestra una tendencia creciente con un patrón estacional claro. Hay algunos valores atípicos, pero siguen la estacionalidad esperada.")
