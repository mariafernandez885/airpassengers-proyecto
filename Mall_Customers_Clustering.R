"C:\Users\maria\Downloads\1735320038738-mall_customers\mall_customers\Mall_Customers.csv"
df <- read.csv("C:\\Users\\maria\\Downloads\\1735320038738-mall_customers\\mall_customers\\Mall_Customers.csv")
# Carga de librerías necesarias
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(tidyr)

# 1. Carga dats
df <- read.csv("C:\\Users\\maria\\Downloads\\1735320038738-mall_customers\\mall_customers\\Mall_Customers.csv")

head(df)

# 2. Explor y limpieza de datos
dim(df)
str(df)
summary(df)

#
colnames(df) <- c("CustomerID", "Genre", "Age", "Annual_Income", "Spending_Score")

# Codific de var categórica
df$Genre <- ifelse(df$Genre == "Male", 1, 0)

# Normaliz de var numéricas
df_scaled <- df
  select(Age, Annual_Income, Spending_Score) %>%
  scale()

# 3. Explora de var
hist(df$Annual_Income, main="Distribución de Ingresos", col="blue")
hist(df$Spending_Score, main="Distribución de Puntuación de Gasto", col="green")

# 4. Entrenamiento de modelos de clustering
# K-Means con método del codo
wcss <- vector()
for (i in 2:10) {
  kmeans_model <- kmeans(df_scaled, centers=i, nstart=10)
  wcss[i] <- kmeans_model$tot.withinss
}

plot(2:10, wcss[2:10], type="b", main="Método del Codo", xlab="Número de clusters", ylab="WCSS")

# Selec de k óptimo y entrenamiento final del modelo
k_optimo <- 5  # segunla gráfica del codo
kmeans_final <- kmeans(df_scaled, centers=k_optimo, nstart=25)
df$Cluster_KMeans <- as.factor(kmeans_final$cluster)

# Clustering Jerárquico
dist_matrix <- dist(df_scaled, method = "euclidean")
hc_model <- hclust(dist_matrix, method = "ward.D")
plot(hc_model, main="Dendrograma de Clustering Jerárquico")
rect.hclust(hc_model, k = k_optimo, border = "red")
df$Cluster_HC <- as.factor(cutree(hc_model, k = k_optimo))

# 5. Evaluacionm de modelos con métrica de silueta
sil_kmeans <- silhouette(kmeans_final$cluster, dist_matrix)
mean(sil_kmeans[,3])  # Promedio de silueta para K-Means

sil_hc <- silhouette(cutree(hc_model, k = k_optimo), dist_matrix)
mean(sil_hc[,3])  # Promedio de silueta para Clustering Jerárquico

# 6. Análisis descriptivo de segmentos
aggregate(df[, c("Age", "Annual_Income", "Spending_Score")], by=list(Cluster_KMeans=df$Cluster_KMeans), mean)
aggregate(df[, c("Age", "Annual_Income", "Spending_Score")], by=list(Cluster_HC=df$Cluster_HC), mean)

# 7.graficos
ggplot(df, aes(x=Annual_Income, y=Spending_Score, color=Cluster_KMeans)) +
  geom_point() +
  ggtitle("Clusters obtenidos con K-Means")

ggplot(df, aes(x=Annual_Income, y=Spending_Score, color=Cluster_HC)) +
  geom_point() +
  ggtitle("Clusters obtenidos con Clustering Jerárquico")

# 8. Guardar el archiv
write.csv(df, "C:\\Users\\maria\\Downloads\\1735320038738-mall_customers\\mall_customers\\Mall_Customers1.csv", row.names=FALSE)


write.csv(df, "C:\\Users\\maria\\Downloads\\1735320038738-mall_customers\\mall_customers\\Mall_Customers1.csv", row.names=FALSE)







