# Sanctions-Forecast
Project related to Sanctions Forecast 

# Cargar librerías necesarias
library(tidyverse)
library(lubridate)
library(ggplot2)
library(GGally)
library(forecast)
library(factoextra)

# Cargar los datos
file_path <- "Sanctioned_List_By_Countries.csv"
data <- read.csv(file_path, stringsAsFactors = FALSE)

# Vista previa de los datos
head(data)

# Convertir las columnas de fecha a formato Date
data$first_seen <- as.Date(data$first_seen)
data$last_seen <- as.Date(data$last_seen)

# Limpieza de datos
# Eliminar filas con países vacíos o con etiquetas faltantes
data <- data %>% filter(!is.na(countries) & countries != "")

# Crear un subconjunto numérico para la matriz de correlación
numeric_data <- data %>% 
  select_if(is.numeric) %>% 
  na.omit()

# Matriz de correlación
if (ncol(numeric_data) > 1) {
  correlation_matrix <- cor(numeric_data)
  print("Matriz de correlación:")
  print(correlation_matrix)
  
  # Visualizar la matriz de correlación
  ggcorr(correlation_matrix, label = TRUE, label_size = 3, 
         method = "circle", legend.title = "Correlación")
} else {
  print("No hay suficientes columnas numéricas para calcular la matriz de correlación.")
}

# Análisis de los países más afectados
countries_count <- data %>% 
  separate_rows(countries, sep = ";") %>% 
  filter(countries != "") %>%  # Filtrar etiquetas vacías después de separar
  count(countries, sort = TRUE)

# Imprimir los 10 países más afectados
print("Top 10 países más afectados:")
print(countries_count %>% top_n(10, n))

# Visualizar los países más afectados 
countries_count %>% 
  top_n(10, n) %>% 
  ggplot(aes(x = reorder(countries, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n), hjust = -0.3, color = "black", size = 3) +  # Agregar etiquetas
  coord_flip() +
  labs(title = "Top 10 Países Más Afectados por Sanciones", 
       x = "País", 
       y = "Cantidad de Sanciones") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  # Centrar el título

# Análisis de sectores ("schema")
sectors_count <- data %>% 
  count(schema, sort = TRUE)

# Visualizar los sectores más afectados
sectors_count %>% 
  top_n(10, n) %>% 
  ggplot(aes(x = reorder(schema, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Top 10 Sectores Más Afectados por Sanciones", x = "Sector", y = "Cantidad de Sanciones")

# Tendencias temporales
sanctions_time <- data %>% 
  group_by(year = year(first_seen)) %>% 
  summarise(count = n())

# Visualizar las tendencias temporales
sanctions_time %>% 
  ggplot(aes(x = year, y = count)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(color = "purple", size = 2) +
  labs(title = "Tendencias Temporales de Sanciones", x = "Año", y = "Cantidad de Sanciones") +
  theme_minimal()

# Identificar y visualizar outliers con un boxplot
# Crear un boxplot para analizar posibles outliers en la cantidad de sanciones por país (con escala logarítmica)
countries_count %>% 
  ggplot(aes(x = "", y = n)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.size = 2, notch = TRUE) +
  scale_y_log10() +
  labs(title = "Distribución de Sanciones por País (Escala Logarítmica)", x = "", y = "Cantidad de Sanciones (Log)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(color = "gray", size = 0.2))

# Distribución de sanciones por país con un histograma 
countries_count %>% 
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_text(stat = 'bin', binwidth = 50, aes(label = ..count..), vjust = -0.5, color = "black", size = 3) + # Agregar etiquetas
  scale_x_continuous(breaks = seq(0, max(countries_count$n), by = 500)) +
  labs(title = "Distribución de Cantidad de Sanciones por País", 
       x = "Cantidad de Sanciones (Rango)", 
       y = "Número de Países") +
  theme_minimal()

# Análisis de correlación entre fechas y cantidad de sanciones
sanctions_by_date <- data %>% 
  group_by(first_seen) %>% 
  summarise(count = n())

sanctions_by_date %>% 
  ggplot(aes(x = first_seen, y = count, color = count)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_color_viridis_c() +
  labs(title = "Evolución Diaria de Sanciones", x = "Fecha", y = "Cantidad de Sanciones") +
  theme_minimal()

# Modelo ARIMA para tendencias temporales
ts_data <- ts(sanctions_by_date$count, start = min(as.numeric(format(sanctions_by_date$first_seen, "%Y"))), frequency = 365)
arima_model <- auto.arima(ts_data)

# Predicciones futuras
forecasted <- forecast(arima_model, h = 365)
autoplot(forecasted) +
  labs(title = "Predicción de Sanciones a Futuro", x = "Días", y = "Cantidad de Sanciones")

# Análisis Clustering

# Preparar datos para clustering
clustering_data <- data %>% 
  separate_rows(countries, sep = ";") %>% 
  group_by(countries, schema) %>% 
  summarise(total_sanctions = n(), .groups = "drop") %>% 
  na.omit()

# Escalar los datos para K-means
scaled_data <- clustering_data %>% 
  select(-countries, -schema) %>% 
  scale()

# Determinar el número óptimo de clusters
fviz_nbclust(scaled_data, kmeans, method = "wss") +
  labs(title = "Determinación del Número Óptimo de Clusters")

# Aplicar K-means con k = 3 (por ejemplo)
set.seed(123)
kmeans_model <- kmeans(scaled_data, centers = 3, nstart = 25)

# Agregar resultados de clustering a los datos originales
clustering_data$cluster <- as.factor(kmeans_model$cluster)

# Visualizar resultados de clustering
ggplot(clustering_data, aes(x = countries, y = total_sanctions, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "Clustering de Países por Sanciones", x = "País", y = "Cantidad Total de Sanciones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
