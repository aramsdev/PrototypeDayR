library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(randomForest)

###################################################

data <- read.csv("data.csv");
head(data);
dim(data)
class(data)
sum(is.na(data))

###################################################

data <- unique(data);

clean_data <- na.omit(data)
summary(clean_data)
dim(clean_data)
sum(is.na(clean_data))

attach(clean_data)

clean_data$bed <- as.integer(clean_data$bed)

###################################################

## Analisis de desviación Estandar en precios
boxplot(clean_data$price, main = "Boxplot de Precios", ylab = "Precio")
umbral_atipico <- 9999999
df_sin_atipicos <- clean_data[clean_data$price < umbral_atipico, ]

## Histograma distribucion precio

set.seed(123)

price_histogram <- ggplot(clean_data, aes(x = price)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Histograma de Precios", x = "Precio", y = "Frecuencia") +
  theme_minimal()+ xlim(0, 10000000)

bed_distribution <- ggplot(clean_data, aes(x = bed)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Histograms de Cuartos", x = "Cuartos", y = "Frecuencia") +
  theme_minimal() + xlim(0, 25)

bath_distribution <- ggplot(clean_data, aes(x = bath)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Histograma de Baños", x = "Baños", y = "Frecuencia") +
  theme_minimal() + xlim(0, 20)

houseSize_distribution <- ggplot(clean_data, aes(x = house_size)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Histograma de Tamaño de Casas", x = "Tamaño de Casa ", y = "Frecuencia") +
  theme_minimal() + xlim(0, 10000)

combined_plots <- cowplot::plot_grid(price_histogram, bed_distribution, bath_distribution, houseSize_distribution, labels = "AUTO", ncol = 2)
print(combined_plots)

###################################################

## Promedio precio por estado
datos_estados <-  clean_data %>%
  group_by(state) %>%
  summarise(media_precio = mean(price))

datos_estados <-  datos_estados %>%
  arrange(desc(media_precio))

ggplot(datos_estados, aes(x = state, y = media_precio)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Precio Promedio por Estado", x = "Estado", y = "Precio") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Promedio precio por ciudad (Top 10 más caras)

datos_ciudad <- clean_data %>%
  group_by(city, state) %>%
  summarise(median_price = median(price)) %>%
  arrange(desc(median_price)) %>%
  head(10)

datos_ciudad$city_state <- paste(datos_ciudad$city, "-", datos_ciudad$state, sep = "")

ggplot(datos_ciudad, aes(x = median_price, y = city_state)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Mediana de Precio por Ciudad y Estado", x = "Precio Promedio", y = "Ciudad-Estado") +
  theme_minimal()

###################################################

## ANALISIS Correlacion

matrizNumerica <- clean_data;
matrizNumerica <- subset(matrizNumerica, select = -status)
matrizNumerica <- data.matrix(matrizNumerica)
correlacion <- cor(matrizNumerica)
correlacion <- correlacion['price', ]

correlation_order <- order(correlacion)


df_ordered <- data.frame(
  variable = names(correlacion)[correlation_order],
  correlation = correlacion[correlation_order]
)

ggplot(df_ordered, aes(x = variable, y = correlation)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Correlación con el Precio", x = "Variable", y = "Correlación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

desviacion_estandar <- apply(matrizNumerica, 2, sd)
print(desviacion_estandar)

## Modelo Regresión Lineal

model_data <- clean_data

modelRL <- lm(price ~ house_size + bed + bath, data = model_data)
plot(modelRL$fitted.values, price, xlab = "Precio en base a baños, cuartos y tamaño", ylab = "Precio", xlim = c(1, 10000000))
abline(lsfit(model$fitted.values, price))

summary(modelRL)


## Modelo Random Forest
rf_model <- randomForest(price ~ house_size + bed + bath, data = model_data)
varImpPlot(rf_model, main = "Importancia de las Variables")


## Crear DataFrame con datos existentes 

set.seed(456)
nueva_muestra <- dplyr::sample_n(clean_data, 10)

nueva_muestra <- nueva_muestra[, !(names(nueva_muestra) %in% c('price'))]


predictions <- predict(rf_model, newdata = nueva_muestra)
print(predictions)

rf_model2 <- randomForest(price ~ house_size + bed + bath + acre_lot, data = model_data)
predictions2 <- predict(rf_model2, newdata = nueva_muestra)
print(predictions2)

rf_model3 <- randomForest(price ~ house_size + bed + bath + acre_lot + city, data = model_data)
predictions3 <- predict(rf_model3, newdata = nueva_muestra)
print(predictions3)

rf_model4 <- randomForest(price ~ house_size + bed + bath + acre_lot + city + zip_code, data = model_data)
predictions4 <- predict(rf_model4, newdata = nueva_muestra)
print(predictions4)

rf_model5 <- randomForest(price ~ house_size + bed + bath + acre_lot + city + zip_code + state, data = model_data, ntree = 500, mtry = 3)
predictions5 <- predict(rf_model5, newdata = nueva_muestra)
print(predictions5)


model_data_sin_atipicos <- df_sin_atipicos
rf_model6 <- randomForest(price ~ house_size + bed + bath + acre_lot + city + zip_code + state, data = model_data_sin_atipicos)
predictions6 <- predict(rf_model6, newdata = nueva_muestra)
print(predictions6)

rf_model7 <- randomForest(price ~ house_size + bed + bath + acre_lot + city + zip_code + state, data = model_data_sin_atipicos, ntree = 500, mtry = 3)
predictions7 <- predict(rf_model7, newdata = nueva_muestra)
print(predictions7)
