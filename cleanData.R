install.packages('tidyr');
install.packages('dplyr');
install.packages('ggplot2');
install.packages(c("tidyverse"))
install.packages('cowplot')
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)

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

attach(clean_data)

clean_data$bed <- as.integer(clean_data$bed)
clean_data$bed <- as.integer(clean_data$bed)
clean_data <- select(clean_data, -prev_sold_date)

###################################################


## Histograma distribucion precio

set.seed(123)

price_histogram <- ggplot(clean_data, aes(x = price)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Histogram of Price", x = "Price", y = "Frequency") +
  theme_minimal()+ xlim(0, 10000000)


print(price_histogram)

bed_distribution <- ggplot(clean_data, aes(x = bed)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Histogram of Bedrooms", x = "Bed", y = "Frequency") +
  theme_minimal() + xlim(0, 25)

bath_distribution <- ggplot(clean_data, aes(x = bath)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Histogram of Baths", x = "Bath", y = "Frequency") +
  theme_minimal() + xlim(0, 20)

houseSize_distribution <- ggplot(clean_data, aes(x = house_size)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Histogram of House Size", x = "House Size", y = "Frequency") +
  theme_minimal() + xlim(0, 10000)

combined_plots <- cowplot::plot_grid(price_histogram, bed_distribution, bath_distribution, houseSize_distribution, labels = "AUTO", ncol = 2)
print(combined_plots)

###################################################

## ANALISIS DATA

bedprice <- ggplot(clean_data[,c("price","bed")], aes(x = bed)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot", x = "Number of bedrooms", y = "Price") +
  theme_minimal() + xlim(0, 8)

print(bedprice)

pairs(~ bed + price, data = clean_data, gap = 0.4, cex.labels = 4, xlim = c(0, 100))

m1 <- lm(bed ~ price, data = clean_data)

plot(m1$fitted.values, price, xlab = "Valores ajustados", ylab = "Price", xlim = c(1, 10))
abline(lsfit(m1$fitted.values, price))

###################################################


## MODELO

model_data <- clean_data[,c("price","house_size","bed","bath", "acre_lot")]

model <- lm(price ~ house_size + bed + bath, data = model_data)
plot(model$fitted.values, price, xlab = "Valores ajustados", ylab = "Price", xlim = c(1, 10000000))
abline(lsfit(model$fitted.values, price))

model <- lm(price ~ house_size + bed + bath + acre_lot, data = model_data)
plot(model$fitted.values, price, xlab = "Valores ajustados", ylab = "Price", xlim = c(1, 10000000))
abline(lsfit(model$fitted.values, price))

summary(model)

       