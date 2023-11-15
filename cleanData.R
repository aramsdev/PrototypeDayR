install.packages('tidyr');
install.packages('dplyr');
install.packages('ggplot2');
install.packages(c("tidyverse"))
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

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

price_histogram <- ggplot(clean_data$price, aes(x = price)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Histogram of Price", x = "Price", y = "Frequency") +
  theme_minimal()


print(price_histogram)

###################################################


pairs(~ bed + bath + acre_lot + house_size + price + zip_code, data = clean_data, gap = 0.4, cex.labels = 4)

m1 <- lm(price ~ bed + bath)
summary(m1)

plot(m1$fitted.values, price, xlab = "Valores ajustados", ylab = "Price")
abline(lsfit(m1$fitted.values, price))
