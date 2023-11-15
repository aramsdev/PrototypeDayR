install.packages('tidyr');
library(tidyr)
library(dplyr)

data <- read.csv("data.csv");
head(data);
dim(data)
class(data)

sum(is.na(data))

mean(data$price)

typeof(data$price)
data <- unique(data);

clean_data <- na.omit(data)
dim(clean_data)




