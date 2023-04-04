# Loading necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Loading data from Excel
data <- read_excel("C:/Users/Muchendu/Documents/cat 1/gasolineData.xlsx")

# Summary statistics for each variable
summary(data)

# Correlation matrix
cor(data)

# Histograms for each variable
data %>%
  gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 20) +
  facet_wrap(~ key, scales = "free")

# Boxplots for each variable
data %>%
  gather() %>%
  ggplot(aes(key, value)) +
  geom_boxplot()

# Scatterplot matrix for all pairs of variables
pairs(data)
