# Load necessary libraries
library(caret)
library(readxl)
library(moments)

# Load data from excel
data <- read_excel("C:/Users/Muchendu/Documents/cat 1/gasolineData.xlsx")

# Set seed for reproducibility
set.seed(123)

# Split data into estimation (70%) and prediction (30%) sets
trainIndex <- createDataPartition(data$y, p = 0.7, list = FALSE)
estimationSet <- data[trainIndex, ]
predictionSet <- data[-trainIndex, ]

# View the dimensions of estimation and prediction sets
dim(estimationSet)
dim(predictionSet)

# Summary statistics for each variable
# summary(data)
# Calculate the skewness of each variable
# Calculate skewness and kurtosis
skewness(data$x1)
kurtosis(data$x2)

# Fit a model involving x1 and x6 to the estimation data
model <- lm(y ~ x1 + x6, data = estimationSet)

# Check the coefficients and fitted values
# summary(model)

# Extract predictor variables from prediction set
x1_pred <- predictionSet$x1
x6_pred <- predictionSet$x6

# Use model to predict response variable
y_pred <- predict(model, newdata = predictionSet)

# Print predicted values
y_pred

# Calculate performance metrics
rmse <- sqrt(mean((y_pred - predictionSet$y)^2))
mae <- mean(abs(y_pred - predictionSet$y))
rsq <- cor(y_pred, predictionSet$y)^2

# Print performance metrics
print(paste0("RMSE: ", round(rmse, 2)))
print(paste0("MAE: ", round(mae, 2)))
print(paste0("R-squared: ", round(rsq, 2)))
