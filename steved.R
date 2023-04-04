#Loading necessary libraries
library(readxl)
library(ggplot2)
library(glmnet)

#loading data from excel
data <- read_excel("C:/Users/Muchendu/Documents/cat 1/data.xlsx")

# extract predictor variables
educ <- data$Educ
precip <- data$Precip 
nonWhite <- data$Nonwhite

# extract dependent variable
mort <- data$Mort

# fit the linear model
linearModel <- lm(Mort ~ Educ, data = data)

# calculate AIC value
n <- length(mort)
k <- length(coefficients(linearModel))
RSS <- sum(resid(linearModel)^2)
linearModel_AIC <- n*log(RSS/n) + 2*k
linearModel_AIC

# fit multiple linear regression
multipleModel <- lm(Mort ~ educ + precip + nonWhite, data = data)

# calculate AIC value
n <- length(mort)
k <- length(coefficients(multipleModel))
RSS <- sum(resid(multipleModel)^2)
multipleModel_AIC <- n*log(RSS/n) + 2*k
multipleModel_AIC

# fit polynomial regression
polynomialModel <- lm(Mort ~ poly(Educ, 2) 
                      + poly(Precip, 2) 
                      + poly(Nonwhite, 2), 
                      data = data)

# calculate AIC value
n <- length(mort)
k <- length(coefficients(polynomialModel))
RSS <- sum(resid(polynomialModel)^2)
polynomial_AIC <- n*log(RSS/n) + 2*k
polynomial_AIC

# fit the interaction effects model
interactionsModel <- lm(Mort ~ Educ*Precip*Nonwhite, data = data)

# calculate AIC value
n <- length(mort)
k <- length(coefficients(interactionsModel))
RSS <- sum(resid(interactionsModel)^2)
interactionsModel_AIC <- n*log(RSS/n) + 2*k
interactionsModel_AIC
