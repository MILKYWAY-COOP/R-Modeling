library(leaps)
library(readxl)

#loading data from excel
data <- read_excel("C:/Users/Muchendu/Documents/cat 1/data.xlsx")

#extract predictor variables
educ <- data$Educ
precip <- data$Precip
nonWhite <- data$Nonwhite

#extract dependent variable
mort <- data$Mort

#Simple Linear Regression Model
lm1 <- lm(mort ~ educ)
msres1 <- mean(lm1$residuals^2)

#Multiple Linear Regression Model
lm2 <- lm(mort ~ educ + precip + nonWhite)
msres2 <- mean(lm2$residuals^2)

#Polynomial Regression Model
lm3 <- lm(mort ~ poly(educ, 2))
msres3 <- mean(lm3$residuals^2)

#Interaction Effects Model
lm4 <- lm(mort ~ educ * precip * nonWhite)
msres4 <- mean(lm4$residuals^2)

#Best Subset Model using MSRes Criterion
subsets <- regsubsets(mort ~ educ + precip + nonWhite, data=data, method="exhaustive")
summary(subsets)
best_subset <- subsets$which[which.min(subsets$rss), ]
lm_best <- lm(mort ~ educ + nonWhite, data=data[, c("Educ", "Nonwhite", "Mort")])
msres_best <- mean(lm_best$residuals^2)

#Print MSRes values for all models
cat("MSRes for Simple Linear Regression Model:", msres1, "\n")
cat("MSRes for Multiple Linear Regression Model:", msres2, "\n")
cat("MSRes for Polynomial Regression Model:", msres3, "\n")
cat("MSRes for Interaction Effects Model:", msres4, "\n")
cat("MSRes for Best Subset Model:", msres_best, "\n")
